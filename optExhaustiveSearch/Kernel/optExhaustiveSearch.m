(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 16-Jan-2018 *)

BeginPackage["optExhaustiveSearch`"]
(* Exported symbols added here with SymbolName::usage *) 

$vesselSpec::usage
$terminalSpec::usage
$production::usage
cashflowTrip::usage
cashflowPlan::usage
valuation::usage
makeProductionInventory::usage
possibleDecisions::usage

Begin["`Private`"]
(* Implementation of the package *)

(*allPonds = Cases[Union[Flatten[Join[EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "BorderingBodiesOfWater"]], 
	EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "Basins"]]], 1]], Entity[___]];*)

calType = "Gregorian";
tz = "Europe/London";

(* TODO: put back seconds ? *)
vDate = DateObject[{2018, 1, 1}, TimeObject[{0, 0, 0}], CalendarType -> calType, TimeZone -> tz];
periodStart = DateObject[{2018, 1, 1}, TimeObject[{0, 0, 0}], CalendarType -> calType, TimeZone -> tz];
periodEnd = DateObject[{2019, 1, 1}, TimeObject[{0, 0, 0}], CalendarType -> calType, TimeZone -> tz];

$vesselSpec =
    <|
      "WSD50" -> <|
        "Speed" -> Quantity[15.0, "NauticalMiles" / "Hours"],
        "Capacity" -> Quantity[20000, ("Meters")^3],
        "Maximum loading rate" -> Quantity[ 1250, ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[1250, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0.12,
        "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[100, "USDollars" / "Days" ]|>,
      "TGE" -> <|
        "Speed" -> Quantity[16.0, "NauticalMiles" / "Hours"],
        "Capacity" -> Quantity[30000, ("Meters")^3],
        "Maximum loading rate" -> Quantity[2000, ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[2500, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0.19,
        "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[150, "USDollars" / "Days" ]|>,
      "Leissner" -> <|
        "Speed" -> Quantity[13.0, "NauticalMiles" / "Hours"],
        "Capacity" -> Quantity[75000, ("Meters")^3],
        "Maximum loading rate" -> Quantity[1000, ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[1000, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0.15,
        "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[200, "USDollars" / "Days" ]|>,
      "Tembek" -> <|
        "Capacity" -> Quantity[216100, ("Meters")^3],
        "Maximum loading rate" -> Quantity[2600 , ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[1300, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0, "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Speed" -> Quantity[16.7, "NauticalMiles" / "Hours"],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[ 250, "USDollars" / "Days" ]|>
    |>;

$terminalSpec =
    <|
      "Golden Pass" -> <|
      	"LatLong" -> GeoPosition[{29.761564, -93.929215}],
        "Currency" -> "USD", "Type" -> "Liquification and Export",
        "Total Terminal Storage Capacity" ->Quantity[350000, ("Meters")^3],
        "Daily Production" -> Quantity[5000, ("Meters")^3/"Days"],
        "Inventory" -> Quantity[100000, ("Meters")^3],
        "Price" -> Quantity[4.1, "USDollars" /("Meters")^3]|>,
      "Gate LNG Terminal Netherland" -> <|
        "LatLong" -> GeoPosition[{51.9718, 4.0755}], "Currency" -> "EUR",
        "Type" -> "Import and Re-gasification",
        "Total Terminal Storage Capacity" ->Quantity[540000, ("Meters")^3],
        "Price" -> Quantity[4.0, "USDollars" / ("Meters")^3]|>,
      "Arun LNG Terminal and Plant Indonesia" -> <|
      	"LatLong" -> GeoPosition[{5.2234, 97.083}],
        "Currency" -> "Rupiah", 
        "Type" -> "Import and Re-gasification",
        "Total Terminal Storage Capacity" -> Quantity[635000, ("Meters")^3],
        "Price" -> Quantity[6.2, "USDollars" / ("Meters")^3] |>,
      "Rudong Jiangsu LNG Terminal China" -> <|
      	"LatLong" -> GeoPosition[{32.5292, 121.428}],
        "Currency" -> "Yuan Renminbi",
        "Type" -> "Import and Re-gasification",
        "Total Terminal Storage Capacity" -> Quantity[680000, ("Meters")^3],
        "Price" -> Quantity[6.5, "USDollars" / ("Meters")^3]|>,
      "PNG LNG Terminal Papua New Guinea" -> <|
      	"LatLong" -> GeoPosition[{-9.33862, 147.01825}],
        "Currency" -> "Kina", 
        "Type" -> "Liquification and Export",
        "Total Terminal Storage Capacity" -> Quantity[320000, ("Meters")^3],
        "Daily Production" -> Quantity[2500, ("Meters")^3/"Days"],
        "Inventory" -> Quantity[320000, ("Meters")^3],
        "Price" -> Quantity[3.9 , "USDollars" / ("Meters")^3]|>
    |>;    
    
$vessel = $vesselSpec   
$production = Select[$terminalSpec, #Type=="Liquification and Export" &];
$market = Select[$terminalSpec, #Type=="Import and Re-gasification" &];



(* Need to update the state of the world *)
(* TODO: add forward curves and modify cashflows appropriately  *)
(*cashflowTrip[v_, p_, Missing[]] := <|"Vessel" -> v, "Production" -> p, "Market" -> m, "cashflows" -> Quantity[-Infinity, "USDollars"]|> *)(* TODO: really consider ? *)
cashflowTrip[v_, Missing[], Missing[], updateQ_, startDay_, endDay_, granularity_] := 
	<|"Vessel" -> v, "Production" -> Missing[], "Market" -> Missing[], "cashflows" -> Quantity[0, "USDollars"]|> (* TODO: really consider ? *)
cashflowTrip[v_, p_, Missing[], updateQ_, startDay_, endDay_, granularity_] :=
    Module[ {lV, lP, toLoadTripTime, loadingVolume, loadingTime, cashflow},
        lV = $vessel[v];
        lP = $production[p];
        (*lM = $market[m];*)
        toLoadTripTime = GeoDistance[{lV["Position"], lP["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
        loadingVolume = Min[lV["Capacity"] - lV["Inventory"], lP["Inventory"]];
        loadingTime = loadingVolume / lV["Maximum loading rate"];
        lV["Position"] = lP["LatLong"];
        lV["Inventory"] = lV["Inventory"] + loadingVolume;
        cashflow = - lV["DailyFixedCost"] (toLoadTripTime + loadingTime);
        
        If[updateQ,  
        	$vessel[v]["Position"] = lP["LatLong"];
        	$vessel[v]["Inventory"] = $vessel[v]["Inventory"] + loadingVolume;
        	$production[p]["Inventory"] = $production[p]["Inventory"] - loadingVolume;
        	$production[p]["Inventory Plan"] = makeProductionInventory[startDay + (toLoadTripTime + loadingTime), endDay, granularity, 
                              $production[p]["Inventory"], $production[p]["Daily Production"], $production[p]["Total Terminal Storage Capacity"]];, 
          0];
        
        <|"Vessel" -> lV, "Production" -> lP, "Market" -> Missing[], "cashflows" -> cashflow|>
    ]
cashflowTrip[v_, Missing[], m_, updateQ_, startDay_, endDay_, granularity_] :=
    Module[ {lV, lP, lM, toDischargeTripTime, dischargeVolume, dischargingTime, cashflow},
        lV = $vessel[v];
   		lP = Missing[];
    	lM = $market[m];
 		
    	toDischargeTripTime = GeoDistance[{lV["Position"], lM["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
    	dischargeVolume = Min[lV["Inventory"] Power[(1.0 - lV["Boil-off rate"]), QuantityMagnitude[toDischargeTripTime]], 
    		lM["Total Terminal Storage Capacity"]];
    	dischargingTime = dischargeVolume / lV["Maximum discharge rate"];
    	
    	lV["Position"] = lM["LatLong"];
    	
    	cashflow = - lV["DailyFixedCost"] (toDischargeTripTime + dischargingTime) + dischargeVolume lM["Price"];
    	
    	If[updateQ,  
        	$vessel[v]["Position"] = lP["LatLong"];
        	$vessel[v]["Inventory"] = $vessel[v]["Inventory"] + loadingVolume;
        	$market[m]["Total Terminal Storage Capacity"] = $market[m]["Total Terminal Storage Capacity"] + dischargeVolume;, 
          0];
    	
   		<|"Vessel" -> lV, "Production" -> lP, "Market" -> lM, "cashflows" -> cashflow|>
    ]  
cashflowTrip[v_, p_, m_, updateQ_, startDay_, endDay_, granularity_] :=
    Module[ {lV, lP, lM, toLoadTripTime, loadingVolume, loadingTime, toDischargeTripTime, dischargeVolume, dischargingTime, cashflow},
    	lV = $vessel[v];
    	lP = $production[p];
    	lM = $market[m];
    	
    	toLoadTripTime = GeoDistance[{lV["Position"], lP["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
    	loadingVolume = Min[lV["Capacity"] - lV["Inventory"], lP["Inventory"]];
    	loadingTime = loadingVolume / lV["Maximum loading rate"];
    	
    	lV["Position"] = lP["LatLong"];
 		lV["Inventory"] = lV["Inventory"] + loadingVolume;
 		
    	toDischargeTripTime = GeoDistance[{lV["Position"], lM["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
    	dischargeVolume = Min[lV["Inventory"] Power[(1.0 - lV["Boil-off rate"]), QuantityMagnitude[toDischargeTripTime]], 
    		lM["Total Terminal Storage Capacity"]];
    	dischargingTime = dischargeVolume / lV["Maximum discharge rate"];
    	
    	lV["Position"] = lM["LatLong"];
    	lV["Inventory"] = lV["Inventory"] - dischargeVolume;
    	
    	cashflow = - lV["DailyFixedCost"] (toLoadTripTime + loadingTime + toDischargeTripTime + dischargingTime) + dischargeVolume lM["Price"];
    	
    	If[updateQ,  
        	$vessel[v]["Position"] = lP["LatLong"];
        	$vessel[v]["Inventory"] = $vessel[v]["Inventory"] + loadingVolume;
        	$production[p]["Inventory"] = $production[p]["Inventory"] - loadingVolume;
        	$production[p]["Inventory Plan"] = makeProductionInventory[startDay + (toLoadTripTime + loadingTime), endDay, granularity, 
                              $production[p]["Inventory"], $production[p]["Daily Production"], $production[p]["Total Terminal Storage Capacity"]];
            $market[m]["Total Terminal Storage Capacity"] = $market[m]["Total Terminal Storage Capacity"] + dischargeVolume;, 
          0];
    	
   		<|"Vessel" -> lV, "Production" -> lP, "Market" -> lM, "cashflows" -> cashflow|>
    ]

cashflowPlan[plan_List, updateQ_, startDay_, endDay_, granularity_] := 
	<|"cashflows" -> Total[cashflowTrip[Sequence @@ #, updateQ, startDay, endDay, granularity]["cashflows"] & /@ plan]|>

makeProductionInventory[dateStart_, dateEnd_, granularity_, initialInventory_, productionRate_, maxInventory_] :=
    Module[ {rate, timeSteps, inventory},
        rate = UnitConvert[productionRate, QuantityUnit[initialInventory] / granularity];
        timeSteps = DateRange[DatePlus[dateStart, granularity], dateEnd, granularity];
        inventory = Clip[initialInventory + Accumulate[Quantity[1, granularity] ConstantArray[rate, Length[timeSteps]]], {0 maxInventory, maxInventory}];
        TimeSeries[AssociationThread[timeSteps, inventory], ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0},
        	 CalendarType -> calType, TimeZone -> tz]
    ]


possibleDecisions[v_, p_, m_] :=
    Module[ {lV, lP, lM, decisions},
        {lV, lP, lM} = With[ {len = Max[Length[#] & /@ {v, p, m}]},
                           PadRight[#, len, Missing[]] & /@ {v, p, m}
                       ];
        decisions = (Sort /@ 
          Flatten[Outer[(Apply[Join, #] & /@ Transpose[{#1, #2}]) &, 
            Flatten[Outer[(Apply[Join, #] & /@ Transpose[{#1, #2}]) &, 
              Permutations[<|"Vessel" -> #|> & /@ lV], 
              Permutations[<|"Production" -> #|> & /@ lP], 1, 1], 1 ], 
            Permutations[<|"Market" -> #|> & /@ lM], 1, 1], 1]) // DeleteDuplicates;
        decisions
    ]


(*updateSOW[startDay_, endDay_, granularity_, v_, Missing[], Missing[]] := Nothing;
updateSOW[startDay_, endDay_, granularity_, v_, p_, Missing[]] :=
    Module[ {lV, lP, toLoadTripTime, loadingVolume},
        lV = $vessel[v];
        lP = $production[p];
        toLoadTripTime = GeoDistance[{lV["Position"], lP["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
        loadingVolume = Min[lV["Capacity"] - lV["Inventory"], lP["Inventory"]];
        $vessel[v]["Position"] = $production[p]["LatLong"];
        $vessel[v]["Inventory"] = $vessel[v]["Inventory"] + loadingVolume;
        $production[p]["Inventory"] = $production[p]["Inventory"] - loadingVolume;
        $production[p]["Inventory Plan"] = makeProductionInventory[startDay, endDay, granularity, 
                              $production[p]["Inventory"], $production[p]["Daily Production"], $production[p]["Total Terminal Storage Capacity"]];
    ]
updateSOW[startDay_, endDay_, granularity_, v_, Missing[], m_] :=
    Module[ {lV, lP, lM, toDischargeTripTime, dischargeVolume},
        lV = $vessel[v];
   		lP = Missing[];
    	lM = $market[m];
 		
    	toDischargeTripTime = GeoDistance[{lV["Position"], lM["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
    	dischargeVolume = Min[lV["Inventory"] Power[(1.0 - lV["Boil-off rate"]), QuantityMagnitude[toDischargeTripTime]], 
    		lM["Total Terminal Storage Capacity"]];
    	
   		$vessel[v]["Position"] = $market[m]["LatLong"];
   		$vessel[v]["Inventory"] = $vessel[v]["Inventory"] - dischargeVolume;
   		$market[m]["Total Terminal Storage Capacity"] = $market[m]["Total Terminal Storage Capacity"] + dischargeVolume;
    ]  
updateSOW[startDay_, endDay_, granularity_, v_, p_, m_] :=
    Module[ {lV, lP, lM, loadingVolume, toDischargeTripTime, dischargeVolume},
    	lV = $vessel[v];
    	lP = $production[p];
    	lM = $market[m];
    	
    	loadingVolume = Min[lV["Capacity"] - lV["Inventory"], lP["Inventory"]];
 		
 		toDischargeTripTime = GeoDistance[{lV["Position"], lM["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
    	dischargeVolume = Min[lV["Inventory"] Power[(1.0 - lV["Boil-off rate"]), QuantityMagnitude[toDischargeTripTime]], 
    		lM["Total Terminal Storage Capacity"]];
   		
   		$vessel[v]["Position"] = $market[m]["LatLong"];
   		$vessel[v]["Inventory"] = $vessel[v]["Inventory"] - dischargeVolume;
   		$production[p]["Inventory"] = $production[p]["Inventory"] - loadingVolume;
        $production[p]["Inventory Plan"] = makeProductionInventory[startDay, endDay, granularity, 
                              $production[p]["Inventory"], $production[p]["Daily Production"], $production[p]["Total Terminal Storage Capacity"]];
   		$market[m]["Total Terminal Storage Capacity"] = $market[m]["Total Terminal Storage Capacity"] + dischargeVolume;
   		
    ]*)


valuation[pStart_, pEnd_, granularity_] :=
    Module[ {timeSteps, decisions, optimalDecision},
        (* pEnd should be inclusive, but see:  
        https://mathematica.stackexchange.com/questions/167731/different-behaviour-of-daterange-between-11-2-and-11-3 *)
        timeSteps = DateRange[DatePlus[pStart, granularity], pEnd, granularity];
        Echo[$vessel, "Vessels"];
        Echo[$production, "Production"];
        Echo["Updating production sites"];
        $production = Module[ {local = #},
                          AppendTo[local, "Inventory Plan" ->  makeProductionInventory[pStart, pEnd, granularity, 
                              local["Inventory"], local["Daily Production"], local["Total Terminal Storage Capacity"]]];
                          local
                      ] & /@ $production;
        Echo[$production, "Production"];
        Echo["Finding optimal decision"];
        decisions = possibleDecisions[Normal[Keys[$vessel]], Normal[Keys[$production]], Normal[Keys[$market]]];
        optimalDecision = First[decisions[[Ordering[cashflowPlan[#, False, pStart, pEnd, granularity] & /@ decisions, -1]]]];
        Echo[optimalDecision, "optimalDecision"];
        Echo["Updating states"];
        cashflowPlan[#, True, pStart, pEnd, granularity] & /@ optimalDecision;
        Echo[$vessel, "Vessels"];
        Echo[$production, "Production"];
        
    ]

(* STATUS: finish casting the production inventory into a TimeSeries, 
   then add it to the valuation *)
(* STATUS: choose the optimal decision update logs *)
(* Market storage capacity: what to do with it ? Can it be useful and how ? *)

End[];

EndPackage[];

