(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 16-Jan-2018 *)

BeginPackage["optExhaustiveSearch`"]
(* Exported symbols added here with SymbolName::usage *) 

$vesselSpec::usage
$terminalSpec::usage
cashflowTrip::usage
cashflowPlan::usage
valuation::usage
makeProductionInventory::usage

Begin["`Private`"]
(* Implementation of the package *)

(*allPonds = Cases[Union[Flatten[Join[EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "BorderingBodiesOfWater"]], 
	EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "Basins"]]], 1]], Entity[___]];*)

calType = "Julian";
tz = "Europe/London";

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
cashflowTrip[v_, p_, Missing[]] := <|"Vessel" -> v, "Production" -> p, "Market" -> m, "cashflows"->Quantity[-Infinity, "USDollars"]|> (* TODO: really consider ? *)
cashflowTrip[v_, Missing[], Missing[]] := <|"Vessel" -> v, "Production" -> p, "Market" -> m, "cashflows"->Quantity[-Infinity, "USDollars"]|> (* TODO: really consider ? *)
cashflowTrip[v_, Missing[], m_] :=
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
    	
   		<|"Vessel" -> lV, "Production" -> lP, "Market" -> lM, "cashflows"->cashflow|>
    ]  
cashflowTrip[v_, p_, m_] :=
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
    	lV["Inventory"] = lV["Inventory"] - loadingVolume;
    	
    	cashflow = - lV["DailyFixedCost"] (toLoadTripTime + loadingTime + toDischargeTripTime + dischargingTime) + dischargeVolume lM["Price"];
    	
   		<|"Vessel" -> lV, "Production" -> lP, "Market" -> lM, "cashflows"->cashflow|>
    ]

cashflowPlan[plan_List] := <|"cashflows" -> Total[cashflowTrip[Sequence @@ #]["cashflows"] & /@ plan]|>

makeProductionInventory[dateStart_, dateEnd_, granularity_, initialInventory_, productionRate_, maxInventory_] := 
	Module[{rate, timeSteps, inventory}, 
	rate = UnitConvert[productionRate, QuantityUnit[initialInventory] / granularity];
	timeSteps = DateRange[DatePlus[dateStart, granularity], dateEnd, granularity];
	inventory = AssociationThread[timeSteps, initialInventory + Accumulate[Quantity[1, granularity] ConstantArray[rate, Length[timeSteps]]]]
	]

valuation[pStart_, pEnd_, granularity_] := 
	Module[{timeSteps},
	timeSteps = DateRange[DatePlus[pStart, granularity], pEnd, granularity];
	
	]

End[];

EndPackage[];

