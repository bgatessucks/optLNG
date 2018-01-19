(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 16-Jan-2018 *)

BeginPackage["optExhaustiveSearch`"]
(* Exported symbols added here with SymbolName::usage *) 

$vessels::usage
$terminals::usage
cashflowTrip::usage

Begin["`Private`"]
(* Implementation of the package *)

(*allPonds = Cases[Union[Flatten[Join[EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "BorderingBodiesOfWater"]], 
	EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "Basins"]]], 1]], Entity[___]];*)

calType = "Julian";
tz = "Europe/London";

vDate = DateObject[{2018, 1, 1}, TimeObject[{0, 0, 0}], CalendarType -> calType, TimeZone -> tz];

$vessels =
    <|
      "WSD50" -> <|"Timestamp"->vDate, 
        "Status"->"Idle",
        "Speed" -> Quantity[15.0, "NauticalMiles" / "Hours"],
        "Capacity" -> Quantity[20000, ("Meters")^3],
        "Maximum loading rate" -> Quantity[ 1250, ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[1250, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0.12,
        "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[100, "USDollars" / "Days" ]|>,
      "TGE" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
        "Speed" -> Quantity[16.0, "NauticalMiles" / "Hours"],
        "Capacity" -> Quantity[30000, ("Meters")^3],
        "Maximum loading rate" -> Quantity[2000, ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[2500, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0.19,
        "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[150, "USDollars" / "Days" ]|>,
      "Leissner" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
        "Speed" -> Quantity[13.0, "NauticalMiles" / "Hours"],
        "Capacity" -> Quantity[75000, ("Meters")^3],
        "Maximum loading rate" -> Quantity[1000, ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[1000, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0.15,
        "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[200, "USDollars" / "Days" ]|>,
      "Tembek" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
        "Capacity" -> Quantity[216100, ("Meters")^3],
        "Maximum loading rate" -> Quantity[2600 , ("Meters")^3 / "Days"],
        "Maximum discharge rate" -> Quantity[1300, ("Meters")^3 / "Days"],
        "Boil-off rate" -> 0, "Position" -> GeoPosition[{58.97`, 5.71`}],
        "Speed" -> Quantity[16.7, "NauticalMiles" / "Hours"],
        "Inventory" -> Quantity[0, ("Meters")^3],
        "DailyFixedCost" -> Quantity[ 250, "USDollars" / "Days" ]|>
    |>;

$terminals =
    <|
      "Golden Pass" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
      	"LatLong" -> GeoPosition[{29.761564, -93.929215}],
        "Currency" -> "USD", "Type" -> "Liquification and Export",
        "Total Terminal Storage Capacity" ->Quantity[350000, ("Meters")^3],
        "Daily Production" -> Quantity[5000, ("Meters")^3/"Days"],
        "Inventory" -> Quantity[100000, ("Meters")^3],
        "Price" -> Quantity[4.1, "USDollars" /("Meters")^3]|>,
      "Gate LNG Terminal Netherland" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
        "LatLong" -> GeoPosition[{51.9718, 4.0755}], "Currency" -> "EUR",
        "Type" -> "Import and Re-gasification",
        "Total Terminal Storage Capacity" ->Quantity[540000, ("Meters")^3],
        "Price" -> Quantity[4.0, "USDollars" / ("Meters")^3]|>,
      "Arun LNG Terminal and Plant Indonesia" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
      	"LatLong" -> GeoPosition[{5.2234, 97.083}],
        "Currency" -> "Rupiah", 
        "Type" -> "Import and Re-gasification",
        "Total Terminal Storage Capacity" -> Quantity[635000, ("Meters")^3],
        "Price" -> Quantity[6.2, "USDollars" / ("Meters")^3] |>,
      "Rudong Jiangsu LNG Terminal China" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
      	"LatLong" -> GeoPosition[{32.5292, 121.428}],
        "Currency" -> "Yuan Renminbi",
        "Type" -> "Import and Re-gasification",
        "Total Terminal Storage Capacity" -> Quantity[680000, ("Meters")^3],
        "Price" -> Quantity[6.5, "USDollars" / ("Meters")^3]|>,
      "PNG LNG Terminal Papua New Guinea" -> <|"Timestamp"->vDate, 
      	"Status"->"Idle",
      	"LatLong" -> GeoPosition[{-9.33862, 147.01825}],
        "Currency" -> "Kina", 
        "Type" -> "Liquification and Export",
        "Total Terminal Storage Capacity" -> Quantity[320000, ("Meters")^3],
        "Daily Production" -> Quantity[2500, ("Meters")^3/"Days"],
        "Inventory" -> Quantity[320000, ("Meters")^3],
        "Price" -> Quantity[3.9 , "USDollars" / ("Meters")^3]|>
    |>;    
    
$production = Select[$terminals, #Type=="Liquification and Export" &];
$market = Select[$terminals, #Type=="Import and Re-gasification" &];

(* Need to update the state of the world *)
cashflowTrip[v_Association, Missing[], Missing[]] := -Infinity (* TODO: really consider ? *)
cashflowTrip[v_Association, Missing[], m_Association] :=
    Module[ {lV, lP, lM, loadingVolume, toDischargeTripTime, dischargeVolume, dischargingTime, cashflow},
        lV = v;
   		lP = $production;
    	lM = m;
 		
    	toDischargeTripTime = GeoDistance[{lV["Position"], lM["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[lV["Speed"], "NauticalMiles"/"Days"];
    	dischargeVolume = Min[lV["Inventory"] Power[(1.0 - lV["Boil-off rate"]), QuantityMagnitude[toDischargeTripTime]], 
    		lM["Total Terminal Storage Capacity"]];
    	dischargingTime = dischargeVolume / lV["Maximum discharge rate"];
    	
    	lV["Position"] = lM["LatLong"];
    	lV["Inventory"] = lV["Inventory"] - loadingVolume;
    	
    	cashflow = - lV["DailyFixedCost"] (toDischargeTripTime + dischargingTime) + dischargeVolume lM["Price"];
    	
   		{lV, lP, lM, cashflow}
    ]  
cashflowTrip[v_Association, p_Association, m_Association] :=
    Module[ {lV, lP, lM, toLoadTripTime, loadingVolume, loadingTime, toDischargeTripTime, dischargeVolume, dischargingTime, cashflow},
    	lV = v;
    	lP = p;
    	lM = m;
    	
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
    	
   		{lV, lP, lM, cashflow}
        
    ]


End[]

EndPackage[]

