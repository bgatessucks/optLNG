(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 16-Jan-2018 *)

BeginPackage["optExhaustiveSearch`"]
(* Exported symbols added here with SymbolName::usage *) 

cashflowTrip::usage

Begin["`Private`"]
(* Implementation of the package *)

(*allPonds = Cases[Union[Flatten[Join[EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "BorderingBodiesOfWater"]], 
	EntityClass["Ocean", "SevenSeas"][EntityProperty["Ocean", "Basins"]]], 1]], Entity[___]];*)

vessel[params_Association, t_, status_Association] := 1 

(* Need to update the state of the world *)
cashflowTrip[v_Association, Missing[], Missing[]] := -Infinity (* TODO: really consider ? *)
cashflowTrip[v_Association, Missing[], m_Association] :=
    Module[ {tripTime, dischargeVolume, cashflow},
        tripTime = GeoDistance[{v["Position"], m["LatLong"]}, UnitSystem -> "NauticalMiles"] / UnitConvert[v["Speed"], "NauticalMiles"/"Days"];
        dischargeVolume = v["Inventory"] Power[(1.0 - v["Boil-off rate"]), QuantityMagnitude[tripTime]];
        cashflow = -v["DailyFixedCost"] (tripTime + dischargeVolume / v["Maximum discharge rate"]) + dischargeVolume m["Price"];
        cashflow
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

