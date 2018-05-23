(* Wolfram Language Test file *)


Test[
    UnitConvert[
    Quantity[5000, ("Meters")^3/("Days")],  ("Meters")^3 / "Hour"]
    ,
    UnitConvert[
    Quantity[5000, ("Meters")^3/("Days")], ("Meters")^3/("Hour")]
    ,
    {Quantity::unkunit}
    ,
    TestID->"TestDateOperations-20180307-Z7C6K0"
]

Test[
    UnitConvert[
    Quantity[5000, ("Meters")^3/("Days")],  ("Meters")^3 / "Hours"]
    ,
    Quantity[625/3, ("Meters")^3/("Hours")]
    ,
    TestID->"TestDateOperations-20180307-T8C5B5"
]