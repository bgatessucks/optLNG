(* Wolfram Language Test file *)


Test[
    DateRange[
    DateObject[{2018, 1, 1, 0, 0, 0.`}, "Instant", "Gregorian", 0.`], 
    DateObject[{2018, 1, 1, 4, 0, 0.`}, "Instant", "Gregorian", 
    0.`], "Hour"]
    ,
    {DateObject[{2018, 1, 1, 0, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 1, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 2, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 3, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 4, 0, 0.}, "Instant", "Gregorian", 0.]}
    ,
    TestID->"TestDateOperations-20180307-H4F1Y0"
]

    
Test[
    DateRange[
    DateObject[{2018, 1, 1, 0, 0, 0.`}, "Instant", "Gregorian", 0.`], 
    DateObject[{2018, 1, 1, 4, 0, 0.`}, "Instant", "Gregorian", 
    0.`], "Hours"]
    ,
    {DateObject[{2018, 1, 1, 0, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 1, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 2, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 3, 0, 0.}, "Instant", "Gregorian", 0.], 
    DateObject[{2018, 1, 1, 4, 0, 0.}, "Instant", "Gregorian", 0.]}
    ,
    TestID->"TestDateOperations-20180307-E1E8L0"
]

Test[
    DatePlus[DateObject[{2018, 1, 2, 3, 0, 0.`}, "Instant", "Gregorian", 
    0.`], "Hour"]
    ,
    DateObject[{2018, 1, 2, 4, 0, 0.}, "Instant", "Gregorian", 0.]
    ,
    TestID->"TestDateOperations-20180307-I2K5K2"
]
  
 Test[
     DatePlus[DateObject[{2018, 1, 2, 3, 0, 0.`}, "Instant", "Gregorian", 
     0.`], "Hours"]
     ,
     DateObject[{2018, 1, 2, 4, 0, 0.}, "Instant", "Gregorian", 0.]
     ,
     TestID->"TestDateOperations-20180307-K6P1E8"
 ]