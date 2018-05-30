(* Wolfram Language Test file *)

(* Wolfram Language Test file *)

dir = "Test";
files = {"TestDateOperations.mt", "TestUnitConversions.mt", "TestCashflows.mt"};
TestSuite[FileNameJoin[{dir, #}] & /@ files];