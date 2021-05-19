open OUnit2

let suite =
  "CheckerTests" >::: [
    TestChecker.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestKit.suite;
    TestAvl.suite;
    TestBurrow.suite;
    TestParameters.suite;
    TestLiquidation.suite;
    TestCfmm.suite;
    TestLiquidationAuction.suite;
    TestAvlModel.suite;
    TestSliceList.suite;
  ]

let () =
  run_test_tt_main
    suite
