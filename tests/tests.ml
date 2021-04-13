open OUnit2

let suite =
  "HuxianTests" >::: [
    TestChecker.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestKit.suite;
    TestAvl.suite;
    TestBurrow.suite;
    TestParameters.suite;
    TestLiquidation.suite;
    TestCfmm.suite;
    TestDelegationAuction.suite;
    TestLiquidationAuction.suite;
  ]

let () =
  run_test_tt_main
    suite
