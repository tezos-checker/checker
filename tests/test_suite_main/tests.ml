open OUnit2

let suite =
  "HuxianTests" >::: [
    TestChecker.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestKit.suite;
    TestBurrow.suite;
    TestParameters.suite;
    TestLiquidation.suite;
    TestCfmm.suite;

    (* Note: tests below take a while to run*)
    TestAvl.suite;
    TestLiquidationAuction.suite;
  ]

let () =
  run_test_tt_main
    suite
