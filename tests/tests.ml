open OUnit2

let suite =
  "CheckerTests" >::: [
    (* fast *)
    TestChecker.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestKit.suite;
    TestLqt.suite;
    TestBurrow.suite;
    TestParameters.suite;
    TestCfmm.suite;
    TestFa2Interface.suite;
    TestLiquidation.suite;
    (* slow *)
    TestAvl.suite;
    TestLiquidationAuction.suite;
    TestAvlModel.suite;
  ]

let () =
  run_test_tt_main
    suite
