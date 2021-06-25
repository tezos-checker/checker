open OUnit2

let suite =
  "CheckerTests" >::: [
    (* fast *)
    TestChecker.suite;
    TestCommon.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestCtez.suite;
    TestKit.suite;
    TestLqt.suite;
    TestBurrow.suite;
    TestMem.suite;
    TestParameters.suite;
    TestCfmm.suite;
    TestFa2Interface.suite;
    TestLiquidation.suite;
    TestCheckerMain.suite;
    (* slow *)
    TestAvl.suite;
    TestLiquidationAuction.suite;
    TestAvlModel.suite;
    TestSliceList.suite;
  ]

let () =
  run_test_tt_main
    suite
