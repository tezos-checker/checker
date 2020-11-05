open OUnit2

let suite =
  "HuxianTests" >::: [
    TestFixedPoint.suite;
    TestTez.suite;
    TestAvl.suite;
    TestParameters.suite;
    TestLiquidation.suite;
  ]

let () =
  run_test_tt_main suite
