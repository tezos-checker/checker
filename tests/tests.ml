open OUnit2

let suite =
  "HuxianTests" >::: [
    TestChecker.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestKit.suite;
    TestParameters.suite;
    TestLiquidation.suite;
    TestUniswap.suite;
    TestDelegationAuction.suite;
    TestLiquidationAuction.suite;
  ]

let () =
  run_test_tt_main
    suite
