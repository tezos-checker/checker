open OUnit2

let suite =
  "HuxianTests" >::: [
    TestConstants.suite;
    TestChecker.suite;
    TestFixedPoint.suite;
    TestTez.suite;
    TestKit.suite;
    TestAvl.suite;
    TestParameters.suite;
    TestLiquidation.suite;
    TestUniswap.suite;
    TestDelegationAuction.suite;
    TestLiquidationAuction.suite;
  ]

let () =
  run_test_tt_main
    suite
