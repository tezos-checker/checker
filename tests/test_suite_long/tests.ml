open OUnit2

let suite =
  "HuxianLongRunningTests" >::: [
    TestAvlModel.suite;
  ]

let () =
  run_test_tt_main
    suite
