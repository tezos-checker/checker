open Huxian
open OUnit2
open Tez

let suite =
  "HuxianTests" >::: [
      "test_step" >::
        fun _ -> let initial_parameters = { q = 0.9;
                                            index = Tez.of_float 0.36;
                                            target = 1.08;
                                            protected_index = Tez.of_float 0.35;
                                            drift = 0.0;
                                            drift' = 0.0;
                                          } in
                 let interblock_time = Seconds 3600 in
                 let new_index = 0.34 in
                 let tez_per_kit = 0.305 in
                 let new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
                 assert_equal 0.900000130208 new_parameters.q ~printer:string_of_float;
                 assert_equal 0.36 (Tez.to_float new_parameters.index) ~printer:string_of_float;
                 assert_equal 1.08 new_parameters.target ~printer:string_of_float;
                 assert_equal 0.35 (Tez.to_float new_parameters.protected_index) ~printer:string_of_float;
                 assert_equal 0.0 new_parameters.drift ~printer:string_of_float;
                 assert_equal 0.0 new_parameters.drift ~printer:string_of_float;
  ]


let () =
  run_test_tt_main suite
