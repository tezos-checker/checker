open OUnit2

let suite =
  "Parameters tests" >::: [
    "test_touch" >::
    fun _ ->
      let initial_parameters : Parameters.t =
        { q = FixedPoint.of_hex_string "0.E666666666666666"; (* 0.9 *)
          index = Tez.of_mutez 360_000;
          target = FixedPoint.of_hex_string "1.147AE147AE147AE1"; (* 1.08 *)
          protected_index = Tez.of_mutez 350_000;
          drift = FixedPoint.zero;
          drift' = FixedPoint.zero;
          burrow_fee_index = FixedPoint.one;
          imbalance_index = FixedPoint.one;
          outstanding_kit = Kit.one; (* TODO: What should that be? *)
          circulating_kit = Kit.zero; (* TODO: What should that be? *)
          last_touched = Timestamp.of_seconds 0;
        } in
      let tezos = Tezos.{
          now = Timestamp.of_seconds 3600;
          level = Level.of_int 60;
          self = Address.of_string "checker";
        } in

      let new_index = Tez.of_mutez 340_000 in
      let kit_in_tez = Q.of_string "305/1000" in
      let total_accrual_to_uniswap, new_parameters = Parameters.touch tezos new_index kit_in_tez initial_parameters in
      assert_equal
        { q = FixedPoint.of_hex_string "0.E6666895A3EC8BA5"; (* 0.90000013020828555983 *)
          index = Tez.of_mutez 340_000;
          protected_index = Tez.of_mutez 340_000;
          target = FixedPoint.of_hex_string "1.00D6E1B366FF4BEE"; (* 1.00327883367481013224 *)
          drift' = FixedPoint.of_hex_string "0.000000000012DA63"; (* 0.00000000000006697957 *)
          drift  = FixedPoint.of_hex_string "0.00000000848F8818"; (* 0.00000000012056322737 *)
          burrow_fee_index = FixedPoint.of_hex_string "1.00000991D674CC29"; (* 1.00000057039729312258 *)
          imbalance_index = FixedPoint.of_hex_string "1.00001323ACE99852"; (* 1.00000114079458624517 *)
          outstanding_kit = Kit.of_mukit 1_000_001;
          circulating_kit = Kit.of_mukit 0_000_000; (* NOTE that it ends up being identical to the one we started with *)
          last_touched = tezos.now;
        }
        new_parameters
        ~printer:Parameters.show;
      assert_equal
        Kit.zero (* NOTE: I'd expect this to be higher I think. *)
        total_accrual_to_uniswap
        ~printer:Kit.show;
  ]
