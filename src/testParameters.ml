open OUnit2

let suite =
  "Parameters tests" >::: [
    "test_step" >::
    fun _ ->
      let initial_parameters : Parameters.t =
        { q = FixedPoint.of_string "0.9";
          index = Tez.of_mutez 360_000;
          target = FixedPoint.of_string "1.08";
          protected_index = Tez.of_mutez 350_000;
          drift = FixedPoint.of_string "0.0";
          drift' = FixedPoint.of_string "0.0";
          burrow_fee_index = FixedPoint.of_string "1.0";
          imbalance_index = FixedPoint.of_string "1.0";
          outstanding_kit = Kit.one; (* TODO: What should that be? *)
          circulating_kit = Kit.zero; (* TODO: What should that be? *)
          last_touched = Timestamp.of_seconds 0;
        } in
      let tezos = Tezos.{
        now = Timestamp.of_seconds 3600;
        level = Level.of_int 60;
      } in

      let new_index = FixedPoint.of_string "0.34" in
      let tez_per_kit = FixedPoint.of_string "0.305" in
      let total_accrual_to_uniswap, new_parameters = Parameters.step tezos new_index tez_per_kit initial_parameters in
      assert_equal
        { q = FixedPoint.of_string "0.900000";
          index = Tez.of_mutez 340_000;
          protected_index = Tez.of_mutez 339_999;
          target = FixedPoint.of_string "1.00327868";
          drift' = FixedPoint.of_string "0.0";
          drift = FixedPoint.of_string "0.0";
          burrow_fee_index = FixedPoint.of_string "1.00000057";
          imbalance_index = FixedPoint.of_string "1.00000114";
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
