open Parameters
open OUnit2
open FixedPoint
open Tez
open Kit
open Timestamp

let suite =
  "Parameters tests" >::: [
    "test_step" >::
    fun _ ->
      let initial_parameters : Parameters.t =
        { q = FixedPoint.of_string "0.9";
          index = Tez.of_float 0.36;
          target = FixedPoint.of_string "1.08";
          protected_index = Tez.of_float 0.35;
          drift = FixedPoint.of_string "0.0";
          drift' = FixedPoint.of_string "0.0";
          burrow_fee_index = FixedPoint.of_string "1.0";
          imbalance_index = FixedPoint.of_string "1.0";
          outstanding_kit = Kit.one; (* TODO: What should that be? *)
          circulating_kit = Kit.zero; (* TODO: What should that be? *)
          last_touched = Timestamp.of_seconds 0;
        } in
      let current_time = Timestamp.of_seconds 3600 in
      let new_index = FixedPoint.of_string "0.34" in
      let tez_per_kit = FixedPoint.of_string "0.305" in
      let _total_accrual_to_uniswap, new_parameters = Parameters.step current_time new_index tez_per_kit initial_parameters in
      assert_equal
        { q = FixedPoint.of_string "0.900000";
          index = Tez.of_float 0.34;
          protected_index = Tez.of_float 0.339999;
          target = FixedPoint.of_string "1.00327868";
          drift' = FixedPoint.of_string "0.0";
          drift = FixedPoint.of_string "0.0";
          burrow_fee_index = FixedPoint.of_string "1.00000057";
          imbalance_index = FixedPoint.of_string "1.00000011";
          outstanding_kit = Kit.of_float 1.000000; (* NOTE that it ends up being identical to the one we started with *)
          circulating_kit = Kit.of_float 0.000000; (* NOTE that it ends up being identical to the one we started with *)
          last_touched = current_time;
        }
        new_parameters
        ~printer:Parameters.show
  ]
