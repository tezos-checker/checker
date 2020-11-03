open FixedPoint
open Huxian
open OUnit2
open Parameters
open Kit
open Tez

type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "HuxianTests" >::: [
    TestFixedPoint.suite;
    TestTez.suite;
    TestAvl.suite;

    "test_step" >::
    fun _ ->
      (* skip_if true "Float comparisons"; *)
      let initial_parameters = { q = FixedPoint.of_string "0.9";
                                 index = Tez.of_float 0.36;
                                 target = FixedPoint.of_string "1.08";
                                 protected_index = Tez.of_float 0.35;
                                 drift = FixedPoint.of_string "0.0";
                                 drift' = FixedPoint.of_string "0.0";
                                 burrow_fee_index = FixedPoint.of_string "1.0";
                                 imbalance_index = FixedPoint.of_string "1.0";
                                 outstanding_kit = Kit.one; (* TODO: What should that be? *)
                                 circulating_kit = Kit.zero; (* TODO: What should that be? *)
                               } in
      let interblock_time = Seconds 3600 in
      let new_index = FixedPoint.of_string "0.34" in
      let tez_per_kit = FixedPoint.of_string "0.305" in
      let _total_accrual_to_uniswap, new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
      assert_equal
        { q = FixedPoint.of_string "0.900000";
          index = Tez.of_float 0.34;
          protected_index = Tez.of_float 0.339999;
          target = FixedPoint.of_string "1.00327868";
          drift' = FixedPoint.of_string "0.0" (* was: 6.69795953361e-14 *);
          drift = FixedPoint.of_string "0.0" (* was: 1.20563271605e-10 *);
          burrow_fee_index = FixedPoint.of_string "1.005";
          imbalance_index = FixedPoint.of_string "1.001";
          outstanding_kit = Kit.of_float 1.006005;
          circulating_kit = Kit.of_float 0.005;
        }
        new_parameters
        ~printer:show_parameters
  ]


let () =
  run_test_tt_main suite
