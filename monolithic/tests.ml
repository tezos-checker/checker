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
    Avl.suite;

    "tez arithmetic" >::
    (fun _ ->
       let tz1 = Tez.of_float 5.0 in
       let tz2 = Tez.of_float 3.0 in
       let tz3 = Tez.of_float 5.1234 in
       let tz4 = Tez.of_float 5001.0 in
       let tz5 = Tez.of_float 40.0 in
       let fp1 = FixedPoint.of_float 3.0 in
       assert_equal ~printer:show_tz (Tez.of_float 8.0) (Tez.add tz1 tz2);
       assert_equal ~printer:show_tz (Tez.of_float 2.0)(Tez.sub tz1 tz2);
       assert_equal ~printer:show_tz (Tez.of_float 15.0) (Tez.mul tz1 tz2);
       assert_equal ~printer:show_tz (Tez.of_float 15.3702) (Tez.mul tz3 tz2);
       (* TODO: negative numbers? *)
       assert_equal ~printer:show_fp (FixedPoint.of_float 8.0) (Tez.div tz5 tz1);
       assert_equal ~printer:show_fp (FixedPoint.of_float 1.7078) (Tez.div tz3 tz2);
       assert_equal ~printer:show_fp (FixedPoint.of_float 125.025) (Tez.div tz4 tz5);
       assert_equal ~printer:show_tz (Tez.of_float 2.0) (Tez.rem tz1 tz2);
       assert_equal ~printer:show_tz (Tez.of_float 2.1234) (Tez.rem tz3 tz2);
       assert_equal ~printer:show_tz (Tez.of_float 15.3702) (Tez.scale tz3 fp1)
    );

    "fixedpoint arithmetic" >::
    (fun _ ->
       let fp1 = FixedPoint.of_float 5.0 in
       let fp2 = FixedPoint.of_float 3.0 in
       let fp3 = FixedPoint.of_float 5.1234 in
       let fp4 = FixedPoint.of_float 5001.0 in
       let fp5 = FixedPoint.of_float (-40.0) in
       let fp6 = FixedPoint.of_float (0.1) in
       assert_equal ~printer:show_fp (FixedPoint.of_float 1.1) (FixedPoint.exp fp6);
       assert_equal ~printer:show_fp (FixedPoint.of_float 8.0) FixedPoint.(fp1 + fp2);
       assert_equal ~printer:show_fp (FixedPoint.of_float 2.0) FixedPoint.(fp1 - fp2);
       assert_equal ~printer:show_fp (FixedPoint.of_float 15.0) FixedPoint.(fp1 * fp2);
       assert_equal ~printer:show_fp (FixedPoint.of_float 15.3702) FixedPoint.(fp3 * fp2);
       assert_equal ~printer:show_fp (FixedPoint.of_float (-204.936)) FixedPoint.(fp3 * fp5);
       (* assert_equal ~printer:show_fp (FixedPoint.of_float 1.6666666) FixedPoint.(fp1 / fp2); *)
       assert_equal ~printer:show_fp (FixedPoint.of_float (-8.0)) FixedPoint.(fp5 / fp1);
       assert_equal ~printer:show_fp (FixedPoint.of_float 1.7078) FixedPoint.(fp3 / fp2);
       assert_equal ~printer:show_fp (FixedPoint.of_float (-125.025)) FixedPoint.(fp4 / fp5)
    );

    "test_step" >::
    fun _ ->
      skip_if true "Float comparisons";
      let initial_parameters = { q = FixedPoint.of_float 0.9;
                                 index = Tez.of_float 0.36;
                                 target = FixedPoint.of_float 1.08;
                                 protected_index = Tez.of_float 0.35;
                                 drift = FixedPoint.of_float 0.0;
                                 drift' = FixedPoint.of_float 0.0;
                                 burrow_fee_index = FixedPoint.of_float 1.0;
                                 imbalance_index = FixedPoint.of_float 1.0;
                                 global_last_minted_kit = Kit.one; (* TODO: What should that be? *)
                               } in
      let interblock_time = Seconds 3600 in
      let new_index = 0.34 in
      let tez_per_kit = 0.305 in
      let _total_accrual_to_uniswap, new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
      assert_equal
        { q = FixedPoint.of_float 0.900000;
          index = Tez.of_float 0.34;
          protected_index = Tez.of_float 0.34;
          target = FixedPoint.of_float 1.00327868;
          drift' = FixedPoint.of_float 6.69795953361e-14;
          drift = FixedPoint.of_float 1.20563271605e-10;
          burrow_fee_index = FixedPoint.of_float 1.0;    (* TODO: use expected value. *)
          imbalance_index = FixedPoint.of_float 1.0;     (* TODO: use expected value. *)
          global_last_minted_kit = Kit.one;              (* TODO: use expected value. *)
        }
        new_parameters
        ~printer:show_parameters
  ]


let () =
  run_test_tt_main suite
