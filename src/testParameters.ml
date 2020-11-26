open OUnit2

(* ************************************************************************* *)
(*                        compute_drift_derivative                           *)
(* ************************************************************************* *)
(*
exp( low ): 201/200 = 1.005 = 1.0147AE147AE147AE147B
exp(-low ): 199/200 = 0.995 = 0.FEB851EB851EB851EB85
exp( high): 21/20   = 1.05  = 1.0CCCCCCCCCCCCCCCCCCD
exp(-high): 19/20   = 0.95  = 0.F3333333333333333333

d_t' =  0                   if exp(-0.005) <  p_t < exp(+0.005)
d_t' = +0.0001 / 86400^2    if exp(+0.005) <= p_t < exp(+0.05)
d_t' = -0.0001 / 86400^2    if exp(-0.005) >= p_t > exp(-0.05)
d_t' = +0.0005 / 86400^2    if exp(+0.05)  <= p_t < +infinity
d_t' = -0.0005 / 86400^2    if exp(-0.05)  >= p_t > -infinity
*)

let test_compute_drift_derivative_no_acceleration =
  "test_compute_drift_derivative_no_acceleration" >:: fun _ ->
    (* exp( 0 ): 1 *)
    let target = FixedPoint.one in
    assert_equal
      ~printer:FixedPoint.show
      FixedPoint.zero
      (Parameters.compute_drift_derivative target);

    (* exp( low ): 201/200 = 1.005 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "1.0147AE147AE147AE" in
    assert_equal
      ~printer:FixedPoint.show
      FixedPoint.zero
      (Parameters.compute_drift_derivative target);

    (* exp(-low ): 199/200 = 0.995 (rounded UP) *)
    let target = FixedPoint.of_hex_string "0.FEB851EB851EB852" in
    assert_equal
      ~printer:FixedPoint.show
      FixedPoint.zero
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_low_positive_acceleration =
  "test_compute_drift_derivative_low_positive_acceleration" >:: fun _ ->
    (* exp( low ): 201/200 = 1.005 (rounded UP) *)
    let target = FixedPoint.of_hex_string "1.0147AE147AE147AF" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "0.000000000003C547")
      (Parameters.compute_drift_derivative target);

    (* exp( high): 21/20   = 1.05 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "1.0CCCCCCCCCCCCCCC" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "0.000000000003C547")
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_low_negative_acceleration =
  "test_compute_drift_derivative_low_negative_acceleration" >:: fun _ ->
    (* exp(-low ): 199/200 = 0.995 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "0.FEB851EB851EB851" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "-0.000000000003C547")
      (Parameters.compute_drift_derivative target);

    (* exp(-high): 19/20   = 0.95 (rounded UP) *)
    let target = FixedPoint.of_hex_string "0.F333333333333334" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "-0.000000000003C547")
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_high_positive_acceleration =
  "test_compute_drift_derivative_high_positive_acceleration" >:: fun _ ->
    (* exp( high): 21/20   = 1.05 (rounded UP) *)
    let target = FixedPoint.of_hex_string "1.0CCCCCCCCCCCCCCD" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "0.000000000012DA63")
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_high_negative_acceleration =
  "test_compute_drift_derivative_high_negative_acceleration" >:: fun _ ->
    (* exp(-high): 19/20   = 0.95 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "0.F333333333333333" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "-0.000000000012DA63")
      (Parameters.compute_drift_derivative target)

(* ************************************************************************* *)
(*                           compute_imbalance                               *)
(* ************************************************************************* *)

(* TODO:
 * 1. Finish the test_compute_imbalance_positive_small and
 *    test_compute_imbalance_positive_capped tests below. I'm not convinced yet
 *    that the current calculation is correct.
 * 2. Ensure that the kit that should be burned (due to imbalance alone) is
 *    actually burned (in checker.touch or parameters.touch).
 * 3. Would be nice to have some property-based random tests about the
 *    following (I've been bitten by this before, due to the difference between
 *    Q.compare and Stdlib.compare):
 *    - The result of compute_imbalance NEVER goes beyond ±0.05
 *    - If burrowed > circulating then (compute_imbalance burrowed circulating) > 0
 *    - If burrowed < circulating then (compute_imbalance burrowed circulating) < 0
 *    - If burrowed1 > burrowed2 > circulating
 *      then (compute_imbalance burrowed1 circulating) >= (compute_imbalance burrowed2 circulating)
 *    - If circulating1 > circulating2 > burrowed
 *      then (compute_imbalance burrowed circulating1) <= (compute_imbalance burrowed circulating2)
*)

let test_compute_imbalance_all_zero =
  "test_compute_imbalance_all_zero" >:: fun _ ->
    let burrowed    = Kit.zero in
    let circulating = Kit.zero in
    assert_equal
      ~printer:(Q.sprint ())
      Q.zero
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_zero_burrowed =
  "test_compute_imbalance_zero_burrowed" >:: fun _ ->
    let burrowed    = Kit.zero in
    let circulating = Kit.one in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-5/100")
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_equal =
  "test_compute_imbalance_equal" >:: fun _ ->
    let burrowed    = Kit.of_mukit 1_000_000_000 in
    let circulating = Kit.of_mukit 1_000_000_000 in
    assert_equal
      ~printer:(Q.sprint ())
      Q.zero
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_small =
  "test_compute_imbalance_positive_small" >:: fun _ ->
    ()
    (* TODO *)

let test_compute_imbalance_positive_capped =
  "test_compute_imbalance_positive_capped" >:: fun _ ->
    ()
    (* TODO *)

let test_compute_imbalance_negative_small =
  "test_compute_imbalance_negative_small" >:: fun _ ->
    let burrowed    = Kit.of_mukit   166_666_667 in
    let circulating = Kit.of_mukit 1_000_000_000 in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-833333333/16666666700") (* JUST BELOW SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_capped =
  "test_compute_imbalance_negative_capped" >:: fun _ ->
    let burrowed    = Kit.of_mukit             1 in
    let circulating = Kit.of_mukit 1_000_000_000 in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-5/100") (* SATURATED *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

(* ************************************************************************* *)
(*                                  touch                                    *)
(* ************************************************************************* *)

let test_touch =
  "test_touch" >:: fun _ ->
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
      ~printer:Kit.show

let suite =
  "Parameters tests" >::: [
    test_compute_drift_derivative_no_acceleration;
    test_compute_drift_derivative_low_positive_acceleration;
    test_compute_drift_derivative_low_negative_acceleration;
    test_compute_drift_derivative_high_positive_acceleration;
    test_compute_drift_derivative_high_negative_acceleration;

    test_compute_imbalance_all_zero;
    test_compute_imbalance_zero_burrowed;
    test_compute_imbalance_equal;
    test_compute_imbalance_positive_small;
    test_compute_imbalance_positive_capped;
    test_compute_imbalance_negative_small;
    test_compute_imbalance_negative_capped;

    test_touch;
  ]
