open OUnit2
open TestLib
(* open Ratio *)
open FixedPoint
open Kit
(* open Parameters *)
open ContinuousDriftDerivative

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let sort_three_kit_amounts_increasing_order kit1 kit2 kit3 =
  match List.stable_sort kit_compare [kit1;kit2;kit3;] with
  | [kit1;kit2;kit3;] -> (kit1, kit2, kit3)
  | _ -> failwith "impossible"

(* ************************************************************************* *)
(*                        compute_drift_derivative                           *)
(* ************************************************************************* *)
(*
exp( high): 21/20   = 1.05  = 1.0CCCCCCCCCCCCCCCCCCD
exp(-high): 19/20   = 0.95  = 0.F3333333333333333333
*)

let test_compute_drift_derivative_no_acceleration =
  "test_compute_drift_derivative_no_acceleration" >:: fun _ ->
    (* d'(1) = 0 *)
    let target = fixedpoint_of_hex_string "1.0000000000000000" in
    assert_fixedpoint_equal
      ~expected:fixedpoint_zero
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_barely_non_positive_acceleration =
  "test_compute_drift_derivative_barely_non_positive_acceleration" >:: fun _ ->
    (* d'(1+ε) = 0 *)
    let target = fixedpoint_of_hex_string "1.0002F2AC4F06D266" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.0000000000000000")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_barely_positive_acceleration =
  "test_compute_drift_derivative_barely_positive_acceleration" >:: fun _ ->
    (* d'(1+ε) > 0 *)
    let target = fixedpoint_of_hex_string "1.0002F2AC4F06D267" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.0000000000000001")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_barely_non_negative_acceleration =
  "test_compute_drift_derivative_barely_non_negative_acceleration" >:: fun _ ->
    (* d'(1-ε) = 0 *)
    let target = fixedpoint_of_hex_string "0.FFFD0D53B0F92D9A" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.0000000000000000")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_barely_negative_acceleration =
  "test_compute_drift_derivative_barely_negative_acceleration" >:: fun _ ->
    (* d'(1-ε) < 0 *)
    let target = fixedpoint_of_hex_string "0.FFFD0D53B0F92D99" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.0000000000000001")
      ~real:(compute_drift_derivative target)


(* FIXME: Add property-based, random tests:
   - strictly monotonic in [-cutoff, +cutoff]
   - flat everywhere else (with known values)
*)

let test_compute_drift_derivative_high_positive_acceleration_barely_non_saturated =
  "test_compute_drift_derivative_high_positive_acceleration_barely_non_saturated" >:: fun _ ->
    (* exp( high): 21/20   = 1.05 (rounded DOWN) *)
    let target = fixedpoint_of_hex_string "1.0CCCCCCCCCCCCCCC" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.000000000012DA62")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_high_positive_acceleration_barely_saturated =
  "test_compute_drift_derivative_high_positive_acceleration_barely_saturated" >:: fun _ ->
    (* exp( high): 21/20   = 1.05 (rounded UP) *)
    let target = fixedpoint_of_hex_string "1.0CCCCCCCCCCCCCCD" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.000000000012DA63")
      ~real:(compute_drift_derivative target)

(* FIXME: add a test past positive saturation. *)

let test_compute_drift_derivative_high_negative_acceleration_barely_non_saturated =
  "test_compute_drift_derivative_high_negative_acceleration_barely_non_saturated" >:: fun _ ->
    (* exp(-high): 19/20   = 0.95 (rounded UP) *)
    let target = fixedpoint_of_hex_string "0.F333333333333334" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.000000000012DA62")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_high_negative_acceleration_barely_saturated =
  "test_compute_drift_derivative_high_negative_acceleration_barely_saturated" >:: fun _ ->
    (* exp(-high): 19/20   = 0.95 (rounded DOWN) *)
    let target = fixedpoint_of_hex_string "0.F333333333333333" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.000000000012DA63")
      ~real:(compute_drift_derivative target)

(* FIXME: add a test past negative saturation. *)

let suite =
  "ContinuousDriftDerivative tests" >::: [
    (* At target = 1 *)
    test_compute_drift_derivative_no_acceleration;

    (* Around the first non-zero drift derivative (positive) *)
    test_compute_drift_derivative_barely_non_positive_acceleration;
    test_compute_drift_derivative_barely_positive_acceleration;

    (* Around the first non-zero drift derivative (negative) *)
    test_compute_drift_derivative_barely_non_negative_acceleration;
    test_compute_drift_derivative_barely_negative_acceleration;

    (* Around saturation (positive limit) *)
    test_compute_drift_derivative_high_positive_acceleration_barely_non_saturated;
    test_compute_drift_derivative_high_positive_acceleration_barely_saturated;

    (* Around saturation (negative limit) *)
    test_compute_drift_derivative_high_negative_acceleration_barely_non_saturated;
    test_compute_drift_derivative_high_negative_acceleration_barely_saturated;
  ]

let () =
  run_test_tt_main
    suite
