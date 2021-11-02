open OUnit2
open TestLib
open FixedPoint
open ContinuousDriftDerivative

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

(* ************************************************************************* *)
(*                  compute_drift_derivative (unit tests)                    *)
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

let test_compute_drift_derivative_high_positive_acceleration_oversaturated =
  "test_compute_drift_derivative_high_positive_acceleration_oversaturated" >:: fun _ ->
    let target = fixedpoint_of_hex_string "1.1234567788ABCD9F" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.000000000012DA63")
      ~real:(compute_drift_derivative target)

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

let test_compute_drift_derivative_high_negative_acceleration_oversaturated =
  "test_compute_drift_derivative_high_negative_acceleration_oversaturated" >:: fun _ ->
    let target = fixedpoint_of_hex_string "0.ABCDEF1243475869" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.000000000012DA63")
      ~real:(compute_drift_derivative target)

(* ************************************************************************* *)
(*              compute_drift_derivative (property-based tests)              *)
(* ************************************************************************* *)

(* The drift derivative cannot go above 0.05 cNp/day^2 *)
let test_compute_drift_derivative_upper_bound =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_compute_drift_derivative_upper_bound"
    ~count:property_test_count
    TestArbitrary.arb_target
  @@ fun target ->
  Ligo.leq_int_int
    (fixedpoint_to_raw (compute_drift_derivative target))
    (fixedpoint_to_raw (fixedpoint_of_hex_string "0.000000000012DA63"))

(* The drift derivative cannot go below -0.05 cNp/day^2 *)
let test_compute_drift_derivative_lower_bound =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_compute_drift_derivative_lower_bound"
    ~count:property_test_count
    TestArbitrary.arb_target
  @@ fun target ->
  Ligo.geq_int_int
    (fixedpoint_to_raw (compute_drift_derivative target))
    (fixedpoint_to_raw (fixedpoint_of_hex_string "-0.000000000012DA63"))

(* The drift derivative calculation is monotonic (not strictly) *)
let test_compute_drift_derivative_monotonic =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_compute_drift_derivative_monotonic"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_target TestArbitrary.arb_target)
  @@ fun (t1, t2) ->
  let t1, t2 = if t1 <= t2 then (t1, t2) else (t2, t1) in
  Ligo.leq_int_int
    (fixedpoint_to_raw (compute_drift_derivative t1))
    (fixedpoint_to_raw (compute_drift_derivative t2))

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
    test_compute_drift_derivative_high_positive_acceleration_oversaturated;

    (* Around saturation (negative limit) *)
    test_compute_drift_derivative_high_negative_acceleration_barely_non_saturated;
    test_compute_drift_derivative_high_negative_acceleration_barely_saturated;
    test_compute_drift_derivative_high_negative_acceleration_oversaturated;

    (* Property-based random tests. *)
    test_compute_drift_derivative_upper_bound;
    test_compute_drift_derivative_lower_bound;
    test_compute_drift_derivative_monotonic;
  ]

let () =
  run_test_tt_main
    suite
