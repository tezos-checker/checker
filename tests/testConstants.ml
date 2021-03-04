open OUnit2
open Ratio
open FixedPoint
open Constants

(* ************************************************************************* *)
(*                                CONSTANTS                                  *)
(* ************************************************************************* *)
let test_correct_cnp_0_01 =
  "test_correct_cnp_0_01" >:: fun _ ->
    assert_equal
      ~printer:show_fixedpoint
      (fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "1") (Ligo.int_from_literal "10000")))
      cnp_0_01

let test_correct_cnp_0_05 =
  "test_correct_cnp_0_05" >:: fun _ ->
    assert_equal
      ~printer:show_fixedpoint
      (fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "10000")))
      cnp_0_05

let test_correct_fixedpoint_seconds_in_a_day =
  "test_correct_fixedpoint_seconds_in_a_day" >:: fun _ ->
    assert_equal
      ~printer:show_fixedpoint
      (fixedpoint_of_int seconds_in_a_day)
      fixedpoint_seconds_in_a_day

let suite =
  "Constant tests" >::: [
    test_correct_cnp_0_01;
    test_correct_cnp_0_05;
    test_correct_fixedpoint_seconds_in_a_day;
  ]
