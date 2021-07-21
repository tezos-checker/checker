open OUnit2
open Ratio
open FixedPoint
open TestLib

let suite =
  "FixedPoint tests"
  >::: [
         ( "fixedpoint arithmetic" >:: fun _ ->
           let fp1 = fixedpoint_of_hex_string "5" in
           let fp2 = fixedpoint_of_hex_string "3" in
           let fp5 = fixedpoint_of_hex_string "-28" in
           let two = fixedpoint_of_hex_string "2" in
           assert_fixedpoint_equal
             ~expected:
               (fixedpoint_of_ratio_floor
                  (ratio_of_int (Ligo.int_from_literal "8")))
             ~real:(fixedpoint_add fp1 fp2);
           assert_fixedpoint_equal
             ~expected:
               (fixedpoint_of_ratio_floor
                  (ratio_of_int (Ligo.int_from_literal "2")))
             ~real:(fixedpoint_sub fp1 fp2);
           assert_fixedpoint_equal
             ~expected:
               (fixedpoint_of_ratio_floor
                  (ratio_of_int (Ligo.int_from_literal "4")))
             ~real:(fixedpoint_pow two (Ligo.nat_from_literal "2n"));
           assert_fixedpoint_equal
             ~expected:
               (fixedpoint_of_ratio_floor
                  (ratio_of_int (Ligo.int_from_literal "2")))
             ~real:(fixedpoint_pow two (Ligo.nat_from_literal "1n"));
           assert_fixedpoint_equal ~expected:fixedpoint_one
             ~real:(fixedpoint_pow fp5 (Ligo.nat_from_literal "0n")) );
       ]

let () = run_test_tt_main suite
