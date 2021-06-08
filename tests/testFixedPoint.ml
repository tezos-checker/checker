open OUnit2
open Ratio
open FixedPoint

let suite =
  "FixedPoint tests" >::: [
    "fixedpoint arithmetic" >::
    (fun _ ->
       let fp1 = fixedpoint_of_hex_string "5" in
       let fp2 = fixedpoint_of_hex_string "3" in
       let fp5 = fixedpoint_of_hex_string "-28" in
       let two = fixedpoint_of_hex_string "2" in
       assert_equal
         ~printer:show_fixedpoint
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "8")))
         (fixedpoint_add fp1 fp2);
       assert_equal
         ~printer:show_fixedpoint
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "2")))
         (fixedpoint_sub fp1 fp2);
       assert_equal
         ~printer:show_fixedpoint
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "4")))
         (fixedpoint_pow two (Ligo.nat_from_literal "2n"));
       assert_equal
         ~printer:show_fixedpoint
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "2")))
         (fixedpoint_pow two (Ligo.nat_from_literal "1n"));
       assert_equal
         ~printer:show_fixedpoint
         (fixedpoint_one)
         (fixedpoint_pow fp5 (Ligo.nat_from_literal "0n"));
    );
  ]
