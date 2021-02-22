open OUnit2
open Ratio
open FixedPoint

type fp = fixedpoint[@@deriving show]

let suite =
  "FixedPoint tests" >::: [
    "fixedpoint arithmetic" >::
    (fun _ ->
       let fp1 = fixedpoint_of_hex_string "5" in
       let fp2 = fixedpoint_of_hex_string "3" in
       let fp3 = fixedpoint_of_hex_string "5.1F972474538EF34D" in
       let fp4 = fixedpoint_of_hex_string "1389" in
       let fp5 = fixedpoint_of_hex_string "-28" in
       let two = fixedpoint_of_hex_string "2" in
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "8")))
         (fixedpoint_add fp1 fp2);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "2")))
         (fixedpoint_sub fp1 fp2);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "15")))
         (fixedpoint_mul fp1 fp2);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_hex_string "F.5EC56D5CFAACD9E7")
         (fixedpoint_mul fp3 fp2);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_hex_string "-CC.EF9DB22D0E560408") (* a little lossy, should have ended in 0418 *)
         (fixedpoint_mul fp3 fp5);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "3")))
         (fixedpoint_div fp1 fp2);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "-8")))
         (fixedpoint_div fp5 fp1);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "17078") (Ligo.int_from_literal "10000")))
         (fixedpoint_div fp3 fp2);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_hex_string "-7D.0666666666666666")
         (fixedpoint_div fp4 fp5);
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "4")))
         (fixedpoint_pow two (Ligo.nat_from_literal "2n"));
       assert_equal
         ~printer:show_fp
         (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "2")))
         (fixedpoint_pow two (Ligo.nat_from_literal "1n"));
       assert_equal
         ~printer:show_fp
         (fixedpoint_one)
         (fixedpoint_pow fp5 (Ligo.nat_from_literal "0n"));
    );
  ]
