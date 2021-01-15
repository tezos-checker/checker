open OUnit2

type fp = FixedPoint.t [@@deriving show]

let suite =
  "FixedPoint tests" >::: [
    "fixedpoint arithmetic" >::
    (fun _ ->
       let fp1 = FixedPoint.of_hex_string "5" in
       let fp2 = FixedPoint.of_hex_string "3" in
       let fp3 = FixedPoint.of_hex_string "5.1F972474538EF34D" in
       let fp4 = FixedPoint.of_hex_string "1389" in
       let fp5 = FixedPoint.of_hex_string "-28" in
       let fp6 = FixedPoint.of_ratio_floor (Ratio.make (Ligo.int_from_literal 1) (Ligo.int_from_literal 10)) in
       let two = FixedPoint.of_hex_string "2" in
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.make (Ligo.int_from_literal 11) (Ligo.int_from_literal 10)))
         (FixedPoint.exp fp6);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.of_int 8))
         (FixedPoint.add fp1 fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.of_int 2))
         (FixedPoint.sub fp1 fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.of_int 15))
         (FixedPoint.mul fp1 fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_hex_string "F.5EC56D5CFAACD9E7")
         (FixedPoint.mul fp3 fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_hex_string "-CC.EF9DB22D0E560408") (* a little lossy, should have ended in 0418 *)
         (FixedPoint.mul fp3 fp5);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.make (Ligo.int_from_literal 5) (Ligo.int_from_literal 3)))
         (FixedPoint.div fp1 fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.of_int (-8)))
         (FixedPoint.div fp5 fp1);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.make (Ligo.int_from_literal 17078) (Ligo.int_from_literal 10000)))
         (FixedPoint.div fp3 fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_hex_string "-7D.0666666666666666")
         (FixedPoint.div fp4 fp5);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.of_int 4))
         (FixedPoint.pow two (Ligo.int_from_literal 2));
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_ratio_floor (Ratio.of_int 2))
         (FixedPoint.pow two (Ligo.int_from_literal 1));
       assert_equal
         ~printer:show_fp
         (FixedPoint.one)
         (FixedPoint.pow fp5 (Ligo.int_from_literal 0));
    );
  ]
