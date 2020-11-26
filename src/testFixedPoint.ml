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
       let fp6 = FixedPoint.of_q_floor (Q.of_string "1/10") in
       let two = FixedPoint.of_hex_string "2" in
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "11/10"))
         (FixedPoint.exp fp6);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "8"))
         FixedPoint.(fp1 + fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "2"))
         FixedPoint.(fp1 - fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "15"))
         FixedPoint.(fp1 * fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_hex_string "F.5EC56D5CFAACD9E7")
         FixedPoint.(fp3 * fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_hex_string "-CC.EF9DB22D0E560408") (* a little lossy, should have ended in 0418 *)
         FixedPoint.(fp3 * fp5);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "5/3"))
         FixedPoint.(fp1 / fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "-8"))
         FixedPoint.(fp5 / fp1);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "17078/10000"))
         FixedPoint.(fp3 / fp2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_hex_string "-7D.0666666666666666")
         FixedPoint.(fp4 / fp5);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "4"))
         (FixedPoint.pow two 2);
       assert_equal
         ~printer:show_fp
         (FixedPoint.of_q_floor (Q.of_string "2"))
         (FixedPoint.pow two 1);
       assert_equal
         ~printer:show_fp
         (FixedPoint.one)
         (FixedPoint.pow fp5 0);
    );
  ]
