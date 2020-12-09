open OUnit2

type kt = Kit.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "KitTests" >::: [
    "kit arithmetic" >::
    (fun _ ->
       (* add *)
       assert_equal ~printer:show_kt
         (Kit.of_mukit (Z.of_int 8_000_000))
         Kit.(of_mukit (Z.of_int 5_000_000) + of_mukit (Z.of_int 3_000_000));
       assert_equal ~printer:show_kt
         (Kit.of_mukit (Z.of_int 2_000_000))
         Kit.(of_mukit (Z.of_int 5_000_000) + of_mukit (Z.of_int (-3_000_000)));

       (* subtract *)
       assert_equal ~printer:show_kt
         (Kit.of_mukit (Z.of_int 2_000_000))
         Kit.(of_mukit (Z.of_int 5_000_000) - of_mukit (Z.of_int 3_000_000));
       assert_equal ~printer:show_kt
         (Kit.of_mukit (Z.of_int 8_000_000))
         Kit.(of_mukit (Z.of_int 5_000_000) - of_mukit (Z.of_int (-3_000_000)));

       (* scale *)
       assert_equal
         ~printer:show_kt
         (Kit.of_mukit (Z.of_int 15_370_401))
         (Kit.scale (Kit.of_mukit (Z.of_int 5_123_467)) (FixedPoint.of_q_floor (Q.of_string "3")));
       assert_equal
         ~printer:show_kt
         (Kit.of_mukit (Z.of_int (-15_370_401)))
         (Kit.scale (Kit.of_mukit (Z.of_int 5_123_467)) (FixedPoint.of_q_floor (Q.of_string "-3")));
       assert_equal
         ~printer:show_kt
         (Kit.of_mukit (Z.of_int (-15_370_401)))
         (Kit.scale (Kit.of_mukit (Z.of_int (-5_123_467))) (FixedPoint.of_q_floor (Q.of_string "3")));
       assert_equal
         ~printer:show_kt
         (Kit.of_mukit (Z.of_int 15_370_401))
         (Kit.scale (Kit.of_mukit (Z.of_int (-5_123_467))) (FixedPoint.of_q_floor (Q.of_string "-3")));

       (* compare *)
       assert_equal
         ~printer:show_kt
         (Kit.of_mukit (Z.of_int 5_000_000))
         (max (Kit.of_mukit (Z.of_int 5_000_000)) (Kit.of_mukit (Z.of_int 3_000_000)));
       assert_equal
         ~printer:show_kt
         (Kit.of_mukit (Z.of_int (-3_000_000)))
         (max (Kit.of_mukit (Z.of_int (-5_000_000))) (Kit.of_mukit (Z.of_int (-3_000_000))));

       (* show *)
       assert_equal
         ~printer:(fun x -> x)
         "-50.309951"
         (show_kt (Kit.of_mukit (Z.of_int (-50_309_951))));
       assert_equal
         ~printer:(fun x -> x)
         "50.309951"
         (show_kt (Kit.of_mukit (Z.of_int 50_309_951)));
    )
  ]
