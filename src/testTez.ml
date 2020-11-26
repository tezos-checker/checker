open OUnit2

type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "TezTests" >::: [
    "tez arithmetic" >::
    (fun _ ->
       (* add *)
       assert_equal ~printer:show_tz
         (Tez.of_mutez 8_000_000)
         Tez.(of_mutez 5_000_000 + of_mutez 3_000_000);
       assert_equal ~printer:show_tz
         (Tez.of_mutez 2_000_000)
         Tez.(of_mutez 5_000_000 + of_mutez (-3_000_000));

       (* subtract *)
       assert_equal ~printer:show_tz
         (Tez.of_mutez 2_000_000)
         Tez.(of_mutez 5_000_000 - of_mutez 3_000_000);
       assert_equal ~printer:show_tz
         (Tez.of_mutez 8_000_000)
         Tez.(of_mutez 5_000_000 - of_mutez (-3_000_000));

       (* scale *)
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez 15_370_401)
         (Tez.scale (Tez.of_mutez 5_123_467) (FixedPoint.of_q_floor (Q.of_string "3")));
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez (-15_370_401))
         (Tez.scale (Tez.of_mutez 5_123_467) (FixedPoint.of_q_floor (Q.of_string "-3")));
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez (-15_370_401))
         (Tez.scale (Tez.of_mutez (-5_123_467)) (FixedPoint.of_q_floor (Q.of_string "3")));
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez 15_370_401)
         (Tez.scale (Tez.of_mutez (-5_123_467)) (FixedPoint.of_q_floor (Q.of_string "-3")));

       (* compare *)
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez 5_000_000)
         (max (Tez.of_mutez 5_000_000) (Tez.of_mutez 3_000_000));
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez (-3_000_000))
         (max (Tez.of_mutez (-5_000_000)) (Tez.of_mutez (-3_000_000)));

       (* show *)
       assert_equal
         ~printer:(fun x -> x)
         "-50.309951"
         (show_tz (Tez.of_mutez (-50_309_951)));
       assert_equal
         ~printer:(fun x -> x)
         "50.309951"
         (show_tz (Tez.of_mutez 50_309_951));
    )
  ]
