open OUnit2

type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "TezTests" >::: [
    "tez arithmetic" >::
    (fun _ ->
       let tz1 = Tez.of_mutez 5_000_000 in
       let tz2 = Tez.of_mutez 3_000_000 in
       let tz3 = Tez.of_mutez 5_123_400 in
       let tz4 = Tez.of_mutez 5_001_000_000 in
       let tz5 = Tez.of_mutez 40_000_000 in
       let fp1 = FixedPoint.of_string "3.0" in
       assert_equal ~printer:show_tz (Tez.of_mutez 8_000_000) Tez.(tz1 + tz2);
       assert_equal ~printer:show_tz (Tez.of_mutez 2_000_000) Tez.(tz1 - tz2);
       (* TODO: negative numbers? *)
       assert_equal ~printer:show_fp (FixedPoint.of_string "8.0")     Tez.(tz5 / tz1);
       assert_equal ~printer:show_fp (FixedPoint.of_string "1.7078")  Tez.(tz3 / tz2);
       assert_equal ~printer:show_fp (FixedPoint.of_string "125.025") Tez.(tz4 / tz5);
       assert_equal ~printer:show_tz (Tez.of_mutez 15_370_200) (Tez.scale tz3 fp1);
       assert_equal ~printer:show_tz tz1 (Tez.max tz1 tz2)
    )
  ]
