open OUnit2

type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "TezTests" >::: [
    "tez arithmetic" >::
    (fun _ ->
       assert_equal ~printer:show_tz
         (Tez.of_mutez (Ligo.int_from_literal 8_000_000))
         (Tez.add (Tez.of_mutez (Ligo.int_from_literal 5_000_000)) (Tez.of_mutez (Ligo.int_from_literal 3_000_000)));
       assert_equal ~printer:show_tz
         (Tez.of_mutez (Ligo.int_from_literal 2_000_000))
         (Tez.sub (Tez.of_mutez (Ligo.int_from_literal 5_000_000)) (Tez.of_mutez (Ligo.int_from_literal 3_000_000)));
       assert_equal
         ~printer:show_tz
         (Tez.of_mutez (Ligo.int_from_literal 5_000_000))
         (max (Tez.of_mutez (Ligo.int_from_literal 5_000_000)) (Tez.of_mutez (Ligo.int_from_literal 3_000_000)));
       assert_equal
         ~printer:(fun x -> x)
         "50309951mutez"
         (show_tz (Tez.of_mutez (Ligo.int_from_literal 50_309_951)));
    )
  ]
