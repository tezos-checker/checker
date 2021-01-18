open OUnit2

type tz = Ligo.tez [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "TezTests" >::: [
    "tez arithmetic" >::
    (fun _ ->
       assert_equal ~printer:show_tz
         (Ligo.tez_from_mutez_literal 8_000_000)
         (Ligo.add_tez_tez (Ligo.tez_from_mutez_literal 5_000_000) (Ligo.tez_from_mutez_literal 3_000_000));
       assert_equal ~printer:show_tz
         (Ligo.tez_from_mutez_literal 2_000_000)
         (Ligo.sub_tez_tez (Ligo.tez_from_mutez_literal 5_000_000) (Ligo.tez_from_mutez_literal 3_000_000));
       assert_equal
         ~printer:show_tz
         (Ligo.tez_from_mutez_literal 5_000_000)
         (max (Ligo.tez_from_mutez_literal 5_000_000) (Ligo.tez_from_mutez_literal 3_000_000));
       assert_equal
         ~printer:(fun x -> x)
         "50309951mutez"
         (show_tz (Ligo.tez_from_mutez_literal 50_309_951));
    )
  ]
