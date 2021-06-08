open OUnit2
open TestCommon

let suite =
  "TezTests" >::: [
    "tez arithmetic" >::
    (fun _ ->
       assert_tez_equal
         ~expected:(Ligo.tez_from_literal "8_000_000mutez")
         ~real:(Ligo.add_tez_tez (Ligo.tez_from_literal "5_000_000mutez") (Ligo.tez_from_literal "3_000_000mutez"));
       assert_tez_equal
         ~expected:(Ligo.tez_from_literal "2_000_000mutez")
         ~real:(Ligo.sub_tez_tez (Ligo.tez_from_literal "5_000_000mutez") (Ligo.tez_from_literal "3_000_000mutez"));
       assert_tez_equal
         ~expected:(Ligo.tez_from_literal "5_000_000mutez")
         ~real:(max (Ligo.tez_from_literal "5_000_000mutez") (Ligo.tez_from_literal "3_000_000mutez"));
       assert_string_equal
         ~expected:"50309951mutez"
         ~real:(Ligo.string_of_tez (Ligo.tez_from_literal "50_309_951mutez"));
    )
  ]
