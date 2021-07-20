open OUnit2
open TestLib
open Error
open Ratio

let suite =
  "RatioTests" >::: [
    "fraction_to_tez_floor" >::
    (fun _ ->
       assert_tez_equal
         ~expected:(Ligo.tez_from_literal "333_333mutez")
         ~real:(fraction_to_tez_floor (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"))
    );
    "fraction_to_tez_floor - fails for negative numerators" >::
    (fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int internalError_FractionToTezFloorNegative))
         (fun _ -> fraction_to_tez_floor (Ligo.int_from_literal "-1") (Ligo.int_from_literal "2")
         )
    );
  ]

let () =
  run_test_tt_main
    suite
