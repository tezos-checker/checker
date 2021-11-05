open OUnit2
open Ctok
open TestLib
open Error

let suite =
  "CtokTests" >::: [
    "ctok arithmetic" >::
    (fun _ ->

       (* add *)
       assert_ctok_equal
         ~expected:(ctok_of_denomination (Ligo.nat_from_literal "8_000_000n"))
         ~real:(ctok_add (ctok_of_denomination (Ligo.nat_from_literal "5_000_000n")) (ctok_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_ctok_equal
         ~expected:(ctok_of_denomination (Ligo.nat_from_literal "2_000_000n"))
         ~real:(ctok_sub (ctok_of_denomination (Ligo.nat_from_literal "5_000_000n")) (ctok_of_denomination (Ligo.nat_from_literal "3_000_000n")));
       assert_raises
         (Failure (Ligo.string_of_int internalError_CtokSubNegative))
         (fun _ ->
            (ctok_sub (ctok_of_denomination (Ligo.nat_from_literal "1n")) (ctok_of_denomination (Ligo.nat_from_literal "2n")));
         );

       (* fractions *)
       assert_ctok_equal
         ~expected:(ctok_of_denomination (Ligo.nat_from_literal "333_334n"))
         ~real:(ctok_of_fraction_ceil (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));

       assert_ctok_equal
         ~expected:(ctok_of_denomination (Ligo.nat_from_literal "333_333n"))
         ~real:(ctok_of_fraction_floor (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));

       (* divide *)
       assert_ctok_equal
         ~expected:(ctok_of_denomination (Ligo.nat_from_literal "5_000_000n"))
         ~real:(max (ctok_of_denomination (Ligo.nat_from_literal "5_000_000n")) (ctok_of_denomination (Ligo.nat_from_literal "3_000_000n")));


       (* show *)
       assert_string_equal
         ~expected:"50.309951ctok"
         ~real:(show_ctok (ctok_of_denomination (Ligo.nat_from_literal "50_309_951n")));
    )
  ]

let () =
  run_test_tt_main
    suite
