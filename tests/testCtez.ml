open OUnit2
open Ctez
open TestLib

let suite =
  "CtezTests" >::: [
    "ctez arithmetic" >::
    (fun _ ->

       (* add *)
       assert_ctez_equal
         ~expected:(ctez_of_muctez (Ligo.nat_from_literal "8_000_000n"))
         ~real:(ctez_add (ctez_of_muctez (Ligo.nat_from_literal "5_000_000n")) (ctez_of_muctez (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_ctez_equal
         ~expected:(ctez_of_muctez (Ligo.nat_from_literal "2_000_000n"))
         ~real:(ctez_sub (ctez_of_muctez (Ligo.nat_from_literal "5_000_000n")) (ctez_of_muctez (Ligo.nat_from_literal "3_000_000n")));
       assert_raises
         (Failure "Ctez.ctez_sub: negative")
         (fun _ ->
            (ctez_sub (ctez_of_muctez (Ligo.nat_from_literal "1n")) (ctez_of_muctez (Ligo.nat_from_literal "2n")));
         );

       (* fractions *)
       assert_ctez_equal
         ~expected:(ctez_of_muctez (Ligo.nat_from_literal "333_334n"))
         ~real:(ctez_of_fraction_ceil (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));

       assert_ctez_equal
         ~expected:(ctez_of_muctez (Ligo.nat_from_literal "333_333n"))
         ~real:(ctez_of_fraction_floor (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));

       (* divide *)
       assert_ctez_equal
         ~expected:(ctez_of_muctez (Ligo.nat_from_literal "5_000_000n"))
         ~real:(max (ctez_of_muctez (Ligo.nat_from_literal "5_000_000n")) (ctez_of_muctez (Ligo.nat_from_literal "3_000_000n")));


       (* show *)
       assert_string_equal
         ~expected:"50309951muctez"
         ~real:(show_ctez (ctez_of_muctez (Ligo.nat_from_literal "50_309_951n")));
    )
  ]
