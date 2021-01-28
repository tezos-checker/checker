open OUnit2
open Ratio
open FixedPoint
open Kit

type kt = kit [@@deriving show]
type fp = fixedpoint[@@deriving show]

let suite =
  "KitTests" >::: [
    "kit arithmetic" >::
    (fun _ ->
       (* add *)
       assert_equal ~printer:show_kt
         (kit_of_mukit (Ligo.nat_from_literal "8_000_000n"))
         (kit_add (kit_of_mukit (Ligo.nat_from_literal "5_000_000n")) (kit_of_mukit (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_equal ~printer:show_kt
         (kit_of_mukit (Ligo.nat_from_literal "2_000_000n"))
         (kit_sub (kit_of_mukit (Ligo.nat_from_literal "5_000_000n")) (kit_of_mukit (Ligo.nat_from_literal "3_000_000n")));

       (* scale *)
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.nat_from_literal "15_370_401n"))
         (kit_scale (kit_of_mukit (Ligo.nat_from_literal "5_123_467n")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "3"))));

       (* compare *)
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.nat_from_literal "5_000_000n"))
         (max (kit_of_mukit (Ligo.nat_from_literal "5_000_000n")) (kit_of_mukit (Ligo.nat_from_literal "3_000_000n")));

       (* show *)
       assert_equal
         ~printer:(fun x -> x)
         "50309951mukit"
         (show_kt (kit_of_mukit (Ligo.nat_from_literal "50_309_951n")));
    )
  ]
