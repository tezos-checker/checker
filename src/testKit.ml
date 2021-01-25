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
         (kit_of_mukit (Ligo.int_from_literal "8_000_000"))
         (kit_add (kit_of_mukit (Ligo.int_from_literal "5_000_000")) (kit_of_mukit (Ligo.int_from_literal "3_000_000")));
       assert_equal ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "2_000_000"))
         (kit_add (kit_of_mukit (Ligo.int_from_literal "5_000_000")) (kit_of_mukit (Ligo.int_from_literal "-3_000_000")));

       (* subtract *)
       assert_equal ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "2_000_000"))
         (kit_sub (kit_of_mukit (Ligo.int_from_literal "5_000_000")) (kit_of_mukit (Ligo.int_from_literal "3_000_000")));
       assert_equal ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "8_000_000"))
         (kit_sub (kit_of_mukit (Ligo.int_from_literal "5_000_000")) (kit_of_mukit (Ligo.int_from_literal "-3_000_000")));

       (* scale *)
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "15_370_401"))
         (kit_scale (kit_of_mukit (Ligo.int_from_literal "5_123_467")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "3"))));
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "-15_370_401"))
         (kit_scale (kit_of_mukit (Ligo.int_from_literal "5_123_467")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "-3"))));
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "-15_370_401"))
         (kit_scale (kit_of_mukit (Ligo.int_from_literal "-5_123_467")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "3"))));
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "15_370_401"))
         (kit_scale (kit_of_mukit (Ligo.int_from_literal "-5_123_467")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "-3"))));

       (* compare *)
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "5_000_000"))
         (max (kit_of_mukit (Ligo.int_from_literal "5_000_000")) (kit_of_mukit (Ligo.int_from_literal "3_000_000")));
       assert_equal
         ~printer:show_kt
         (kit_of_mukit (Ligo.int_from_literal "-3_000_000"))
         (max (kit_of_mukit (Ligo.int_from_literal "-5_000_000")) (kit_of_mukit (Ligo.int_from_literal "-3_000_000")));

       (* show *)
       assert_equal
         ~printer:(fun x -> x)
         "-50309951mukit"
         (show_kt (kit_of_mukit (Ligo.int_from_literal "-50_309_951")));
       assert_equal
         ~printer:(fun x -> x)
         "50309951mukit"
         (show_kt (kit_of_mukit (Ligo.int_from_literal "50_309_951")));
    )
  ]
