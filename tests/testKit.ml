open OUnit2
open Ratio
open FixedPoint
open Kit
open TestCommon

let suite =
  "KitTests" >::: [
    "kit arithmetic" >::
    (fun _ ->
       (* scaling factor (int) *)
       assert_int_equal
         ~expected:kit_scaling_factor_int
         ~real:(Common.pow_int_nat (Ligo.int_from_literal "10") kit_decimal_digits);

       (* scaling factor (nat) *)
       assert_int_equal
         ~expected:kit_scaling_factor_int
         ~real:(Ligo.int kit_scaling_factor_nat);

       (* add *)
       assert_kit_equal
         ~expected:(kit_of_mukit (Ligo.nat_from_literal "8_000_000n"))
         ~real:(kit_add (kit_of_mukit (Ligo.nat_from_literal "5_000_000n")) (kit_of_mukit (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_kit_equal
         ~expected:(kit_of_mukit (Ligo.nat_from_literal "2_000_000n"))
         ~real:(kit_sub (kit_of_mukit (Ligo.nat_from_literal "5_000_000n")) (kit_of_mukit (Ligo.nat_from_literal "3_000_000n")));

       (* scale *)
       assert_kit_equal
         ~expected:(kit_of_mukit (Ligo.nat_from_literal "15_370_401n"))
         ~real:(kit_scale (kit_of_mukit (Ligo.nat_from_literal "5_123_467n")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "3"))));

       (* compare *)
       assert_kit_equal
         ~expected:(kit_of_mukit (Ligo.nat_from_literal "5_000_000n"))
         ~real:(max (kit_of_mukit (Ligo.nat_from_literal "5_000_000n")) (kit_of_mukit (Ligo.nat_from_literal "3_000_000n")));

       (* show *)
       assert_string_equal
         ~expected:"50309951mukit"
         ~real:(show_kit (kit_of_mukit (Ligo.nat_from_literal "50_309_951n")));
    )
  ]
