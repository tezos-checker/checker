open OUnit2
open Ratio
open FixedPoint
open Kit
open TestLib
open Error

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
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "8_000_000n"))
         ~real:(kit_add (kit_of_denomination (Ligo.nat_from_literal "5_000_000n")) (kit_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "2_000_000n"))
         ~real:(kit_sub (kit_of_denomination (Ligo.nat_from_literal "5_000_000n")) (kit_of_denomination (Ligo.nat_from_literal "3_000_000n")));
       assert_raises
         (Failure (Ligo.string_of_int internalError_KitSubNegative))
         (fun _ ->
            (kit_sub (kit_of_denomination (Ligo.nat_from_literal "1n")) (kit_of_denomination (Ligo.nat_from_literal "2n")));
         );

       (* scale *)
       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "15_370_401n"))
         ~real:(kit_scale (kit_of_denomination (Ligo.nat_from_literal "5_123_467n")) (fixedpoint_of_ratio_floor (ratio_of_int (Ligo.int_from_literal "3"))));

       (* fractions *)
       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "333_334n"))
         ~real:(kit_of_fraction_ceil (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));
       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "333_333n"))
         ~real:(kit_of_fraction_floor (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));

       (* compare *)
       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "5_000_000n"))
         ~real:(max (kit_of_denomination (Ligo.nat_from_literal "5_000_000n")) (kit_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* show *)
       assert_string_equal
         ~expected:"50309951mukit"
         ~real:(show_kit (kit_of_denomination (Ligo.nat_from_literal "50_309_951n")));
    )
  ]

let () =
  run_test_tt_main
    suite
