open OUnit2
open Lqt
open TestLib
open Error

let suite =
  "LqtTests"
  >::: [
         ( "lqt arithmetic" >:: fun _ ->
           (* scaling factor (int) *)
           assert_int_equal ~expected:lqt_scaling_factor_int
             ~real:
               (Common.pow_int_nat
                  (Ligo.int_from_literal "10")
                  lqt_decimal_digits);

           (* scaling factor (nat) *)
           assert_int_equal ~expected:lqt_scaling_factor_int
             ~real:(Ligo.int lqt_scaling_factor_nat);

           (* add *)
           assert_lqt_equal
             ~expected:
               (lqt_of_denomination (Ligo.nat_from_literal "8_000_000n"))
             ~real:
               (lqt_add
                  (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n"))
                  (lqt_of_denomination (Ligo.nat_from_literal "3_000_000n")));

           (* subtract *)
           assert_lqt_equal
             ~expected:
               (lqt_of_denomination (Ligo.nat_from_literal "2_000_000n"))
             ~real:
               (lqt_sub
                  (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n"))
                  (lqt_of_denomination (Ligo.nat_from_literal "3_000_000n")));
           assert_raises
             (Failure (Ligo.string_of_int internalError_LqtSubNegative))
             (fun _ ->
               lqt_sub
                 (lqt_of_denomination (Ligo.nat_from_literal "1n"))
                 (lqt_of_denomination (Ligo.nat_from_literal "2n")));

           (* fractions *)
           assert_lqt_equal
             ~expected:(lqt_of_denomination (Ligo.nat_from_literal "333_334n"))
             ~real:
               (lqt_of_fraction_ceil
                  (Ligo.int_from_literal "1")
                  (Ligo.int_from_literal "3"));
           assert_lqt_equal
             ~expected:(lqt_of_denomination (Ligo.nat_from_literal "333_333n"))
             ~real:
               (lqt_of_fraction_floor
                  (Ligo.int_from_literal "1")
                  (Ligo.int_from_literal "3"));

           (* compare *)
           assert_lqt_equal
             ~expected:
               (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n"))
             ~real:
               (max
                  (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n"))
                  (lqt_of_denomination (Ligo.nat_from_literal "3_000_000n")));

           (* show *)
           assert_string_equal ~expected:"50.309951lqt"
             ~real:
               (show_lqt
                  (lqt_of_denomination (Ligo.nat_from_literal "50_309_951n")))
         );
       ]

let () = run_test_tt_main suite
