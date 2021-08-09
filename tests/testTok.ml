open OUnit2
open Tok
open TestLib
open Error

let suite =
  "TokTests" >::: [
    "tok arithmetic" >::
    (fun _ ->
       (* scaling factor (int) *)
       assert_int_equal
         ~expected:tok_scaling_factor_int
         ~real:(Common.pow_int_nat (Ligo.int_from_literal "10") tok_decimal_digits);

       (* scaling factor (nat) *)
       assert_int_equal
         ~expected:tok_scaling_factor_int
         ~real:(Ligo.int tok_scaling_factor_nat);

       (* add *)
       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "8_000_000n"))
         ~real:(tok_add (tok_of_denomination (Ligo.nat_from_literal "5_000_000n")) (tok_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "2_000_000n"))
         ~real:(tok_sub (tok_of_denomination (Ligo.nat_from_literal "5_000_000n")) (tok_of_denomination (Ligo.nat_from_literal "3_000_000n")));
       assert_raises
         (Failure (Ligo.string_of_int internalError_TokSubNegative))
         (fun _ ->
            (tok_sub (tok_of_denomination (Ligo.nat_from_literal "1n")) (tok_of_denomination (Ligo.nat_from_literal "2n")));
         );

       (* fractions *)
       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "333_334n"))
         ~real:(tok_of_fraction_ceil (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));
       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "333_333n"))
         ~real:(tok_of_fraction_floor (Ligo.int_from_literal "1") (Ligo.int_from_literal "3"));

       (* compare *)
       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "5_000_000n"))
         ~real:(max (tok_of_denomination (Ligo.nat_from_literal "5_000_000n")) (tok_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* show *)
       assert_string_equal
         ~expected:"50.309951tok"
         ~real:(show_tok (tok_of_denomination (Ligo.nat_from_literal "50_309_951n")));
    )
  ]

let () =
  run_test_tt_main
    suite
