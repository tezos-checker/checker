open OUnit2
open TestLib
open Error
open Common

let suite =
  "CommonTests" >::: [
    "pow_int_nat" >::
    (fun _ ->
       assert_int_equal
         ~expected:(Ligo.int_from_literal "1")
         ~real:(pow_int_nat (Ligo.int_from_literal "3") (Ligo.nat_from_literal "0n"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "3")
         ~real:(pow_int_nat (Ligo.int_from_literal "3") (Ligo.nat_from_literal "1n"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "9")
         ~real:(pow_int_nat (Ligo.int_from_literal "3") (Ligo.nat_from_literal "2n"));
    );

    "cdiv_int_int - fails when denominator is zero" >::
    (fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int internalError_CdivIntIntZeroDenominator))
         (fun _ -> cdiv_int_int (Ligo.int_from_literal "5") (Ligo.int_from_literal "0"))
    );

    "cdiv_int_int" >::
    (fun _ ->
       assert_int_equal
         ~expected:(Ligo.int_from_literal "3")
         ~real:(cdiv_int_int (Ligo.int_from_literal "5") (Ligo.int_from_literal "2"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "-2")
         ~real:(cdiv_int_int (Ligo.int_from_literal "-5") (Ligo.int_from_literal "2"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "-2")
         ~real:(cdiv_int_int (Ligo.int_from_literal "5") (Ligo.int_from_literal "-2"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "3")
         ~real:(cdiv_int_int (Ligo.int_from_literal "-5") (Ligo.int_from_literal "-2"));
    );

    "fdiv_int_int - fails when denominator is zero" >::
    (fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int internalError_FdivIntIntZeroDenominator))
         (fun _ -> fdiv_int_int (Ligo.int_from_literal "5") (Ligo.int_from_literal "0"))
    );

    "fdiv_int_int" >::
    (fun _ ->
       assert_int_equal
         ~expected:(Ligo.int_from_literal "2")
         ~real:(fdiv_int_int (Ligo.int_from_literal "5") (Ligo.int_from_literal "2"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "-3")
         ~real:(fdiv_int_int (Ligo.int_from_literal "-5") (Ligo.int_from_literal "2"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "-3")
         ~real:(fdiv_int_int (Ligo.int_from_literal "5") (Ligo.int_from_literal "-2"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "2")
         ~real:(fdiv_int_int (Ligo.int_from_literal "-5") (Ligo.int_from_literal "-2"));
    );

    "clamp_int" >::
    (fun _ ->
       assert_int_equal
         ~expected:(Ligo.int_from_literal "-5")
         ~real:(clamp_int (Ligo.int_from_literal "-6") (Ligo.int_from_literal "-5") (Ligo.int_from_literal "7"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "-2")
         ~real:(clamp_int (Ligo.int_from_literal "-2") (Ligo.int_from_literal "-5") (Ligo.int_from_literal "7"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "2")
         ~real:(clamp_int (Ligo.int_from_literal "2") (Ligo.int_from_literal "-5") (Ligo.int_from_literal "7"));
       assert_int_equal
         ~expected:(Ligo.int_from_literal "7")
         ~real:(clamp_int (Ligo.int_from_literal "8") (Ligo.int_from_literal "-5") (Ligo.int_from_literal "7"));
    );

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
