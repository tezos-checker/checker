open OUnit2
open Lqt
open TestCommon

let suite =
  "LqtTests" >::: [
    "lqt arithmetic" >::
    (fun _ ->
       (* scaling factor (int) *)
       assert_equal ~printer:Ligo.string_of_int
         (Common.pow_int_nat (Ligo.int_from_literal "10") lqt_decimal_digits)
         lqt_scaling_factor_int;

       (* scaling factor (nat) *)
       assert_equal ~printer:Ligo.string_of_int
         lqt_scaling_factor_int
         (Ligo.int lqt_scaling_factor_nat);

       (* add *)
       assert_lqt_equal
         ~expected:(lqt_of_denomination (Ligo.nat_from_literal "8_000_000n"))
         ~real:(lqt_add (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n")) (lqt_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* subtract *)
       assert_lqt_equal
         ~expected:(lqt_of_denomination (Ligo.nat_from_literal "2_000_000n"))
         ~real:(lqt_sub (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n")) (lqt_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* compare *)
       assert_lqt_equal
         ~expected:(lqt_of_denomination (Ligo.nat_from_literal "5_000_000n"))
         ~real:(max (lqt_of_denomination (Ligo.nat_from_literal "5_000_000n")) (lqt_of_denomination (Ligo.nat_from_literal "3_000_000n")));

       (* show *)
       assert_equal
         ~printer:(fun x -> x)
         "50.309951lqt"
         (show_lqt (lqt_of_denomination (Ligo.nat_from_literal "50_309_951n")));
    )
  ]
