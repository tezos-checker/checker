type lqt

(* Basic operations. *)
val lqt_add : lqt -> lqt -> lqt
val lqt_sub : lqt -> lqt -> lqt

val lqt_zero : lqt
val lqt_one : lqt

val lqt_decimal_digits : Ligo.nat
val lqt_scaling_factor_int : Ligo.int
val lqt_scaling_factor_nat : Ligo.nat

(* Conversions to/from other types. *)
val lqt_of_denomination : Ligo.nat -> lqt
val lqt_to_denomination_int : lqt -> Ligo.int
val lqt_to_denomination_nat : lqt -> Ligo.nat

val lqt_of_fraction_ceil : Ligo.int -> Ligo.int -> lqt
val lqt_of_fraction_floor : Ligo.int -> Ligo.int -> lqt

val geq_lqt_lqt : lqt -> lqt -> bool

val eq_lqt_lqt : lqt -> lqt -> bool
val lt_lqt_lqt : lqt -> lqt -> bool

(* BEGIN_OCAML *)
val lqt_compare : lqt -> lqt -> int

val lqt_to_ratio : lqt -> Common.ratio

val pp_lqt : Format.formatter -> lqt -> unit
val show_lqt : lqt -> string
(* END_OCAML *)
