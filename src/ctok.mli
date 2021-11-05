type ctok

val ctok_token_id : Fa2Interface.fa2_token_id

val ctok_decimal_digits : Ligo.nat
val ctok_scaling_factor_int : Ligo.int
val ctok_scaling_factor_nat : Ligo.nat

val ctok_add : ctok -> ctok -> ctok
val ctok_sub : ctok -> ctok -> ctok

val ctok_min : ctok -> ctok -> ctok
val ctok_max : ctok -> ctok -> ctok

val ctok_zero : ctok
val ctok_one : ctok

val ctok_of_denomination : Ligo.nat -> ctok
val ctok_to_denomination_int : ctok -> Ligo.int
val ctok_to_denomination_nat : ctok -> Ligo.nat

val ctok_of_fraction_ceil : Ligo.int -> Ligo.int -> ctok
val ctok_of_fraction_floor : Ligo.int -> Ligo.int -> ctok
val ctok_scale : ctok -> FixedPoint.fixedpoint -> ctok

val leq_ctok_ctok : ctok -> ctok -> bool
val geq_ctok_ctok : ctok -> ctok -> bool

val lt_ctok_ctok : ctok -> ctok -> bool
val eq_ctok_ctok : ctok -> ctok -> bool
val gt_ctok_ctok : ctok -> ctok -> bool

(* BEGIN_OCAML *)
val ctok_to_ratio : ctok -> Common.ratio
val ctok_compare : ctok -> ctok -> int

val show_ctok : ctok -> string
val pp_ctok : Format.formatter -> ctok -> unit
(* END_OCAML *)
