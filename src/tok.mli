type tok

val tok_token_id : Fa2Interface.fa2_token_id

val tok_decimal_digits : Ligo.nat
val tok_scaling_factor_int : Ligo.int
val tok_scaling_factor_nat : Ligo.nat

val tok_add : tok -> tok -> tok
val tok_sub : tok -> tok -> tok

val tok_min : tok -> tok -> tok
val tok_max : tok -> tok -> tok

val tok_zero : tok
val tok_one : tok

val tok_of_denomination : Ligo.nat -> tok
val tok_to_denomination_int : tok -> Ligo.int
val tok_to_denomination_nat : tok -> Ligo.nat

val tok_of_fraction_ceil : Ligo.int -> Ligo.int -> tok
val tok_of_fraction_floor : Ligo.int -> Ligo.int -> tok
val tok_scale : tok -> FixedPoint.fixedpoint -> tok

val leq_tok_tok : tok -> tok -> bool
val geq_tok_tok : tok -> tok -> bool

val lt_tok_tok : tok -> tok -> bool
val eq_tok_tok : tok -> tok -> bool
val gt_tok_tok : tok -> tok -> bool

(* BEGIN_OCAML *)
val tok_to_ratio : tok -> Common.ratio
val tok_compare : tok -> tok -> int

val show_tok : tok -> string
val pp_tok : Format.formatter -> tok -> unit
(* END_OCAML *)
