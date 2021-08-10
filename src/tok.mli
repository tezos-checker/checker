type tok

(* Basic operations. *)
val tok_add : tok -> tok -> tok
val tok_sub : tok -> tok -> tok

val tok_zero : tok
val tok_one : tok

val tok_decimal_digits : Ligo.nat
val tok_scaling_factor_int : Ligo.int
val tok_scaling_factor_nat : Ligo.nat

(* Conversions to/from other types. *)
val tok_of_denomination : Ligo.nat -> tok
val tok_to_denomination_int : tok -> Ligo.int
val tok_to_denomination_nat : tok -> Ligo.nat

(* TOKFIX: temporary, compatibility layer *)
val tok_of_tez : Ligo.tez -> tok
val tez_of_tok : tok -> Ligo.tez

val tok_of_fraction_ceil : Ligo.int -> Ligo.int -> tok
val tok_of_fraction_floor : Ligo.int -> Ligo.int -> tok

val geq_tok_tok : tok -> tok -> bool
val leq_tok_tok : tok -> tok -> bool

val eq_tok_tok : tok -> tok -> bool
val gt_tok_tok : tok -> tok -> bool
val lt_tok_tok : tok -> tok -> bool

val max_tok : tok -> tok -> tok

(* BEGIN_OCAML *)
val tok_compare : tok -> tok -> int

val tok_to_ratio : tok -> Common.ratio

val pp_tok : Format.formatter -> tok -> unit
val show_tok : tok -> string
(* END_OCAML *)
