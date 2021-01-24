open FixedPoint

type kit

(* Basic operations. *)
val add : kit -> kit -> kit
val sub : kit -> kit -> kit
val scale : kit -> fixedpoint -> kit

val min : kit -> kit -> kit
val max : kit -> kit -> kit

val zero : kit
val one : kit

(* Conversions to/from other types. *)
val of_mukit : Ligo.int -> kit
val to_mukit : kit -> Ligo.int
val to_ratio : kit -> Ratio.ratio
val of_ratio_ceil : Ratio.ratio -> kit
val of_ratio_floor : Ratio.ratio -> kit

(* Kit are really tickets. *)
type kit_token_content = Kit
type token = kit_token_content Ligo.ticket

val issue : kit -> token

val assert_valid_kit_token : token -> token

(* George: I really didn'kit want to have these here, but the clutter without
 * them was unbearable. They should be inlined, eventually. *)
val read_kit : token -> kit * token
val split_or_fail : token -> kit -> kit -> token * token
val join_or_fail : token -> token -> token

(* BEGIN_OCAML *)
val compare : kit -> kit -> int

val pp : Format.formatter -> kit -> unit
val pp_kit : Format.formatter -> kit -> unit
val show : kit -> string

val pp_token : Format.formatter -> token -> unit
val show_token : token -> string
(* END_OCAML *)
