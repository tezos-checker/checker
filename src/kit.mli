open FixedPoint

type kit

(* Basic operations. *)
val kit_add : kit -> kit -> kit
val kit_sub : kit -> kit -> kit
val kit_scale : kit -> fixedpoint -> kit

val kit_min : kit -> kit -> kit
val kit_max : kit -> kit -> kit

val kit_zero : kit
val kit_one : kit

(* Conversions to/from other types. *)
val kit_of_mukit : Ligo.int -> kit
val kit_to_mukit : kit -> Ligo.int
val kit_to_ratio : kit -> Ratio.ratio
val kit_of_ratio_ceil : Ratio.ratio -> kit
val kit_of_ratio_floor : Ratio.ratio -> kit

(* Kit are really tickets. *)
type kit_token_content = Kit
type kit_token = kit_token_content Ligo.ticket

val kit_issue : kit -> kit_token

val assert_valid_kit_token : kit_token -> kit_token

(* George: I really didn'kit want to have these here, but the clutter without
 * them was unbearable. They should be inlined, eventually. *)
val read_kit : kit_token -> kit * kit_token
val kit_split_or_fail : kit_token -> kit -> kit -> kit_token * kit_token
val kit_join_or_fail : kit_token -> kit_token -> kit_token

(* BEGIN_OCAML *)
val kit_compare : kit -> kit -> int

val pp : Format.formatter -> kit -> unit
val pp_kit : Format.formatter -> kit -> unit
val show : kit -> string
val show_kit : kit -> string

val pp_kit_token : Format.formatter -> kit_token -> unit
val show_kit_token : kit_token -> string
(* END_OCAML *)
