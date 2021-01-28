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
val kit_of_mukit : Ligo.nat -> kit
val kit_to_mukit : kit -> Ligo.nat
val kit_to_ratio : kit -> Ratio.ratio
val kit_of_ratio_ceil : Ratio.ratio -> kit
val kit_of_ratio_floor : Ratio.ratio -> kit

(* BEGIN_OCAML *)
val kit_compare : kit -> kit -> int

val pp_kit : Format.formatter -> kit -> unit
val show_kit : kit -> string
(* END_OCAML *)
