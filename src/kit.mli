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
val kit_scaling_factor_int : Ligo.int

(* Conversions to/from other types. *)
val kit_of_mukit : Ligo.nat -> kit
val kit_to_mukit_int : kit -> Ligo.int
val kit_to_mukit_nat : kit -> Ligo.nat

val kit_of_ratio_ceil : Ratio.ratio -> kit
val kit_of_ratio_floor : Ratio.ratio -> kit

(* BEGIN_OCAML *)
val kit_compare : kit -> kit -> int

val kit_to_ratio : kit -> Ratio.ratio

val pp_kit : Format.formatter -> kit -> unit
val show_kit : kit -> string
(* END_OCAML *)
