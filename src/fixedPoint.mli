type fixedpoint

(* Predefined values. *)
val fixedpoint_zero : fixedpoint
val fixedpoint_one : fixedpoint

(* Arithmetic operations. *)
val fixedpoint_add : fixedpoint -> fixedpoint -> fixedpoint
val fixedpoint_sub : fixedpoint -> fixedpoint -> fixedpoint
val fixedpoint_mul : fixedpoint -> fixedpoint -> fixedpoint
val fixedpoint_div : fixedpoint -> fixedpoint -> fixedpoint

val fixedpoint_neg : fixedpoint -> fixedpoint
val fixedpoint_pow : fixedpoint -> Ligo.nat -> fixedpoint

(* Conversions to/from other types. *)
val fixedpoint_of_int : Ligo.int -> fixedpoint
val fixedpoint_to_ratio : fixedpoint  -> Ratio.ratio
val fixedpoint_of_ratio_ceil : Ratio.ratio -> fixedpoint
val fixedpoint_of_ratio_floor : Ratio.ratio -> fixedpoint

val fixedpoint_of_raw : Ligo.int -> fixedpoint
val fixedpoint_to_raw : fixedpoint -> Ligo.int

(* BEGIN_OCAML *)
val fixedpoint_of_hex_string : string -> fixedpoint
val show_fixedpoint : fixedpoint  -> string
val pp_fixedpoint : Format.formatter -> fixedpoint  -> unit
(* END_OCAML *)
