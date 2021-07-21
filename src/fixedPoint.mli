open Common

type fixedpoint

(* Predefined values. *)
val fixedpoint_zero : fixedpoint
val fixedpoint_one : fixedpoint
val fixedpoint_scaling_factor : Ligo.int

(* Arithmetic operations. *)
val fixedpoint_add : fixedpoint -> fixedpoint -> fixedpoint
val fixedpoint_sub : fixedpoint -> fixedpoint -> fixedpoint
val fixedpoint_pow : fixedpoint -> Ligo.nat -> fixedpoint

(* Conversions to/from other types. *)
val fixedpoint_of_ratio_ceil : ratio -> fixedpoint
val fixedpoint_of_ratio_floor : ratio -> fixedpoint

val fixedpoint_of_raw : Ligo.int -> fixedpoint
val fixedpoint_to_raw : fixedpoint -> Ligo.int

(* BEGIN_OCAML *)
val fixedpoint_to_ratio : fixedpoint  -> Common.ratio
val fixedpoint_of_hex_string : string -> fixedpoint
val show_fixedpoint : fixedpoint -> string
val pp_fixedpoint : Format.formatter -> fixedpoint  -> unit

val show_fixedpoint_raw : fixedpoint -> string
(* END_OCAML *)
