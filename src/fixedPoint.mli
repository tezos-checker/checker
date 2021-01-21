type t

(* Predefined values. *)
val zero : t
val one : t

(* Arithmetic operations. *)
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t

val neg : t -> t
val pow : t -> Ligo.nat -> t
val exp : t -> t

(* Conversions to/from other types. *)
val of_int : Ligo.int -> t
val to_ratio : t -> Ratio.t
val of_ratio_ceil : Ratio.t -> t
val of_ratio_floor : Ratio.t -> t

(* BEGIN_OCAML *)
val of_hex_string : string -> t
val show : t -> string
val pp : Format.formatter -> t -> unit
(* END_OCAML *)
