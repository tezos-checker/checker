type t

(* Basic operations. *)
val add : t -> t -> t
val sub : t -> t -> t
val compare : t -> t -> int

val zero : t
val one : t

(* Conversions to/from other types. *)
val of_mutez : Ligo.int -> t
val to_mutez : t -> Ligo.int (* as-is *)
val to_ratio : t -> Ratio.t (* divided by scaling factor *)
val of_ratio_ceil : Ratio.t -> t
val of_ratio_floor : Ratio.t -> t

(* Pretty printing functions *)
val pp : Format.formatter -> t -> unit
val show : t -> string

(* Tez payments *)
type payment = {destination: Ligo.address ; amount: t;}

val pp_payment : Format.formatter -> payment -> unit
val show_payment : payment -> string

