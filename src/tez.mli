type t

(* Basic operations. *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val compare : t -> t -> int

val zero : t
val one : t

(* Conversions to/from other types. *)
val of_mutez : int -> t
val to_ratio : t -> Ratio.t
val of_ratio_ceil : Ratio.t -> t
val of_ratio_floor : Ratio.t -> t

(* Pretty printing functions *)
val pp : Format.formatter -> t -> unit
val show : t -> string

(* Tez payments *)
type payment = {destination: Address.t ; amount: t;}

val pp_payment : Format.formatter -> payment -> unit
val show_payment : payment -> string

