(* ************************************************************************* *)
(*                               FixedPoint                                  *)
(* ************************************************************************* *)
type t

val scaling_factor : Z.t

(* Predefined values. *)
val zero : t
val one : t

(* Arithmetic operations. *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val neg : t -> t
val pow : t -> int -> t

(* Conversions to/from other types. *)
val of_int : int -> t
val to_int : t -> int
val of_rep : Z.t -> t (* NOTE: Exposes internal representation. *)
val to_rep : t -> Z.t (* NOTE: Exposes internal representation. *)
val of_string : string -> t

val to_q : t -> Q.t (* NOTE: For precision. *)
val of_q_ceil : Q.t -> t (* NOTE: For precision. *)
val of_q_floor : Q.t -> t (* NOTE: For precision. *)

val exp : t -> t

(* Pretty printing functions *)
val pp : Format.formatter -> t -> unit
val show : t -> string

