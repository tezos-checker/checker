(* ************************************************************************* *)
(*                                   Kit                                     *)
(* ************************************************************************* *)
type t

(* Basic operations. *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val scale : t -> FixedPoint.t -> t
val compare : t -> t -> int

val zero : t
val one : t

(* Conversions to/from other types. *)
val of_mukit : int -> t
val to_q : t -> Q.t
val of_q_ceil : Q.t -> t
val of_q_floor : Q.t -> t

(* Pretty printing functions *)
val pp : Format.formatter -> t -> unit
val show : t -> string

