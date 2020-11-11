(* ************************************************************************* *)
(*                                   Kit                                     *)
(* ************************************************************************* *)
type t

val scaling_factor : Z.t

(* Basic arithmetic operations. *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( / ) : t -> t -> FixedPoint.t

val zero : t
val one : t

val compare : t -> t -> int

(* Conversions to/from other types. *)
val of_mukit : int -> t

val to_fp : t -> FixedPoint.t

val to_q : t -> Q.t (* NOTE: For precision. *)
val of_q_ceil : Q.t -> t (* NOTE: For precision. *)
val of_q_floor : Q.t -> t (* NOTE: For precision. *)

val scale : t -> FixedPoint.t -> t

(* Pretty printing functions *)
val pp : Format.formatter -> t -> unit
val show : t -> string

