(* ************************************************************************* *)
(*                                Address                                    *)
(* ************************************************************************* *)
type t

val show : t -> string
val pp : Format.formatter -> t -> unit

val initial_address : t
val next : t -> t

val compare : t -> t -> int

val of_string : string -> t

