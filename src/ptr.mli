type t (* George: perhaps I'd prefer a phantom parameter to not mix up pointers. *)

val show : t -> string
val pp : Format.formatter -> t -> unit

val null : t
val init : t
val next : t -> t

val compare : t -> t -> int

val to_string : t -> string
