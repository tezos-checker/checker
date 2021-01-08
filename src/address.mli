type t

val show : t -> string
val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val of_string : string -> t
