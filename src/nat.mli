type t (* INVARIANT: NON-NEGATIVE *)

val show : t -> string
val pp : Format.formatter -> t -> unit
val from_literal : Int.t -> t

val compare : t -> t -> int
val add : t -> t -> t
val sub : t -> t -> Ligo.int

val int : t -> Ligo.int
val abs : Ligo.int -> t
val is_nat : Ligo.int -> t option
