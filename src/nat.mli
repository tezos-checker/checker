type t (* INVARIANT: NON-NEGATIVE *)

val show : t -> string
val pp : Format.formatter -> t -> unit
val from_literal : Int.t -> t

val compare : t -> t -> int
val add : t -> t -> t
val sub : t -> t -> Z.t

val int : t -> Z.t
val abs : Z.t -> t
val is_nat : Z.t -> t option
