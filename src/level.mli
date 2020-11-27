type t
val show : t -> string
val pp : Format.formatter -> t -> unit
val of_int : int -> t
val blocks_elapsed : start:t -> finish:t -> int
val cycle : t -> int
