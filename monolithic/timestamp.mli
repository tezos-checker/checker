type t
val show : t -> string
val pp : Format.formatter -> t -> unit
val of_seconds : int -> t
val to_seconds : t -> int
val seconds_elapsed : start:t -> finish:t -> int
