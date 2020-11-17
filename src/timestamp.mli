type t
val show : t -> string
val pp : Format.formatter -> t -> unit
val of_seconds : int -> t
val seconds_elapsed : start:t -> finish:t -> int
val add_seconds : t -> int -> t
