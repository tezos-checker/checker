module Timestamp : sig
  type t
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val of_seconds : int -> t
  val to_seconds : t -> int
  val seconds_elapsed : t -> t -> int
end = struct
  type t = int [@@deriving show]
  let of_seconds s = s
  let to_seconds d = d
  let seconds_elapsed start finish = finish - start
end
