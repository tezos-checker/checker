module Duration : sig
  type t
  val of_seconds : int -> t
  val to_seconds : t -> int
end = struct
  type t = int
  let of_seconds s = s
  let to_seconds d = d
end
