(* ************************************************************************* *)
(*                                Address                                    *)
(* ************************************************************************* *)
module Address : sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val initial_address : t
  val next : t -> t

  val compare : t -> t -> int

  val of_string : string -> t
end =
struct
  type t = int

  let show address =
    Format.sprintf "tz_%d" address

  let pp ppf address =
    Format.fprintf ppf "%s" (show address)

  let initial_address = 0
  let next = succ

  let compare = Stdlib.compare

  let of_string s = int_of_string s
end

