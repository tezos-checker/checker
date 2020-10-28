(* ************************************************************************* *)
(*                                Address                                    *)
(* ************************************************************************* *)
module Address : sig
  type t

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val of_string : string -> t
end =
struct
  type t = string
  [@@deriving show]

  let compare = String.compare

  let of_string s = s
end

