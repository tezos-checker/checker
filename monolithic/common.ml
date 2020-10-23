(* ************************************************************************* *)
(*                                 Common                                    *)
(* ************************************************************************* *)
(* Things we don't really care about much at the moment; presumably will be
 * available when the time comes to implement the smart contracts. Consider
 * adding other things here (e.g. liquidity) to keep them more opaque. *)
module Common : sig
  type address

  val show_address : address -> string
  val pp_address : Format.formatter -> address -> unit

  val of_string : string -> address
end =
struct
  type address = string
  [@@deriving show]

  let of_string s = s
end

