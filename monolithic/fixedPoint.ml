(* ************************************************************************* *)
(*                                   Tez                                     *)
(* ************************************************************************* *)
module FixedPoint : sig
  type t

  (* Basic arithmetic operations. TODO: delete division, or at least limit it. *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t

  (* Conversions to/from other types. *)
  val of_float : float -> t (* TODO: Delete this one. *)
  val to_float : t -> float (* TODO: Delete this one. *)

  (* Pretty printing functions *)
  val pp : t -> Format.formatter -> unit
end =
struct
  type t = float (* TODO: Use int64 instead *)

  (* Basic arithmetic operations. TODO: delete division, or at least limit it. *)
  let add = ( +. ) (* TODO: Add checks for over/under-flow *)
  let sub = ( -. ) (* TODO: Add checks for over/under-flow *)
  let mul = ( *. ) (* TODO: Add checks for over/under-flow *)
  let div = ( /. ) (* TODO: Add checks for over/under-flow *)

  (* Conversions to/from other types. *)
  let of_float amount = amount
  let to_float amount = amount

  (* Pretty printing functions *)
  let pp amount ppf = Format.fprintf ppf "%.16f" amount
end

