
(* TODO: Switch to Z from "zarith" *)

open Address
open FixedPoint

(* ************************************************************************* *)
(*                                   Tez                                     *)
(* ************************************************************************* *)
module Tez : sig
  type t (* Invariant: >= zero *)

  val scaling_factor : Int64.t

  (* Basic arithmetic operations. TODO: delete division, or at least limit it. *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> FixedPoint.t

  val zero : t
  val one : t

  val compare : t -> t -> int

  (* Conversions to/from other types. *)
  val of_float : float -> t (* TODO: Delete this one. *)
  val to_float : t -> float (* TODO: Delete this one. *)

  val of_fp : FixedPoint.t -> t
  val to_fp : t -> FixedPoint.t

  val scale : t -> FixedPoint.t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
  val show_tez : t -> string

  (* Tez UTXO *)
  type utxo = {destination : Address.t ; amount : t}
  val show_utxo : utxo -> string
  val pp_utxo : Format.formatter -> utxo -> unit
end =
struct
  type t = Int64.t
  let scaling_factor = 1000000L
  let scaling_exponent = 6

  (* Basic arithmetic operations. TODO: delete division, or at least limit it. *)
  let add x y =
    assert (x >= 0L);
    assert (y >= 0L);
    assert (not (x > Int64.sub Int64.max_int y)); (* Overflow *)
    assert (not (y > Int64.sub Int64.max_int x)); (* Overflow *)
    Int64.add x y

  let sub x y =
    assert (y >= 0L);
    assert (x >= y);
    Int64.sub x y

  let compare x y = Int64.compare x y

  let zero = 0L
  let one = scaling_factor

  (* Conversions to/from other types. *)
  let of_float amount = (* TODO: lossy *)
    assert (amount >= 0.0);
    let upper = Int64.of_float amount in
    let lower = Int64.of_float ((amount -. Int64.to_float upper) *. Int64.to_float scaling_factor) in
    Int64.add (Int64.mul upper scaling_factor) lower

  let to_float amount = (* TODO: lossy *)
    (Int64.to_float amount) /. Int64.to_float scaling_factor

  let of_fp fp =
    Int64.div
      (FixedPoint.to_int64 fp)
      (Int64.div FixedPoint.scaling_factor scaling_factor)

  let to_fp t = (* TODO: overflow check? *)
    FixedPoint.of_int64 (Int64.mul t (Int64.div FixedPoint.scaling_factor scaling_factor))

  let div x y = (* TODO: lossy *)
    assert (x >= 0L);
    assert (y >= 0L);
    assert (y > 0L); (* Overflow *)
    FixedPoint.(to_fp x / to_fp y)

  let scale amount fp = (* TODO: Over/Under- flow checks *)
    of_fp FixedPoint.(to_fp amount * fp)

  let partition x = (Int64.div x scaling_factor, Int64.rem x scaling_factor)

  (* Pretty printing functions *)
  let show_tez amount =
    let zfill s width =
      let to_fill = width - (String.length s) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let (upper, lower) = partition amount in
    Format.sprintf "%s.%s"
      (Int64.to_string upper)
      (zfill (Int64.to_string lower) scaling_exponent)

  let pp ppf amount =
    Format.fprintf ppf "%s" (show_tez amount)

  (* Tez UTXO *)
  type utxo = {destination : Address.t ; amount : t}
  [@@deriving show]
end

