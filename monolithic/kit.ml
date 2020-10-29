
open Address
open FixedPoint

(* ************************************************************************* *)
(*                                   Kit                                     *)
(* ************************************************************************* *)
module Kit : sig
  type t (* Invariant: >= zero *)

  val scaling_factor : Z.t

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
  val show_kit : t -> string

  (* Kit UTXO *)
  type utxo = {destination : Address.t ; amount : t}
  val show_utxo : utxo -> string
  val pp_utxo : Format.formatter -> utxo -> unit
end =
struct
  type t = Z.t
  let scaling_factor = Z.of_int64 1000000L
  let scaling_exponent = 6

  (* Basic arithmetic operations. TODO: delete division, or at least limit it. *)
  let add x y = Z.(x + y)
  let sub x y = Z.(x - y)

  let compare x y = Z.compare x y

  let zero = Z.zero
  let one = scaling_factor

  (* Conversions to/from other types. *)
  let of_float amount = (* TODO: lossy *)
    let upper = Z.of_float amount in
    let lower = Z.of_float ((amount -. Z.to_float upper) *. Z.to_float scaling_factor) in
    Z.add (Z.mul upper scaling_factor) lower

  let to_float amount = (* TODO: lossy *)
    (Z.to_float amount) /. Z.to_float scaling_factor

  let of_fp fp =
    Z.div
      (FixedPoint.to_z fp)
      (Z.div FixedPoint.scaling_factor scaling_factor)

  let to_fp t = (* TODO: overflow check? *)
    FixedPoint.of_z (Z.mul t (Z.div FixedPoint.scaling_factor scaling_factor))

  let div x y = (* TODO: lossy *)
    FixedPoint.(to_fp x / to_fp y)

  let scale amount fp = (* TODO: Over/Under- flow checks *)
    of_fp FixedPoint.(to_fp amount * fp)

  let partition x = (Z.div x scaling_factor, Z.rem x scaling_factor)

  (* Pretty printing functions *)
  let show_kit amount =
    let zfill s width =
      let to_fill = width - (String.length s) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let (upper, lower) = partition amount in
    Format.sprintf "%s.%s"
      (Z.to_string upper)
      (zfill (Z.to_string lower) scaling_exponent)

  let pp ppf amount =
    Format.fprintf ppf "%s" (show_kit amount)

  (* Kit UTXO *)
  type utxo = {destination : Address.t ; amount : t}
  [@@deriving show]
end

