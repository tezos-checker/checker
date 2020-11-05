
open Address
open FixedPoint

(* ************************************************************************* *)
(*                                   Tez                                     *)
(* ************************************************************************* *)
module Tez : sig
  type t (* Invariant: >= zero *)

  val scaling_factor : Z.t

  (* Basic arithmetic operations. *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> FixedPoint.t

  val zero : t
  val one : t

  val compare : t -> t -> int

  (* Conversions to/from other types. *)
  val of_string : string -> t

  val of_fp : FixedPoint.t -> t
  val to_fp : t -> FixedPoint.t

  val scale : t -> FixedPoint.t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
  val show : t -> string

  (* Tez UTXO *)
  type utxo = {destination : Address.t ; amount : t}
  val show_utxo : utxo -> string
  val pp_utxo : Format.formatter -> utxo -> unit
end =
struct
  type t = Z.t
  let scaling_factor = Z.of_int64 1000000L
  let scaling_exponent = 6

  (* Basic arithmetic operations. *)
  let ( + ) x y = Z.(x + y)
  let ( - ) x y = Z.(x - y)

  let compare x y = Z.compare x y

  let zero = Z.zero
  let one = scaling_factor

  (* Conversions to/from other types. *)
  let of_string str =
    let without_dot = Str.replace_first (Str.regexp (Str.quote ".")) "" str in
    let dotpos = String.rindex_opt str '.' in
    let mantissa = match dotpos with
      | None -> Z.one
      | Some pos -> Z.pow (Z.of_int 10) Stdlib.(String.length str - pos - 1) in
    Z.((Z.of_string_base 10 without_dot * scaling_factor) / mantissa)

  let of_fp fp =
    Z.((FixedPoint.to_rep fp) * scaling_factor / FixedPoint.scaling_factor)

  let to_fp t = (* TODO: overflow check? *)
    FixedPoint.of_rep Z.(t * (FixedPoint.scaling_factor / scaling_factor))

  let ( / ) x y = (* TODO: lossy *)
    FixedPoint.(to_fp x / to_fp y)

  let scale amount fp = (* TODO: Over/Under- flow checks *)
    of_fp FixedPoint.(to_fp amount * fp)

  (* Pretty printing functions *)
  let show amount =
    let zfill s width =
      let to_fill = Stdlib.(width - (String.length s)) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let (upper, lower) = Z.div_rem amount scaling_factor in
    Format.sprintf "%s.%s"
      (Z.to_string upper)
      (zfill (Z.to_string lower) scaling_exponent)

  let pp ppf amount =
    Format.fprintf ppf "%s" (show amount)

  (* Tez UTXO *)
  type utxo = {destination : Address.t ; amount : t}
  [@@deriving show]
end

