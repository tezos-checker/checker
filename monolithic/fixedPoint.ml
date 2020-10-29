(* ************************************************************************* *)
(*                               FixedPoint                                  *)
(* ************************************************************************* *)
module FixedPoint : sig
  type t

  val scaling_factor : Z.t
  val scaling_factor_int64 : Int64.t (* TODO: Remove; only for compatibility with kit/tez right now *)

  (* Basic arithmetic operations. *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val neg : t -> t
  val sqr : t -> t (* TODO: Generalize, if needed *)

  val zero : t
  val one : t

  (* Conversions to/from other types. *)
  val of_float : float -> t (* TODO: Delete this one. *)
  val to_float : t -> float (* TODO: Delete this one. *)

  (* CAUTION: These expose the internal representation. *)
  val of_z : Z.t -> t (* TODO: Remove; only for compatibility with kit/tez right now *)
  val to_z : t -> Z.t (* TODO: Remove; only for compatibility with kit/tez right now *)

  val of_int64 : int64 -> t (* TODO: Remove; only for compatibility with kit/tez right now *)
  val to_int64 : t -> int64 (* TODO: Remove; only for compatibility with kit/tez right now *)

  val exp : t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
end =
struct
  type t = Z.t
  let scaling_factor = Z.of_int64 100000000L
  let scaling_exponent = 8
  let scaling_factor_int64 = Z.to_int64 scaling_factor

  (* Basic arithmetic operations. *)
  let ( + ) x y = Z.(x + y)
  let ( - ) x y = Z.(x - y)
  let ( * ) x y = Z.((x * y) / scaling_factor)
  let ( / ) x y =
    let upper, lower = Z.div_rem x y in
    Z.((upper * scaling_factor) + ((lower * scaling_factor) / y))

  let neg x = Z.neg x
  let sqr x = x * x

  let zero = Z.zero
  let one = scaling_factor

  (* Conversions to/from other types. *)
  let of_float x = (* TODO: lossy *)
    (* TODO: Assertions here *)
    let sign = if (x >= 0.0) then Z.one else Z.minus_one in
    let amount = Float.abs x in
    let upper = Z.of_float amount in
    let lower = Z.of_float ((amount -. Z.to_float upper) *. Z.to_float scaling_factor) in
    Z.mul sign (Z.add (Z.mul upper scaling_factor) lower)

  let to_float amount = (* TODO: lossy *)
    (Z.to_float amount) /. Z.to_float scaling_factor

  let of_int64 t = Z.of_int64 t
  let to_int64 t = Z.to_int64 t

  let of_z t = t
  let to_z t = t

  let exp amount = one + amount
  (* Note: Use another term from the taylor sequence for more accuracy:
       one + amount + (amount * amount) / (one + one)
  *)

  (* Pretty printing functions *)
  let pp ppf amount =
    let zfill s width =
      let to_fill = Stdlib.(width - (String.length s)) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let abs_amount = Z.abs amount in

    Format.fprintf ppf "%s%s.%s"
      (if amount >= Z.zero then "" else "-")
      (Z.to_string (Z.div abs_amount scaling_factor))
      (zfill (Z.to_string (Z.rem abs_amount scaling_factor)) scaling_exponent)
end

