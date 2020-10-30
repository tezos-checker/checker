(* ************************************************************************* *)
(*                               FixedPoint                                  *)
(* ************************************************************************* *)
module FixedPoint : sig
  type t

  val scaling_factor : Z.t

  (* Predefined values. *)
  val zero : t
  val one : t

  (* Arithmetic operations. *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val neg : t -> t
  val sqr : t -> t (* TODO: Generalize, if needed *)

  (* Conversions to/from other types. *)
  val of_float : float -> t (* TODO: Delete this one. *)
  val of_int : int -> t
  val to_int : t -> int
  val of_rep : Z.t -> t (* NOTE: Exposes internal representation. *)
  val to_rep : t -> Z.t (* NOTE: Exposes internal representation. *)

  val exp : t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
end =
struct
  type t = Z.t
  let scaling_factor = Z.of_int64 100000000L
  let scaling_exponent = 8

  (* Predefined values. *)
  let zero = Z.zero
  let one = scaling_factor

  (* Arithmetic operations. *)
  let ( + ) x y = Z.(x + y)
  let ( - ) x y = Z.(x - y)
  let ( * ) x y = Z.((x * y) / scaling_factor)

  (* We round towards 0, for fixedpoint calculation, measuring things which are
   * inherently noisy, this is ok. Greater care must be excerced when doing
   * accounting (e.g. uniswap)... for measuring things like drift, targets,
   * imbalances etc which are naturally imprecise this is fine. *)
  let ( / ) x y = Z.(x * scaling_factor / y)
  let neg x = Z.neg x
  let sqr x = x * x

  (* NOTE: Use another term from the taylor sequence for more accuracy:
   *   one + amount + (amount * amount) / (one + one) *)
  let exp amount = one + amount

  (* Conversions to/from other types. *)
  let of_float x = (* TODO: lossy *)
    let sign = if (x >= 0.0) then Z.one else Z.minus_one in
    let amount = Float.abs x in
    let upper = Z.of_float amount in
    let lower = Z.of_float ((amount -. Z.to_float upper) *. Z.to_float scaling_factor) in
    Z.mul sign (Z.add (Z.mul upper scaling_factor) lower)

  let of_int amount = Z.(of_int amount * scaling_factor)
  let to_int amount = Z.(to_int (amount / scaling_factor))

  let of_rep t = t
  let to_rep t = t

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

