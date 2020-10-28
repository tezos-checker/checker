(* TODO: Switch to Q from "zarith" *)
(* ************************************************************************* *)
(*                               FixedPoint                                  *)
(* ************************************************************************* *)
module FixedPoint : sig
  type t (* Invariant: >= zero *)

  val scaling_factor : Int64.t

  (* Basic arithmetic operations. TODO: delete division, or at least limit it. *)
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
  val of_int64 : int64 -> t
  val to_int64 : t -> int64

  val exp : t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
end =
struct
  type t = Int64.t
  let scaling_factor = 100000000L
  let scaling_exponent = 8

  (* Basic arithmetic operations. *)
  let ( + ) x y =
    if (x >= 0L && y <= 0L) || (x <= 0L && y >= 0L) then
      Int64.add x y
    else if (x >= 0L && y >= 0L) then
      (assert (not (x > Int64.sub Int64.max_int y)); (* Overflow *)
       assert (not (y > Int64.sub Int64.max_int x)); (* Overflow *)
       Int64.add x y)
    else (* (x <= 0 && y <= 0) *)
      (assert (not (x < Int64.sub Int64.min_int y)); (* Underflow *)
       assert (not (y < Int64.sub Int64.min_int x)); (* Underflow *)
       Int64.add x y)

  let ( - ) x y =
    assert (y <> Int64.min_int); (* Overflow *)
    if (x >= 0L && y >= 0L) || (x <= 0L && y <= 0L) then
      Int64.sub x y
    else
      x + (Int64.neg y)

  let ( * ) x y = (* TODO: lossy *)
    if (x = 0L || y = 0L) then
      0L
    else
      (assert (x <> Int64.min_int && y <> Int64.min_int);
       let sign =
         if (x > 0L && y > 0L) || (x < 0L && y < 0L)
         then Int64.one
         else Int64.minus_one
       in
       let absx = Int64.abs x in
       let absy = Int64.abs y in
       (assert (not (absx > Int64.div Int64.max_int absy)); (* Overflow *)
        assert (not (absy > Int64.div Int64.max_int absx)); (* Overflow *)
        Int64.mul sign (Int64.div (Int64.mul absx absy) scaling_factor)))

  let ( / ) x y = (* TODO: lossy *)
    (assert (y <> 0L); (* Overflow/Underflow *)
     assert (x <> Int64.min_int && y <> Int64.min_int); (* TODO *)
     let sign =
       if (x > 0L && y > 0L) || (x < 0L && y < 0L)
       then Int64.one
       else Int64.minus_one
     in
     let absx = Int64.abs x in
     let absy = Int64.abs y in
     let upper = Int64.div absx absy in
     let lower = Int64.div (Int64.mul (Int64.rem absx absy) scaling_factor) absy in
     Int64.mul sign (Int64.add (Int64.mul upper scaling_factor) lower))

  let neg x = assert (x <> Int64.min_int); Int64.neg x

  let sqr x = x * x

  let zero = 0L
  let one = scaling_factor

  (* Conversions to/from other types. *)
  let of_float x = (* TODO: lossy *)
    (* TODO: Assertions here *)
    let sign = if (x >= 0.0) then Int64.one else Int64.minus_one in
    let amount = Float.abs x in
    let upper = Int64.of_float amount in
    let lower = Int64.of_float ((amount -. Int64.to_float upper) *. Int64.to_float scaling_factor) in
    Int64.mul sign (Int64.add (Int64.mul upper scaling_factor) lower)

  let to_float amount = (* TODO: lossy *)
    (Int64.to_float amount) /. Int64.to_float scaling_factor

  let of_int64 t = t
  let to_int64 t = t

  let exp amount = one + amount
  (* Note: Use another term from the taylor sequence for more accuracy:
       one + amount + (amount * amount) / (one + one)
  *)

  (* Pretty printing functions *)
  let pp ppf amount =
    assert (amount <> Int64.min_int); (* TODO *)
    let zfill s width =
      let to_fill = Stdlib.(width - (String.length s)) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let abs_amount = Int64.abs amount in

    Format.fprintf ppf "%s%s.%s"
      (if amount >= 0L then "" else "-")
      (Int64.to_string (Int64.div abs_amount scaling_factor))
      (zfill (Int64.to_string (Int64.rem abs_amount scaling_factor)) scaling_exponent)
end

