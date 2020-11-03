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
  val of_int : int -> t
  val to_int : t -> int
  val of_rep : Z.t -> t (* NOTE: Exposes internal representation. *)
  val to_rep : t -> Z.t (* NOTE: Exposes internal representation. *)
  val of_string : string -> t

  val exp : t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
  val show : t -> string
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
  let of_int amount = Z.(of_int amount * scaling_factor)
  let to_int amount = Z.(to_int (amount / scaling_factor))

  let of_rep t = t
  let to_rep t = t

  let of_string str =
    let ensure_only_digits s =
      let is_digit c = Char.(code '0' <= code c && code c <= code '9') in
      let check_is_digit c =
        if is_digit c
        then ()
        else failwith (Format.sprintf "FixedPoint.check_is_digit: Unexpected input: %c" c)
      in
      String.iter check_is_digit s ; s
    in
    let take width s =
      if width <= 0 then
        ""
      else if width >= String.length s then
        s
      else
        String.sub s 0 width (* TODO: Warn about ignored digits? *)
    in
    let zfill s width = match Stdlib.(width - (String.length s)) with
      | to_fill when to_fill <= 0 -> s
      | to_fill -> s ^ (String.make to_fill '0')
    in
    let compute_upper u = Z.(of_string u * scaling_factor) in
    let compute_lower l = zfill (take 8 l) 8 |> ensure_only_digits |> Z.of_string in
    let compute_sign u =
      if String.length u = 0 then
        Z.one
      else if String.get u 0 = '-' then
        Z.minus_one
      else
        Z.one
    in
    match String.split_on_char '.' str with
    | [ left ; right ] ->
      let upper = compute_upper left in
      let lower = Z.(compute_lower right * compute_sign left) in
      Z.(upper + lower)
    | [ left ] -> compute_upper left
    | _ -> failwith (Format.sprintf "FixedPoint.of_string: Unexpected input: %s" str)

  (* Pretty printing functions *)
  let show amount =
    let zfill s width =
      let to_fill = Stdlib.(width - (String.length s)) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let abs_amount = Z.abs amount in

    Format.sprintf "%s%s.%s"
      (if amount >= Z.zero then "" else "-")
      (Z.to_string (Z.div abs_amount scaling_factor))
      (zfill (Z.to_string (Z.rem abs_amount scaling_factor)) scaling_exponent)

  let pp ppf amount =
    Format.fprintf ppf "%s" (show amount)
end

