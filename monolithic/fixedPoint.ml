(* ************************************************************************* *)
(*                               FixedPoint                                  *)
(* ************************************************************************* *)
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
 * inherently noisy, this is ok. Greater care must be excercised when doing
 * accounting (e.g. uniswap)... for measuring things like drift, targets,
 * imbalances etc which are naturally imprecise this is fine. *)
let ( / ) x y = Z.(x * scaling_factor / y)
let neg x = Z.neg x

let pow x y =
  assert (y >= 0); (* TODO: George: we should allow zero too here, I say. *)
  if y = 0
  then Z.one
  else Z.div (Z.pow x y) (Z.pow scaling_factor Stdlib.(y - 1))

(* NOTE: Use another term from the taylor sequence for more accuracy:
 *   one + amount + (amount * amount) / (one + one) *)
let exp amount = one + amount

(* Conversions to/from other types. *)
let of_int amount = Z.(of_int amount * scaling_factor)
let to_int amount = Z.(to_int (amount / scaling_factor))

let of_rep t = t
let to_rep t = t

let of_string str =
  let without_dot = Str.replace_first (Str.regexp (Str.quote ".")) "" str in
  let dotpos = String.rindex_opt str '.' in
  let mantissa = match dotpos with
    | None -> Z.one
    | Some pos -> Z.pow (Z.of_int 10) Stdlib.(String.length str - pos - 1) in
  Z.((Z.of_string_base 10 without_dot * scaling_factor) / mantissa)

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

let pp ppf amount = Format.fprintf ppf "%s" (show amount)

