type t = Ligo.int

let scaling_base = Ligo.int_from_literal 2
let scaling_exponent = 64
let scaling_factor = Ligo.pow_int_nat scaling_base scaling_exponent

(* Predefined values. *)
let zero = Ligo.int_from_literal 0
let one = scaling_factor

(* Arithmetic operations. *)
let add x y = Ligo.add_int_int x y
let sub x y = Ligo.sub_int_int x y
let mul x y = Ligo.shift_right_trunc_int_nat (Ligo.mul_int_int x y) scaling_exponent

(* We round towards 0, for fixedpoint calculation, measuring things which are
 * inherently noisy, this is ok. Greater care must be excercised when doing
 * accounting (e.g. uniswap)... for measuring things like drift, targets,
 * imbalances etc which are naturally imprecise this is fine. *)
let div x y = Ligo.div_int_int (Ligo.shift_left_int_nat x scaling_exponent) y
let neg x = Ligo.neg_int x

let pow x y =
  assert (y >= 0);
  if y = 0
  then one
  else Ligo.div_int_int (Ligo.pow_int_nat x y) (Ligo.pow_int_nat scaling_factor (y - 1))

(* NOTE: Use another term from the taylor sequence for more accuracy:
 *   one + amount + (amount * amount) / (one + one) *)
let exp amount = add one amount

(* Conversions to/from other types. *)
let of_int amount = Ligo.shift_left_int_nat (Ligo.int_from_literal amount) scaling_exponent

let of_hex_string str =
  let without_dot = Str.replace_first (Str.regexp (Str.quote ".")) "" str in
  let dotpos = String.rindex_opt str '.' in
  let mantissa = match dotpos with
    | None -> Ligo.int_from_literal 1
    | Some pos -> Ligo.pow_int_nat (Ligo.int_from_literal 16) (String.length str - pos - 1) in
  Ligo.div_int_int (Ligo.shift_left_int_nat (Ligo.of_string_base_int 16 without_dot) scaling_exponent) mantissa

let to_ratio amount = Ratio.make amount scaling_factor
let of_ratio_ceil  amount = Ligo.cdiv_int_int (Ligo.shift_left_int_nat (Ratio.num amount) scaling_exponent) (Ratio.den amount)
let of_ratio_floor amount = Ligo.fdiv_int_int (Ligo.shift_left_int_nat (Ratio.num amount) scaling_exponent) (Ratio.den amount)
(* George: do we need flooring-division or truncating-division? more thought is needed *)

(* Pretty printing functions (in hex, otherwise it's massive) *)
let show amount =
  let zfill s width =
    let to_fill = (width - (String.length s)) in
    if to_fill <= 0
    then s
    else (String.make to_fill '0') ^ s in

  let sign = if amount < Ligo.int_from_literal 0 then "-" else "" in
  let (upper, lower) = Ligo.div_rem_int_int (Ligo.abs_int amount) scaling_factor in

  Format.sprintf "%s%s.%s"
    sign
    (Ligo.format_int "%X" upper)
    (zfill (Ligo.format_int "%X" lower) (scaling_exponent / 4))

let pp ppf amount = Format.fprintf ppf "%s" (show amount)

