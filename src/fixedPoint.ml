type t = Ligo.int

(* let scaling_base = Ligo.int_from_literal 2 *)
let scaling_exponent = 64
let scaling_factor = Ligo.int_from_literal "18446744073709551616" (* 2 (scaling_base) ^ 64 (scaling_exponent) *)

(* Predefined values. *)
let zero = Ligo.int_from_literal "0"
let one = scaling_factor

(* Arithmetic operations. *)
let add x y = Ligo.add_int_int x y
let sub x y = Ligo.sub_int_int x y
let mul x y = Ligo.div_int_int (Ligo.mul_int_int x y) scaling_factor

(* We round towards 0, for fixedpoint calculation, measuring things which are
 * inherently noisy, this is ok. Greater care must be excercised when doing
 * accounting (e.g. uniswap)... for measuring things like drift, targets,
 * imbalances etc which are naturally imprecise this is fine. *)
let div x y = Ligo.div_int_int (Ligo.mul_int_int x scaling_factor) y
let neg x = Common.neg_int x

let pow x y =
  if Ligo.eq_nat_nat y (Ligo.nat_from_literal "0n") then
    one
  else
    Ligo.div_int_int
      (Common.pow_int_nat x y)
      (Common.pow_int_nat scaling_factor (Ligo.abs (Ligo.sub_int_int (Ligo.int y) (Ligo.int_from_literal "1"))))

(* NOTE: Use another term from the taylor sequence for more accuracy:
 *   one + amount + (amount * amount) / (one + one) *)
let exp amount = add one amount

(* Conversions to/from other types. *)
let of_int amount = Ligo.mul_int_int amount scaling_factor

let of_hex_string str =
  let without_dot = Str.replace_first (Str.regexp (Str.quote ".")) "" str in
  let dotpos = String.rindex_opt str '.' in
  let mantissa = match dotpos with
    | None -> Ligo.int_from_literal "1"
    | Some pos -> Common.pow_int_nat (Ligo.int_from_literal "16") (Ligo.abs (Ligo.int_from_literal (string_of_int (String.length str - pos - 1)))) in (* FIXME: NOT LEGITIMATE *)
  Ligo.div_int_int (Ligo.mul_int_int (Ligo.of_string_base_int 16 without_dot) scaling_factor) mantissa

let to_ratio amount = Ratio.make amount scaling_factor
let of_ratio_ceil  amount = Common.cdiv_int_int (Ligo.mul_int_int (Ratio.num amount) scaling_factor) (Ratio.den amount)
let of_ratio_floor amount = Common.fdiv_int_int (Ligo.mul_int_int (Ratio.num amount) scaling_factor) (Ratio.den amount)
(* George: do we need flooring-division or truncating-division? more thought is needed *)

(* BEGIN_OCAML *)
let show amount =
  let zfill s width =
    let to_fill = (width - (String.length s)) in
    if to_fill <= 0
    then s
    else (String.make to_fill '0') ^ s in

  let sign = if amount < Ligo.int_from_literal "0" then "-" else "" in
  let (upper, lower) = Ligo.div_rem_int_int (Common.abs_int amount) scaling_factor in

  (* in hex, otherwise it's massive *)
  Format.sprintf "%s%s.%s"
    sign
    (Ligo.format_int "%X" upper)
    (zfill (Ligo.format_int "%X" lower) (scaling_exponent / 4))

let pp ppf amount = Format.fprintf ppf "%s" (show amount)
(* END_OCAML *)
