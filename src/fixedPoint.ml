open Ratio
open Common

type fixedpoint = Ligo.int

let fixedpoint_scaling_factor = Ligo.int_from_literal "18446744073709551616" (* 2 (scaling_base) ^ 64 (scaling_exponent) *)

(* Predefined values. *)
let[@inline] fixedpoint_zero = Ligo.int_from_literal "0"
let[@inline] fixedpoint_one = fixedpoint_scaling_factor

(* Arithmetic operations. *)
let[@inline] fixedpoint_add (x: fixedpoint) (y: fixedpoint) = Ligo.add_int_int x y
let[@inline] fixedpoint_sub (x: fixedpoint) (y: fixedpoint) = Ligo.sub_int_int x y

let fixedpoint_pow (x: fixedpoint) (y: Ligo.nat) =
  if Ligo.eq_nat_nat y (Ligo.nat_from_literal "0n") then
    fixedpoint_one
  else
    Ligo.div_int_int
      (pow_int_nat x y)
      (pow_int_nat fixedpoint_scaling_factor (Ligo.abs (Ligo.sub_int_int (Ligo.int y) (Ligo.int_from_literal "1"))))

(* Conversions to/from other types. *)
let fixedpoint_of_ratio_ceil  (amnt: ratio) = cdiv_int_int (Ligo.mul_int_int amnt.num fixedpoint_scaling_factor) amnt.den
let fixedpoint_of_ratio_floor (amnt: ratio) = fdiv_int_int (Ligo.mul_int_int amnt.num fixedpoint_scaling_factor) amnt.den
(* George: do we need flooring-division or truncating-division? more thought is needed *)

let[@inline] fixedpoint_of_raw (amnt: Ligo.int) : fixedpoint = amnt
let[@inline] fixedpoint_to_raw (amnt: fixedpoint) : Ligo.int = amnt

(* BEGIN_OCAML *)
[@@@coverage off]
let fixedpoint_scaling_exponent = 64

let fixedpoint_to_ratio (amnt: fixedpoint) = make_real_unsafe amnt fixedpoint_scaling_factor

let fixedpoint_of_hex_string str =
  let without_dot = Str.replace_first (Str.regexp (Str.quote ".")) "" str in
  let dotpos = String.rindex_opt str '.' in
  let mantissa = match dotpos with
    | None -> Ligo.int_from_literal "1"
    | Some pos -> pow_int_nat (Ligo.int_from_literal "16") (Ligo.abs (Ligo.int_from_literal (string_of_int (String.length str - pos - 1)))) in
  Ligo.div_int_int (Ligo.mul_int_int (Ligo.of_string_base_int 16 without_dot) fixedpoint_scaling_factor) mantissa

let show_fixedpoint amnt =
  let zfill s width =
    let to_fill = (width - (String.length s)) in
    if to_fill <= 0
    then s
    else (String.make to_fill '0') ^ s in

  let sign = if amnt < Ligo.int_from_literal "0" then "-" else "" in
  let (upper, lower) = Ligo.div_rem_int_int (abs_int amnt) fixedpoint_scaling_factor in

  (* in hex, otherwise it's massive *)
  Format.sprintf "%s%s.%s"
    sign
    (Ligo.format_int "%X" upper)
    (zfill (Ligo.format_int "%X" lower) (fixedpoint_scaling_exponent / 4))

let pp_fixedpoint ppf amnt = Format.fprintf ppf "%s" (show_fixedpoint amnt)

[@@@coverage on]
(* END_OCAML *)
