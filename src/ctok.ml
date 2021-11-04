

open Common
open Error
open Fa2Interface
open FixedPoint

type ctok = Ligo.nat

(* FA2 token id for this token *)
let[@inline] ctok_token_id  : fa2_token_id = Ligo.nat_from_literal "0n" (* FIXME: We don't issue this one. *)

let[@inline] ctok_decimal_digits = Ligo.nat_from_literal "6n"
let[@inline] ctok_scaling_factor_int = Ligo.int_from_literal "1000000"
let[@inline] ctok_scaling_factor_nat = Ligo.nat_from_literal "1000000n"

(* Basic arithmetic operations. *)
let[@inline] ctok_add (x: ctok) (y: ctok) = Ligo.add_nat_nat x y
let ctok_sub (x: ctok) (y: ctok) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  (* FIXME: Need unique errors per token type *)
  | None -> (Ligo.failwith internalError_CtokSubNegative : ctok)

let[@inline] ctok_min (x: ctok) (y: ctok) = if Ligo.leq_nat_nat x y then x else y
let[@inline] ctok_max (x: ctok) (y: ctok) = if Ligo.geq_nat_nat x y then x else y

let[@inline] ctok_zero = Ligo.nat_from_literal "0n"
let[@inline] ctok_one = ctok_scaling_factor_nat

(* Conversions to/from other types. *)
let[@inline] ctok_of_denomination (amnt: Ligo.nat) : ctok = amnt
let[@inline] ctok_to_denomination_int (amnt: ctok) : Ligo.int = Ligo.int amnt
let[@inline] ctok_to_denomination_nat (amnt: ctok) : Ligo.nat = amnt

let ctok_of_fraction_ceil (x_num: Ligo.int) (x_den: Ligo.int) : ctok =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (Ligo.failwith internalError_CtokOfFractionCeilNegative : ctok)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int x_num ctok_scaling_factor_int) x_den)

let ctok_of_fraction_floor (x_num: Ligo.int) (x_den: Ligo.int) : ctok =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (Ligo.failwith internalError_CtokOfFractionFloorNegative : ctok)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int x_num ctok_scaling_factor_int) x_den)

let[@inline] ctok_scale (amnt: ctok) (fp: fixedpoint) =
  ctok_of_fraction_floor
    (Ligo.mul_int_nat (fixedpoint_to_raw fp) amnt)
    (Ligo.mul_int_int fixedpoint_scaling_factor ctok_scaling_factor_int)

let[@inline] geq_ctok_ctok = Ligo.geq_nat_nat
let[@inline] leq_ctok_ctok = Ligo.leq_nat_nat

let[@inline] lt_ctok_ctok = Ligo.lt_nat_nat
let[@inline] gt_ctok_ctok = Ligo.gt_nat_nat

let[@inline] eq_ctok_ctok = Ligo.eq_nat_nat

(* BEGIN_OCAML *)
[@@@coverage off]
let[@inline] ctok_to_ratio (amnt: ctok) : ratio = make_ratio (Ligo.int amnt) ctok_scaling_factor_int

let ctok_compare x y = compare_nat x y

let show_ctok amnt =
  let zfill s width = match Stdlib.(width - (String.length s)) with
    | to_fill when to_fill <= 0 -> s
    | to_fill -> (String.make to_fill '0') ^ s
  in
  let as_string =
    if ctok_decimal_digits = Ligo.nat_from_literal "0n" then
      Ligo.string_of_nat amnt
    else
      let d, r = Option.get (Ligo.ediv_nat_nat amnt ctok_scaling_factor_nat) in
      let ctok_decimal_digits = Stdlib.int_of_string (Ligo.string_of_nat ctok_decimal_digits) in (* little hacky *)
      (Ligo.string_of_nat d) ^ "." ^ zfill (Ligo.string_of_nat r) ctok_decimal_digits
  in as_string ^ "ctok"

let pp_ctok ppf amnt = Format.fprintf ppf "%s" (show_ctok amnt)

[@@@coverage on]
(* END_OCAML *)
