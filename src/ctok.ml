open Common
open Error

type ctok = Ligo.nat

let[@inline] ctok_scaling_factor_int = Ligo.int_from_literal "1_000_000"

(* Basic arithmetic operations. *)
let[@inline] ctok_add (x: ctok) (y: ctok) = Ligo.add_nat_nat x y
let ctok_sub (x: ctok) (y: ctok) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  | None -> (Ligo.failwith internalError_CtokSubNegative : ctok)

let[@inline] ctok_zero = Ligo.nat_from_literal "0n"

(* Conversions to/from other types. *)
let[@inline] ctok_of_muctok (amnt: Ligo.nat) : ctok = amnt
let[@inline] ctok_to_muctok_nat (amnt: ctok) : Ligo.nat = amnt

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

let[@inline] eq_ctok_ctok = Ligo.eq_nat_nat
let[@inline] lt_ctok_ctok = Ligo.lt_nat_nat
let[@inline] gt_ctok_ctok = Ligo.gt_nat_nat

(* BEGIN_OCAML *)
[@@@coverage off]
let[@inline] ctok_to_muctok_int (amnt: ctok) : Ligo.int = Ligo.int amnt
let ctok_to_ratio (amnt: ctok) : ratio = make_ratio (Ligo.int amnt) ctok_scaling_factor_int
let ratio_of_ctok (x: ctok) : ratio = { num = ctok_to_muctok_int x; den = ctok_scaling_factor_int; }

let ctok_from_tez (tez: Ligo.tez) : ctok = Ligo.abs (tez_to_mutez tez)

let ctok_compare x y = compare_nat x y

let show_ctok amnt = Ligo.string_of_nat amnt ^ "muctok"
let pp_ctok ppf amnt = Format.fprintf ppf "%s" (show_ctok amnt)

[@@@coverage on]
(* END_OCAML *)
