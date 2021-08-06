open Common
open FixedPoint
open Error

type kit = Ligo.nat

let[@inline] kit_decimal_digits = Ligo.nat_from_literal "6n"
let[@inline] kit_scaling_factor_int = Ligo.int_from_literal "1_000_000"
let[@inline] kit_scaling_factor_nat = Ligo.nat_from_literal "1_000_000n"

(* Basic arithmetic operations. *)
let[@inline] kit_add (x: kit) (y: kit) = Ligo.add_nat_nat x y
let kit_sub (x: kit) (y: kit) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  | None -> (Ligo.failwith internalError_KitSubNegative : kit)

let[@inline] kit_min (x: kit) (y: kit) = if Ligo.leq_nat_nat x y then x else y
let[@inline] kit_max (x: kit) (y: kit) = if Ligo.geq_nat_nat x y then x else y

let[@inline] kit_zero = Ligo.nat_from_literal "0n"
let[@inline] kit_one = kit_scaling_factor_nat

(* Conversions to/from other types. *)
let[@inline] kit_of_denomination (amnt: Ligo.nat) : kit = amnt
let[@inline] kit_to_denomination_int (amnt: kit) : Ligo.int = Ligo.int amnt
let[@inline] kit_to_denomination_nat (amnt: kit) : Ligo.nat = amnt

let kit_of_fraction_ceil (x_num: Ligo.int) (x_den: Ligo.int) : kit =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (Ligo.failwith internalError_KitOfFractionCeilNegative : kit)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int x_num kit_scaling_factor_int) x_den)

let kit_of_fraction_floor (x_num: Ligo.int) (x_den: Ligo.int) : kit =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (Ligo.failwith internalError_KitOfFractionFloorNegative : kit)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int x_num kit_scaling_factor_int) x_den)

let[@inline] kit_scale (amnt: kit) (fp: fixedpoint) =
  kit_of_fraction_floor
    (Ligo.mul_int_nat (fixedpoint_to_raw fp) amnt)
    (Ligo.mul_int_int fixedpoint_scaling_factor kit_scaling_factor_int)

let[@inline] geq_kit_kit = Ligo.geq_nat_nat

let[@inline] lt_kit_kit = Ligo.lt_nat_nat
let[@inline] gt_kit_kit = Ligo.gt_nat_nat

let[@inline] eq_kit_kit = Ligo.eq_nat_nat

(* BEGIN_OCAML *)
[@@@coverage off]
let[@inline] kit_to_ratio (amnt: kit) : ratio = make_ratio (Ligo.int amnt) kit_scaling_factor_int

let kit_compare x y = compare_nat x y

let show_kit amnt = Ligo.string_of_nat amnt ^ "mukit"
let pp_kit ppf amnt = Format.fprintf ppf "%s" (show_kit amnt)

[@@@coverage on]
(* END_OCAML *)
