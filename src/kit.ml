open Common
open FixedPoint

type kit = Ligo.nat
let[@inline] kit_scaling_factor_int = Ligo.int_from_literal "1_000_000"
let[@inline] kit_scaling_factor_nat = Ligo.nat_from_literal "1_000_000n"

(* Basic arithmetic operations. *)
let[@inline] kit_add (x: kit) (y: kit) = Ligo.add_nat_nat x y
let kit_sub (x: kit) (y: kit) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  | None -> (failwith "Kit.kit_sub: negative" : kit)

let[@inline] kit_min (x: kit) (y: kit) = if Ligo.leq_nat_nat x y then x else y
let[@inline] kit_max (x: kit) (y: kit) = if Ligo.geq_nat_nat x y then x else y

let[@inline] kit_zero = Ligo.nat_from_literal "0n"
let[@inline] kit_one = kit_scaling_factor_nat

(* Conversions to/from other types. *)
let[@inline] kit_of_mukit (amnt: Ligo.nat) : kit = amnt
let[@inline] kit_to_mukit_int (amnt: kit) : Ligo.int = Ligo.int amnt
let[@inline] kit_to_mukit_nat (amnt: kit) : Ligo.nat = amnt

let kit_of_fraction_ceil (x_num: Ligo.int) (x_den: Ligo.int) : kit =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (failwith "Kit.kit_of_fraction_ceil: negative" : kit)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int x_num kit_scaling_factor_int) x_den)

let kit_of_fraction_floor (x_num: Ligo.int) (x_den: Ligo.int) : kit =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (failwith "Kit.kit_of_fraction_floor: negative" : kit)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int x_num kit_scaling_factor_int) x_den)

let[@inline] kit_scale (amnt: kit) (fp: fixedpoint) =
  kit_of_fraction_floor
    (Ligo.mul_int_int (fixedpoint_to_raw fp) (Ligo.int amnt))
    (Ligo.mul_int_int fixedpoint_scaling_factor kit_scaling_factor_int)

(* BEGIN_OCAML *)
open Ratio
let[@inline] kit_to_ratio (amnt: kit) : ratio = make_real_unsafe (Ligo.int amnt) kit_scaling_factor_int

let kit_compare x y = compare_nat x y

let show_kit amnt = Ligo.string_of_nat amnt ^ "mukit"
let pp_kit ppf amnt = Format.fprintf ppf "%s" (show_kit amnt)
(* END_OCAML *)
