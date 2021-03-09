open Ratio
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
let[@inline] kit_to_mukit (amnt: kit) : Ligo.nat = amnt

let[@inline] kit_to_ratio (amnt: kit) : ratio = make_real_unsafe (Ligo.int amnt) kit_scaling_factor_int

let kit_of_ratio_ceil  (amnt: ratio) : kit =
  if lt_ratio_ratio amnt zero_ratio
  then (failwith "Kit.kit_of_ratio_ceil: negative" : kit)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int amnt.num kit_scaling_factor_int) amnt.den)

let kit_of_ratio_floor (amnt: ratio) : kit =
  if lt_ratio_ratio amnt zero_ratio
  then (failwith "Kit.kit_of_ratio_floor: negative" : kit)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int amnt.num kit_scaling_factor_int) amnt.den)

let[@inline] kit_scale (amnt: kit) (fp: fixedpoint) =
  kit_of_ratio_floor (mul_ratio (fixedpoint_to_ratio fp) (kit_to_ratio amnt))

(* BEGIN_OCAML *)
let kit_compare x y = compare_nat x y

let show_kit amnt = Ligo.string_of_nat amnt ^ "mukit"
let pp_kit ppf amnt = Format.fprintf ppf "%s" (show_kit amnt)
(* END_OCAML *)
