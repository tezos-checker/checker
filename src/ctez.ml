open Common

type ctez = Ligo.nat

let[@inline] ctez_scaling_factor_int = Ligo.int_from_literal "1_000_000"

(* Basic arithmetic operations. *)
let[@inline] ctez_add (x: ctez) (y: ctez) = Ligo.add_nat_nat x y
let ctez_sub (x: ctez) (y: ctez) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  | None -> (failwith "Ctez.ctez_sub: negative" : ctez)

let[@inline] ctez_zero = Ligo.nat_from_literal "0n"

(* Conversions to/from other types. *)
let[@inline] ctez_of_muctez (amnt: Ligo.nat) : ctez = amnt
let[@inline] ctez_to_muctez_int (amnt: ctez) : Ligo.int = Ligo.int amnt
let[@inline] ctez_to_muctez_nat (amnt: ctez) : Ligo.nat = amnt

let ctez_of_fraction_ceil (x_num: Ligo.int) (x_den: Ligo.int) : ctez =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (failwith "Ctez.ctez_of_fraction_ceil: negative" : ctez)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int x_num ctez_scaling_factor_int) x_den)

let ctez_of_fraction_floor (x_num: Ligo.int) (x_den: Ligo.int) : ctez =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (failwith "Ctez.ctez_of_fraction_floor: negative" : ctez)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int x_num ctez_scaling_factor_int) x_den)

(* BEGIN_OCAML *)
[@@@coverage off]
open Ratio
let ctez_to_ratio (amnt: ctez) : ratio = make_real_unsafe (Ligo.int amnt) ctez_scaling_factor_int
let ratio_of_ctez (x: ctez) : ratio = { num = ctez_to_muctez_int x; den = ctez_scaling_factor_int; }

let ctez_from_tez (tez: Ligo.tez) : ctez = Ligo.abs (tez_to_mutez tez)

let ctez_compare x y = compare_nat x y

let show_ctez amnt = Ligo.string_of_nat amnt ^ "muctez"
let pp_ctez ppf amnt = Format.fprintf ppf "%s" (show_ctez amnt)

[@@@coverage on]
(* END_OCAML *)
