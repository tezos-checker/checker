open Error

(* OPERATIONS ON int *)
let[@inline] int_zero = Ligo.int_from_literal "0"

let min_int (x: Ligo.int) (y: Ligo.int) = if Ligo.leq_int_int x y then x else y
let max_int (x: Ligo.int) (y: Ligo.int) = if Ligo.geq_int_int x y then x else y

let neg_int (x: Ligo.int) = Ligo.sub_int_int (Ligo.int_from_literal "0") x

(* Note that ligo is not happy with nested lets. Take out when ready, but
 * keep internal for now. *)
let rec pow_rec (y, x, n: Ligo.int * Ligo.int * Ligo.nat) : Ligo.int =
  if Ligo.eq_nat_nat n (Ligo.nat_from_literal "0n") then
    y
  else if Ligo.eq_nat_nat n (Ligo.nat_from_literal "1n") then
    Ligo.mul_int_int x y
  else
    match Ligo.ediv_nat_nat n (Ligo.nat_from_literal "2n") with
    (* Note: Ignoring coverage for this line because it is unreachable *)
    | None -> (Ligo.failwith internalError_PowRecImpossible : Ligo.int)
              [@coverage off]
    | Some quot_rem ->
      let (quot, rem) = quot_rem in
      if Ligo.eq_nat_nat rem (Ligo.nat_from_literal "0n") then
        pow_rec (y, Ligo.mul_int_int x x, quot)
      else
        pow_rec (Ligo.mul_int_int x y, Ligo.mul_int_int x x, quot)

let pow_int_nat (x: Ligo.int) (n: Ligo.nat) = pow_rec (Ligo.int_from_literal "1", x, n)

let cdiv_int_int (x: Ligo.int) (y: Ligo.int) =
  match Ligo.ediv_int_int x y with
  | None -> (Ligo.failwith internalError_CdivIntIntZeroDenominator : Ligo.int)
  | Some quot_rem ->
    let (quot, rem) = quot_rem in
    if Ligo.eq_nat_nat rem (Ligo.nat_from_literal "0n") then
      quot
    else if Ligo.lt_int_int y (Ligo.int_from_literal "0") then
      quot
    else
      Ligo.add_int_int quot (Ligo.int_from_literal "1")

let fdiv_int_int (x: Ligo.int) (y: Ligo.int) =
  match Ligo.ediv_int_int x y with
  | None -> (Ligo.failwith internalError_FdivIntIntZeroDenominator : Ligo.int)
  | Some quot_rem ->
    let (quot, rem) = quot_rem in
    if Ligo.eq_nat_nat rem (Ligo.nat_from_literal "0n") then
      quot
    else if Ligo.gt_int_int y (Ligo.int_from_literal "0") then
      quot
    else
      Ligo.sub_int_int quot (Ligo.int_from_literal "1")

let clamp_int (v: Ligo.int) (lower: Ligo.int) (upper: Ligo.int) : Ligo.int =
  assert (Ligo.leq_int_int lower upper);
  min_int upper (max_int v lower)

(* OPERATIONS ON tez *)
let[@inline] tez_to_mutez_nat (amnt: Ligo.tez) = Ligo.div_tez_tez amnt (Ligo.tez_from_literal "1mutez")

let tez_to_mutez (x: Ligo.tez) = Ligo.int (tez_to_mutez_nat x)

let tez_scaling_factor_int : Ligo.int = Ligo.int_from_literal "1_000_000"
let tez_scaling_factor_nat : Ligo.nat = Ligo.nat_from_literal "1_000_000n"

(* OPERATIONS ON nat *)
let min_nat (x: Ligo.nat) (y: Ligo.nat) = if Ligo.leq_nat_nat x y then x else y
let max_nat (x: Ligo.nat) (y: Ligo.nat) = if Ligo.geq_nat_nat x y then x else y

(* RATIOS AND OPERATIONS ON THEM *)

(** A rational is represented as a pair numerator/denominator, reduced to have
  * a positive denominator. This form is canonical. *)
type ratio = {
  num: Ligo.int; (** Numerator. *)
  den: Ligo.int; (** Denominator, > 0 *)
}

(* The denominator must be positive. *)
let[@inline] make_ratio (n: Ligo.int) (d: Ligo.int) : ratio =
  assert (Ligo.gt_int_int d (Ligo.int_from_literal "0"));
  { num = n; den = d; }

(* zero: 0/1 *)
let[@inline] zero_ratio : ratio = { num = Ligo.int_from_literal "0"; den = Ligo.int_from_literal "1"; }

(* one: 1/1 *)
let[@inline] one_ratio : ratio = { num = Ligo.int_from_literal "1"; den = Ligo.int_from_literal "1"; }

(* Floor a fraction to an amount of tez. NOTE: this function deals in tez, not
 * in mutez. So, for example, fraction_to_tez_floor (1/1) = 1tez and
 * fraction_to_tez_floor (1/3) = 333_333mutez. *)
let fraction_to_tez_floor (x_num: Ligo.int) (x_den: Ligo.int) : Ligo.tez =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  match Ligo.is_nat x_num with
  | None -> (Ligo.failwith internalError_FractionToTezFloorNegative : Ligo.tez)
  | Some n ->
    let n = Ligo.mul_nat_nat n tez_scaling_factor_nat in
    let d = Ligo.abs x_den in
    (match Ligo.ediv_nat_nat n d with
     (* Note: Ignoring coverage for the case below since the assertion above makes it unreachable in OCaml *)
     | None -> (Ligo.failwith internalError_FractionToTezFloorZeroDenominator : Ligo.tez)
               [@coverage off]
     | Some quot_and_rem ->
       let (quot, _) = quot_and_rem in
       Ligo.mul_nat_tez quot (Ligo.tez_from_literal "1mutez") (* ignore the remainder; we floor towards zero here *)
    )

(* Floor a fraction to a natural number. *)
let fraction_to_nat_floor (x_num: Ligo.int) (x_den: Ligo.int) : Ligo.nat =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  match Ligo.is_nat x_num with
  | None -> (Ligo.failwith internalError_FractionToNatFloorNegative : Ligo.nat)
  | Some n ->
    (match Ligo.ediv_nat_nat n (Ligo.abs x_den) with
     (* Note: Ignoring coverage for the case below since the assertion above makes it unreachable in OCaml *)
     | None -> (Ligo.failwith internalError_FractionToNatFloorZeroDenominator : Ligo.nat)
               [@coverage off]
     | Some quot_and_rem ->
       let (quot, _) = quot_and_rem in
       quot (* ignore the remainder; we floor towards zero here *)
    )

(* Ensure that there is no tez given. To prevent accidental fund loss. *)
let ensure_no_tez_given () =
  if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez"
  then Ligo.failwith error_UnwantedTezGiven
  else ()

(* BEGIN_OCAML *)
[@@@coverage off]
let compare_int (i: Ligo.int) (j: Ligo.int) : Int.t =
  if Ligo.gt_int_int i j then
    1
  else if Ligo.eq_int_int i j then
    0
  else
    -1

let compare_nat (i: Ligo.nat) (j: Ligo.nat) : Int.t =
  if Ligo.gt_nat_nat i j then
    1
  else if Ligo.eq_nat_nat i j then
    0
  else
    -1

let show_ratio n = (Ligo.string_of_int n.num) ^ "/" ^ (Ligo.string_of_int n.den)
let pp_ratio f x = Format.pp_print_string f (show_ratio x)
[@@@coverage on]
(* END_OCAML *)
