(* Oracle data *)
let oracle_entrypoint : string = "%getPrice"

(* OPERATIONS ON int *)
let[@inline] int_zero = Ligo.int_from_literal "0"

let min_int (x: Ligo.int) (y: Ligo.int) = if Ligo.leq_int_int x y then x else y
let max_int (x: Ligo.int) (y: Ligo.int) = if Ligo.geq_int_int x y then x else y

let neg_int (x: Ligo.int) = Ligo.sub_int_int (Ligo.int_from_literal "0") x
let abs_int (x: Ligo.int) = Ligo.int (Ligo.abs x)

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
    | None -> (failwith "impossible" : Ligo.int)
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
  | None -> (failwith "Ligo.cdiv_int_int: zero denominator" : Ligo.int)
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
  | None -> (failwith "Ligo.fdiv_int_int: zero denominator" : Ligo.int)
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
let min_tez (x: Ligo.tez) (y: Ligo.tez) = if Ligo.leq_tez_tez x y then x else y
let max_tez (x: Ligo.tez) (y: Ligo.tez) = if Ligo.geq_tez_tez x y then x else y
let tez_to_mutez (x: Ligo.tez) = Ligo.int (Ligo.div_tez_tez x (Ligo.tez_from_literal "1mutez"))

(* OPERATIONS ON nat *)
let min_nat (x: Ligo.nat) (y: Ligo.nat) = if Ligo.leq_nat_nat x y then x else y
let max_nat (x: Ligo.nat) (y: Ligo.nat) = if Ligo.geq_nat_nat x y then x else y

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
[@@@coverage on]
(* END_OCAML *)
