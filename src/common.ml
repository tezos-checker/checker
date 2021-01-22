(* Tezos utilities *)

let level_to_cycle (t : Ligo.nat) = Ligo.div_nat_nat t (Ligo.nat_from_literal "4096n")

(* OPERATIONS ON int *)
let min_int (x: Ligo.int) (y: Ligo.int) = if Ligo.leq_int_int x y then x else y
let max_int (x: Ligo.int) (y: Ligo.int) = if Ligo.geq_int_int x y then x else y

let neg_int (x: Ligo.int) = Ligo.mul_int_int x (Ligo.int_from_literal "-1")
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
    | None -> (failwith "impossible" : Ligo.int)
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

(* OPERATIONS ON tez *)
let min_tez (x: Ligo.tez) (y: Ligo.tez) = if Ligo.leq_tez_tez x y then x else y
let max_tez (x: Ligo.tez) (y: Ligo.tez) = if Ligo.geq_tez_tez x y then x else y
let tez_to_mutez (x: Ligo.tez) = Ligo.int (Ligo.div_tez_tez x (Ligo.tez_from_literal "1mutez"))

(* BEGIN_OCAML *)
let compare_int (i: Ligo.int) (j: Ligo.int) : Int.t =
  if Ligo.gt_int_int i j then
    1
  else if Ligo.eq_int_int i j then
    0
  else
    -1
(* END_OCAML *)
