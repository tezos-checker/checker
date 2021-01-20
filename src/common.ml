
(* OPERATIONS ON int *)
let int_min x y = if Ligo.leq_int_int x y then x else y
let int_max x y = if Ligo.geq_int_int x y then x else y

let neg_int x = Ligo.mul_int_int x (Ligo.int_from_literal (-1))
let abs_int x = Ligo.int (Ligo.abs x)

let pow_int_nat x n =
  (* Note that ligo is not happy with nested lets. Take out when ready, but
   * keep internal for now. *)
  let rec pow_rec y x n =
    if Ligo.eq_nat_nat n (Ligo.nat_from_literal 0) then
      y
    else if Ligo.eq_nat_nat n (Ligo.nat_from_literal 1) then
      Ligo.mul_int_int x y
    else
      match Ligo.ediv_nat_nat n (Ligo.nat_from_literal 2) with
      | None -> (failwith "impossible" : Ligo.int)
      | Some (quot, rem) ->
        if Ligo.eq_nat_nat rem (Ligo.nat_from_literal 0) then
          pow_rec y (Ligo.mul_int_int x x) quot
        else
          pow_rec (Ligo.mul_int_int x y) (Ligo.mul_int_int x x) quot
  in
  pow_rec (Ligo.int_from_literal 1) x n

(* OPERATIONS ON tez *)
let tez_min x y = if Ligo.leq_tez_tez x y then x else y
let tez_max x y = if Ligo.geq_tez_tez x y then x else y
let tez_to_mutez x = Ligo.int (Ligo.div_tez_tez x (Ligo.tez_from_mutez_literal 1))
