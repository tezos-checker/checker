
(* OPERATIONS ON int *)
let int_min x y = if Ligo.leq_int_int x y then x else y
let int_max x y = if Ligo.geq_int_int x y then x else y

let neg_int x = Ligo.mul_int_int x (Ligo.int_from_literal (-1))
let abs_int x = Ligo.int (Ligo.abs x)

(* OPERATIONS ON tez *)
let tez_min x y = if Ligo.leq_tez_tez x y then x else y
let tez_max x y = if Ligo.geq_tez_tez x y then x else y
let tez_to_mutez x = Ligo.int (Ligo.div_tez_tez x (Ligo.tez_from_mutez_literal 1))
