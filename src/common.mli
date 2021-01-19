
(* OPERATIONS ON int *)
val int_min : Ligo.int -> Ligo.int -> Ligo.int
val int_max : Ligo.int -> Ligo.int -> Ligo.int

val neg_int : Ligo.int -> Ligo.int
val abs_int : Ligo.int -> Ligo.int

(* OPERATIONS ON tez *)
val tez_min : Ligo.tez -> Ligo.tez -> Ligo.tez
val tez_max : Ligo.tez -> Ligo.tez -> Ligo.tez
val tez_to_mutez : Ligo.tez -> Ligo.int
