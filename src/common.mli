(* Oracle data *)
val oracle_address : Ligo.address
val oracle_entrypoint : string

(* Tezos utilities *)
val level_to_cycle : Ligo.nat -> Ligo.nat
val checker_address : Ligo.address

(* OPERATIONS ON int *)
val min_int : Ligo.int -> Ligo.int -> Ligo.int
val max_int : Ligo.int -> Ligo.int -> Ligo.int

val neg_int : Ligo.int -> Ligo.int
val abs_int : Ligo.int -> Ligo.int

val pow_int_nat : Ligo.int -> Ligo.nat -> Ligo.int

val cdiv_int_int : Ligo.int -> Ligo.int -> Ligo.int
val fdiv_int_int : Ligo.int -> Ligo.int -> Ligo.int

val clamp_int : Ligo.int -> Ligo.int -> Ligo.int -> Ligo.int

(* OPERATIONS ON tez *)
val min_tez : Ligo.tez -> Ligo.tez -> Ligo.tez
val max_tez : Ligo.tez -> Ligo.tez -> Ligo.tez
val tez_to_mutez : Ligo.tez -> Ligo.int

(* BEGIN_OCAML *)
val compare_int : Ligo.int -> Ligo.int -> Int.t
val compare_nat : Ligo.nat -> Ligo.nat -> Int.t
(* END_OCAML *)
