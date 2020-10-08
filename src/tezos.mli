type address
type entrypoint

val address_of_string : string -> address option
val get_entrypoint : address -> string -> address

type nat
type tez

val tez_of_nat : nat -> tez
val nat_of_int : int -> nat option

type ratio

(** Data available in a Tezos call, the amount sent and the sender. *)
type call = {amount : tez ; sender : address }

(** A ticket, a new proposed feature in Michelson. *)
type 't ticket = {issuer : address ; amount : nat ; content : 't}

(** A transaction coming out of some contract and paying tez to some address *)
type payment = {destination : address ; amount : tez}
