type entrypoint = unit

type address = string
let address_of_string x = Some x
type nat = int
type ratio = nat * nat
type tez = int
let tez_of_nat x = x
let nat_of_int x = if x < 0 then None else Some x
let get_entrypoint address ep = address ^ "%" ^  ep
type call = {amount : nat ; sender : address }
type 't ticket = {issuer : address ; amount : nat ; content : 't}
type payment = {destination : address ; amount : tez}
