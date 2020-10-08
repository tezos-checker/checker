(** A mock OCaml Implementation of Checker to serve as a specification *)

let redeem_liquidity = failwith "not_implemented"
let add_liquidity = failwith "not_implemented"
let sell_kit = failwith "not_implemented"
let buy_kit = failwith "not_implemented"
let mint_kit = failwith "not_implemented"
let create_burrow = failwith "not_implemented"
type error = | Insufficient_collateral
type lqs_utxo = unit
type kit_utxo = unit
type t = unit
let touch x = x
