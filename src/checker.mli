(** A mock OCaml Implementation of Checker to serve as a specification *)

(** Represents the state of a checker contract *)
type t

(** A ticket signed by Checker representing a balance in kit. *)
type kit_utxo

(** A ticket signed by Checker representing a balance in liquidity shares. *)
type lqs_utxo

type error = | Insufficient_collateral

(***** Burrow creation and kint minting section *****)

(** "touch" can be called by anyone, it performs housekeeping tasks on the
  * contract state such as pulling in oracle values *)
val touch : t -> t

(** Creates and return a new burrow owned by owner, fund it if tez is passed
  * to the call. *)
val create_burrow : t -> call:Tezos.call -> owner:Tezos.address -> (Burrow.t * t)

(** Mint kits. Must be called by a burrow. Mint a kit utxo and send it back
  * to the calling burrow. *)
val mint_kit : t -> call:Tezos.call -> amount:Tezos.nat -> (t, error)  result

(***** uniswap section *****)

(** Buys kit by sending tez. Kit, and possibly tez, are sent back. *)
val buy_kit : t -> call:Tezos.call -> max_price:Tezos.ratio -> kit_utxo * Tezos.payment * t

(** Buys tez by sending kit. Tez, and a possibly kit is sent back. *)
val sell_kit : t -> call:Tezos.call -> kit_utxo -> min_price:Tezos.ratio -> kit_utxo * Tezos.payment * t

(** Add liquidity to the pool by sending tez and/or kits. *)
val add_liquidity: t -> call:Tezos.call -> kits:(Tezos.nat option) -> lqs_utxo * t

(** Redeem lqs utxo. *)
val redeem_liquidity : t -> call:Tezos.call -> lqs_utxo -> kit_utxo * Tezos.tez * t
