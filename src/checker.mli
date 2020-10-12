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

(***** auctions section *****)

(**

   Check state of a burrow and update if necessary.

   - Updates the burrow's outstanding kit balance according to the
     compounding burrowing fee and imbalance adjustments.

   - Calculates whether the collateral is sufficient for the burrow's
     outstanding kit balance.

   The amount of tez needed per kit, to be considered adequate collateral, is

    f * q * tz(liq).

   The amount of outstanding kit that does not have adequate collateral is therefore

    kit_without_collateral = kit_balance - (collateral / f * q * tz(liq))

   If kit_without_collateral <= 0, then no liquidation is required.

   We To each illiquid kit to restore that shortfall would be expected to cost

    tz(minting)

   The liquidation_threshold for the collateral is:

     kit_balance * f * q * tz(liq)

   If collateral >= liquidation_threshold then no action is taken.

   Otherwise, a payment of burrow_creation_deposit + 0.1cNp of the tez
   collateral is immediately transferred to the caller as a reward.

   The burrow_creation_deposit is then refilled from the remaining
   collateral, which is reduced accordingly. If it cannot be refilled,
   everything is liquidated and the burrow is simply closed. FIXME

   The cost in tez to mint the kit balance would be

    full_minting_collateral = burrow_creation_deposit + kit_balance * f * q * tz(minting)

   The amount

    collateral_shortfall = full_minting_collateral - liquidation_threshold

   is the amount of tez collateral that

   Then, the difference between the threshold and the new tez balance
   is set aside for auction.

 **)
val touch_burrow : t -> call:Tezos.call -> burrow:Tezos.address -> Tezos.payment option
