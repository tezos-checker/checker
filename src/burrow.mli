(** Type of a burrow *)
type t

type kit_utxo

(** A right can be an {b admin} right, which implies all rights,
    or a {b user} right which can include depositing tez,
    withdrawing tez, minting kits, burning kit, and setting
    the delegate. *)
type rights

(** A permission is a ticket containing a right. *)
type permission

(** Whether or not the contract accepts permissionless tez deposits. *)
val allow_all_tez : t -> bool

(**** Deposits and withdrawals, burns and mints ****)

(** Simple deposit, accepting only tez with no parameters. Only possible if
  * allow_all_tez is set to true. *)
val default : t -> t

(** Deposit tez and / or burn kits. If the burrow does not require a
  * permission to deposit, the permission can be None. *)
val deposit : t -> call:Tezos.call -> permission option -> kit_utxo option -> t

(** Withdraw tez and/or mint_kits *)
val withdraw : t -> Tezos.tez -> kit_utxo -> Tezos.payment * kit_utxo * t

(** Sets the delegate *)
val set_delegate : t -> permission -> Tezos.address -> t

(**** Setting permissions ****)

(** Sets whether or not to accept all tez deposits without permissions. Requires admin. *)
val set_allow_all_tez_deposits : t -> permission -> bool -> t

(** Sets whether or not to accept all kit burns without permissions. Requires admin. *)
val set_allow_all_kit_burns : t -> permission -> bool -> t

(** Creates a new permission. Requires admin. *)
val make_permission : t -> permission -> rights -> (permission * t)

(** Requires admin. Increments a counter so that all previous permissions are
  * now invalid and returns a new admin permission.  This makes is easy to
  * transfer an admin permission to another party. *)
val invalidate_all_permissions : t -> permission -> (permission * t)


(**** Liquidation related ****)

(** - If the contract is not in liquidation and
    - It should not be liquidated : do nothing
      	 - If it should be liquidated : send an appropriate amount of tez to
      	   the liquidation queue and tip the caller
    - If the contract is in liquidation and
      	 - The amount in the liquidation queue is insufficient : send more to
      	   the liquidation queue
    - The amount in the liquidation queue is too high :
      	      - And that amount is 0: mark the contract as not being under
      	        liquidation anymore
      	      - And that amount is greater than 0: cancel the next upcoming
      	        liquidation for this contract

     To avoid hysteresis, it is assumed that tez sent to the liquidation will
     sell for, say 2/3 of the current price estimate.
 **)

(** Anyone can call touch, this helps perform housekeeping operations on the burrow. *)
val touch : t -> t

(* A few thoughts. What does it mean to be liquidated? It means that a
   portion of the funds is going to be sent to a liquidation queue in the
   checker contract. That liquidation queue auctions out blocks of, say,
   10,000 tez for kits (or some price defined amount).

   When the tez is taken from the contract and placed in the queue, the
   amount and "bins" your collateral is placed in is recorded. When the sell
   happens, a claim can be made for the kits burned to be deducted from this
   contract debt at the pro-rata.

   If the contract has recovered and should not be in liquidation anymore,
   cancel the earliest bid in the auction queue and get some tez back. This
   can be called repeatedly until it's all cancelled.
*)
