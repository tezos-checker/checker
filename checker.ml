(** A mock OCaml Implementation of Checker to serve as a specification *)

module Tezos : sig
  type address
  type entrypoint
  val address_of_string : string -> address option
  val get_entrypoint : address -> string -> address
  type nat
  type tez
  val tez_of_nat : nat -> tez
  val nat_of_int : int -> nat option
  type ratio = nat * nat

  (** Data available in a Tezos call, the amount sent and the sender. *)
  type call = {amount : tez ; sender : address }

  (** A ticket, a new proposed feature in Michelson. *)
  type 't ticket = {issuer : address ; amount : nat ; content : 't}

  (** A transaction coming out of some contract and paying tez to some address *)
  type payment = {destination : address ; amount : tez}
end = struct
  type address = string
  let address_of_string x = Some x
  type nat = int
  type tez = int
  let tez_of_nat x = x
  let nat_of_int x = if x < 0 then None else Some x
  let get_entrypoint address ep = address ^ "%" ^  ep
  type call = {amount : nat ; sender : address }
  type 't ticket = {issuer : address ; amount : nat ; content : 't}
  type payment = {destination : address ; amount : tez}
end


module Checker : sig
  type kit
  type kit_utxo
end = struct
  type kit = Tezos.nat
  type kit_utxo = unit Tezos.ticket
end

module rec Burrow : sig
  (** Type of a burrow *)
  type t

  (** A right can be an {b admin} right, which implies all rights,
      or a {b user} right which can include depositing tez,
      withdrawing tez, minting kits, burning kit, and setting
      the delegate. *)
  type rights
    = Admin
    | User of
        { deposit_tez : bool
        ; withdraw_tez : bool
        ; mint_kit : bool
        ; burn_kit : bool
        ; set_delegate : bool
        }

  (** A permission is a ticket containing a right. *)
  type permission = rights ticket

  (** Whether or not the contract accepts permissionless tez deposits. *)
  val allow_all_tez : t -> bool

  (**** Deposits and withdrawals, burns and mints ****)

  (** Simple deposit, accepting only tez with no parameters. Only possible if
    * allow_all_tez is set to true. *)
  val default : t -> t

  (** Deposit tez and / or burn kits. If the burrow does not require a
    * permission to deposit, the permission can be None. *)
  val deposit : t -> call:Tezos.call -> permission option -> Checker.kit_utxo option -> t

  (** Withdraw tez and/or mint_kits *)
  val withdraw : t -> Tezos.tez ->  Checker.kit -> Tezos.payment * Checker.kit_utxo * t

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

end = struct
  type t = unit
end
and Checker : sig
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


end = struct
end

