open Ptr
open Kit
open Permission
open Parameters
open Uniswap
open Burrow
open DelegationAuction
open LiquidationAuction

(* Tez payments (operations, really) *)
type tez_payment = {destination: Ligo.address; amnt: Ligo.tez;}

val pp_tez_payment : Format.formatter -> tez_payment -> unit
val show_tez_payment : tez_payment -> string

(* TODO: Actually, at the end, this should be a Michelson address, which we
 * receive when we originate the burrow contract (Tezos.create_ticket_contract). *)
type burrow_id = Ptr.t

type t =
  { burrows : (ptr, burrow) Ligo.big_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
    delegate : Ligo.key_hash option;
    last_burrow_id: ptr;
  }

(** Make a fresh state. *)
val initial_checker : t

(** Perform housekeeping tasks on the contract state. This includes:
  * - Updating the system parameters
  * - Updating uniswap parameters (e.g. adding accrued burrowing fees to it)
  * - Update auction-related info (e.g. start a new auction)
  * - NOTE: Are there any other tasks to put in this list?
*)
val touch : t -> Ligo.tez -> (kit_token * Ligo.operation list * t)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(** Create and return a new burrow containing the given tez as collateral,
  * minus the creation deposit. Fail if the tez is not enough to cover the
  * creation deposit. Additionally, return an Admin permission ticket to the
  * sender. *)
val create_burrow : t -> (burrow_id * permission * t)

(** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
  * the burrow does not exist, or if the burrow does not allow deposits from
  * anyone and the permission ticket given is insufficient. *)
val deposit_tez : t -> permission option -> burrow_id -> t

(** Withdraw a non-negative amount of tez from a burrow. Fail if the burrow
  * does not exist, if this action would overburrow it, or if the permission
  * ticket given is insufficient. *)
val withdraw_tez : t -> permission -> Ligo.tez -> burrow_id -> (tez_payment * t)

(** Mint kits from a specific burrow. Fail if the burrow does not exist, if
  * there is not enough collateral, or if the permission ticket given is
  * insufficient. *)
val mint_kit : t -> permission -> burrow_id -> kit -> (kit_token * t)

(** Deposit/burn a non-negative amount of kit to a burrow. If there is
  * excess kit, simply store it into the burrow. Fail if the burrow does not
  * exist, or if the burrow does not allow kit burnings from anyone and the
  * permission ticket given is insufficient. *)
val burn_kit : t -> permission option -> burrow_id -> kit_token -> t

(** Activate a currently inactive burrow. Fail if the burrow does not exist,
  * if the burrow is already active, if the amount of tez given is less than
  * the creation deposit, or if the permission ticket given is not an admin
  * ticket. *)
val activate_burrow : t -> permission -> burrow_id -> t

(** Deativate a currently active burrow. Fail if the burrow does not exist,
  * if it is already inactive, if it is overburrowed, if it has kit
  * outstanding, if it has collateral sent off to auctions, or if the
  * permission ticket given is not an admin ticket. If deactivation is
  * successful, make a tez payment to the given address. *)
val deactivate_burrow : t -> permission -> burrow_id -> Ligo.address -> (tez_payment * t)

(** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
  * liquidation or if the burrow does not exist. If successful, return the
  * reward, to be credited to the liquidator. *)
val mark_for_liquidation : t -> burrow_id -> (tez_payment * t)

(** Process the liquidation slices on completed liquidation auctions. Invalid
  * leaf_ptr's fail, and slices that correspond to incomplete liquidations are
  * ignored. *)
val touch_liquidation_slices : t * LiquidationAuctionTypes.leaf_ptr list -> t

(** Cancel the liquidation of a slice. The burden is on the caller to provide
  * both the burrow_id and the leaf_ptr. This operation can fail for several
  * reasons:
  * - If the leaf_ptr does not refer to the burrow_id given,
  * - if the permission given is insufficient for this operation,
  * - if the slice is already at the current auction,
  * - if the slice is part of an already completed auction,
  * - if the burrow is overburrowed at the moment.
*)
val cancel_liquidation_slice : t -> permission -> LiquidationAuctionTypes.leaf_ptr -> t

(** Perform maintainance tasks for the burrow. *)
val touch_burrow : t -> burrow_id -> t

(** Set the delegate of a burrow. *)
val set_burrow_delegate : t -> permission -> burrow_id -> Ligo.address -> t

(** Requires admin. Create a new permission for a burrow. *)
val make_permission : t -> permission -> burrow_id -> right -> permission

(** Requires admin. Increments a counter so that all previous permissions are
  * now invalid and returns a new admin permission. This makes it easy to
  * transfer an admin permission to another party. *)
val invalidate_all_permissions : t -> permission -> burrow_id -> (permission * t)

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

(** Buy some kit from the uniswap contract. Fail if the desired amount of kit
  * cannot be bought or if the deadline has passed. *)
val buy_kit : t -> kit -> Ligo.timestamp -> (kit_token * Ligo.operation list * t)

(** Sell some kit to the uniswap contract. Fail if the desired amount of tez
  * cannot be bought or if the deadline has passed. *)
val sell_kit : t -> kit_token -> Ligo.tez -> Ligo.timestamp -> (tez_payment * Ligo.operation list * t)

(** Buy some liquidity (liquidity tokens) from the uniswap contract, by
  * giving it some tez and some kit. If the given amounts do not have the
  * right ratio, the uniswap contract keeps as much of the given tez and kit
  * as possible with the right ratio, and returns the leftovers, along with
  * the liquidity tokens. *)
val add_liquidity : t -> kit_token -> Ligo.nat -> Ligo.timestamp -> (liquidity * kit_token * Ligo.operation list * t)

(** Sell some liquidity (liquidity tokens) to the uniswap contract in
  * exchange for the corresponding tez and kit of the right ratio. *)
val remove_liquidity : t -> liquidity -> Ligo.tez -> kit -> Ligo.timestamp -> (tez_payment * kit_token * Ligo.operation list * t)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

(** Bid in current liquidation auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a ticket which can be used to
  * reclaim the kit when outbid. *)
val liquidation_auction_place_bid : t -> kit_token -> (liquidation_auction_bid_ticket * t)

(** Reclaim a failed bid for the current or a completed liquidation auction. *)
val liquidation_auction_reclaim_bid : t -> liquidation_auction_bid_ticket -> kit_token

(** Reclaim a winning bid for the current or a completed liquidation auction. *)
val liquidation_auction_reclaim_winning_bid : t -> liquidation_auction_bid_ticket -> (tez_payment * t)

(* (\** Increase a failed bid for the current auction. *\)
 * val increase_bid : t -> address:Ligo.address -> increase:kit -> bid_ticket:liquidation_auction_bid_ticket
 *   -> liquidation_auction_bid_ticket *)

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

(** Bid in current auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a token which can be used to either
  * reclaim the tez when outbid, or claim the auction result. *)
val delegation_auction_place_bid :
  t -> (delegation_auction_bid_ticket * Ligo.operation list * t)

(** Claim a win in the last cycle in order to become the delegate for this one. *)
val delegation_auction_claim_win : t -> delegation_auction_bid_ticket -> Ligo.key_hash -> (Ligo.operation list * t)

(** Reclaim a failed bid for the current or a completed auction. *)
val delegation_auction_reclaim_bid : t -> delegation_auction_bid_ticket -> tez_payment * Ligo.operation list * t

(* ENTRYPOINTS *)

type params =
  | DelegationAuctionClaimWin of (delegation_auction_bid_ticket * Ligo.key_hash)

val main : params * t -> Ligo.operation list * t
