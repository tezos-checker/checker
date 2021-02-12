open Kit
open Permission
open Parameters
open Uniswap
open Burrow
open DelegationAuction
open LiquidationAuction
open LiquidationAuctionTypes
open TokenTypes

type burrow_id = Ligo.address

type checker =
  { burrows : (burrow_id, burrow) Ligo.big_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
    delegate : Ligo.key_hash option;
  }

(** Make a fresh state. *)
val initial_checker : checker

(** Perform housekeeping tasks on the contract state. This includes:
  * - Updating the system parameters
  * - Updating uniswap parameters (e.g. adding accrued burrowing fees to it)
  * - Update auction-related info (e.g. start a new auction)
  * - NOTE: Are there any other tasks to put in this list?
*)
val touch : checker -> Ligo.tez -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(** Create and return a new burrow containing the given tez as collateral,
  * minus the creation deposit. Fail if the tez is not enough to cover the
  * creation deposit. Additionally, return an Admin permission ticket to the
  * sender. *)
val create_burrow : checker -> Ligo.key_hash option -> (LigoOp.operation list * checker)

(** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
  * the burrow does not exist, or if the burrow does not allow deposits from
  * anyone and the permission ticket given is insufficient. *)
val deposit_tez : checker -> permission option -> burrow_id -> (LigoOp.operation list * checker)

(** Withdraw a non-negative amount of tez from a burrow. Fail if the burrow
  * does not exist, if this action would overburrow it, or if the permission
  * ticket given is insufficient. *)
val withdraw_tez : checker -> permission -> Ligo.tez -> burrow_id -> (LigoOp.operation list * checker)

(** Mint kits from a specific burrow. Fail if the burrow does not exist, if
  * there is not enough collateral, or if the permission ticket given is
  * insufficient. *)
val mint_kit : checker -> permission -> burrow_id -> kit -> (LigoOp.operation list * checker)

(** Deposit/burn a non-negative amount of kit to a burrow. If there is
  * excess kit, simply store it into the burrow. Fail if the burrow does not
  * exist, or if the burrow does not allow kit burnings from anyone and the
  * permission ticket given is insufficient. *)
val burn_kit : checker -> permission option -> burrow_id -> kit_token -> (LigoOp.operation list * checker)

(** Activate a currently inactive burrow. Fail if the burrow does not exist,
  * if the burrow is already active, if the amount of tez given is less than
  * the creation deposit, or if the permission ticket given is not an admin
  * ticket. *)
val activate_burrow : checker -> permission -> burrow_id -> (LigoOp.operation list * checker)

(** Deativate a currently active burrow. Fail if the burrow does not exist,
  * if it is already inactive, if it is overburrowed, if it has kit
  * outstanding, if it has collateral sent off to auctions, or if the
  * permission ticket given is not an admin ticket. If deactivation is
  * successful, make a tez payment to the given address. *)
val deactivate_burrow : checker -> permission -> burrow_id -> Ligo.address -> (LigoOp.operation list * checker)

(** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
  * liquidation or if the burrow does not exist. If successful, return the
  * reward, to be credited to the liquidator. *)
val mark_for_liquidation : checker -> burrow_id -> (LigoOp.operation list * checker)

(** Process the liquidation slices on completed liquidation auctions. Invalid
  * leaf_ptr's fail, and slices that correspond to incomplete liquidations are
  * ignored. *)
val touch_liquidation_slices : checker -> leaf_ptr list -> (LigoOp.operation list * checker)

(** Cancel the liquidation of a slice. The burden is on the caller to provide
  * both the burrow_id and the leaf_ptr. This operation can fail for several
  * reasons:
  * - If the leaf_ptr does not refer to the burrow_id given,
  * - if the permission given is insufficient for this operation,
  * - if the slice is already at the current auction,
  * - if the slice is part of an already completed auction,
  * - if the burrow is overburrowed at the moment.
*)
val cancel_liquidation_slice : checker -> permission -> leaf_ptr -> (LigoOp.operation list * checker)

(** Perform maintainance tasks for the burrow. *)
val touch_burrow : checker -> burrow_id -> (LigoOp.operation list * checker)

(** Set the delegate of a burrow. *)
val set_burrow_delegate : checker -> permission -> burrow_id -> Ligo.key_hash option -> (LigoOp.operation list * checker)

(** Requires admin. Create a new permission for a burrow. *)
val make_permission : checker -> permission -> burrow_id -> rights -> (LigoOp.operation list * checker)

(** Requires admin. Increments a counter so that all previous permissions are
  * now invalid and returns a new admin permission. This makes it easy to
  * transfer an admin permission to another party. *)
val invalidate_all_permissions : checker -> permission -> burrow_id -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

(** Buy some kit from the uniswap contract. Fail if the desired amount of kit
  * cannot be bought or if the deadline has passed. *)
val buy_kit : checker -> kit -> Ligo.timestamp -> (LigoOp.operation list * checker)

(** Sell some kit to the uniswap contract. Fail if the desired amount of tez
  * cannot be bought or if the deadline has passed. *)
val sell_kit : checker -> kit_token -> Ligo.tez -> Ligo.timestamp -> (LigoOp.operation list * checker)

(** Buy some liquidity (liquidity tokens) from the uniswap contract, by
  * giving it some tez and some kit. If the given amounts do not have the
  * right ratio, the uniswap contract keeps as much of the given tez and kit
  * as possible with the right ratio, and returns the leftovers, along with
  * the liquidity tokens. *)
val add_liquidity : checker -> kit_token -> Ligo.nat -> Ligo.timestamp -> (LigoOp.operation list * checker)

(** Sell some liquidity (liquidity tokens) to the uniswap contract in
  * exchange for the corresponding tez and kit of the right ratio. *)
val remove_liquidity : checker -> liquidity -> Ligo.tez -> kit -> Ligo.timestamp -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

(** Bid in current liquidation auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a ticket which can be used to
  * reclaim the kit when outbid. *)
val checker_liquidation_auction_place_bid : checker -> kit_token -> (LigoOp.operation list * checker)

(** Reclaim a failed bid for the current or a completed liquidation auction. *)
val checker_liquidation_auction_reclaim_bid : checker -> liquidation_auction_bid_ticket -> (LigoOp.operation list * checker)

(** Reclaim a winning bid for the current or a completed liquidation auction. *)
val checker_liquidation_auction_reclaim_winning_bid : checker -> liquidation_auction_bid_ticket -> (LigoOp.operation list * checker)

(* (\** Increase a failed bid for the current auction. *\)
 * val increase_bid : checker -> address:Ligo.address -> increase:kit -> bid_ticket:liquidation_auction_bid_ticket
 *   -> liquidation_auction_bid_ticket *)

(** Receive a liquidation slice from a burrow; we gather the slices in the
  * checker contract, and the checker contract is responsible for transfering
  * the lot to the liquidation auction winner. *)
val receive_slice_from_burrow : checker -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

(** Bid in current auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a token which can be used to either
  * reclaim the tez when outbid, or claim the auction result. *)
val checker_delegation_auction_place_bid : checker -> (LigoOp.operation list * checker)

(** Claim a win in the last cycle in order to become the delegate for this one. *)
val checker_delegation_auction_claim_win : checker -> delegation_auction_bid Ligo.ticket -> Ligo.key_hash -> (LigoOp.operation list * checker)

(** Reclaim a failed bid for the current or a completed auction. *)
val checker_delegation_auction_reclaim_bid : checker -> delegation_auction_bid Ligo.ticket -> (LigoOp.operation list * checker)

(* ENTRYPOINTS *)

type params =
  | Touch
  (* Burrows *)
  | CreateBurrow of Ligo.key_hash option
  | DepositTez of (permission option * burrow_id)
  | WithdrawTez of (permission * Ligo.tez * burrow_id)
  | MintKit of (permission * burrow_id * kit)
  | BurnKit of (permission option * burrow_id * kit_token)
  | ActivateBurrow of (permission * burrow_id)
  | DeactivateBurrow of (permission * burrow_id * Ligo.address)
  | MarkBurrowForLiquidation of burrow_id
  | TouchLiquidationSlices of leaf_ptr list
  | CancelSliceLiquidation of (permission * leaf_ptr)
  | TouchBurrow of burrow_id
  | SetBurrowDelegate of (permission * burrow_id * Ligo.key_hash option)
  | MakePermission of (permission * burrow_id * rights)
  | InvalidateAllPermissions of (permission * burrow_id)
  (* Uniswap *)
  | BuyKit of (kit * Ligo.timestamp)
  | SellKit of (kit_token * Ligo.tez * Ligo.timestamp)
  | AddLiquidity of (kit_token * Ligo.nat * Ligo.timestamp)
  | RemoveLiquidity of (liquidity * Ligo.tez * kit * Ligo.timestamp)
  (* Liquidation Auction *)
  | LiqAuctionPlaceBid of kit_token
  | LiqAuctionReclaimBid of liquidation_auction_bid_ticket
  | LiqAuctionReclaimWinningBid of liquidation_auction_bid_ticket
  | ReceiveLiquidationSlice
  (* Delegation Auction *)
  | DelegationAuctionPlaceBid
  | DelegationAuctionClaimWin of (delegation_auction_bid Ligo.ticket * Ligo.key_hash)
  | DelegationAuctionReclaimBid of delegation_auction_bid Ligo.ticket

val main : params * checker -> LigoOp.operation list * checker
