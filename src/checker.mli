open Kit
open LiquidationAuctionPrimitiveTypes
open Tickets
open CheckerTypes

(** Perform housekeeping tasks on the contract state. This includes:
  * - Updating the system parameters
  * - Updating uniswap parameters (e.g. adding accrued burrowing fees to it)
  * - Update auction-related info (e.g. start a new auction)
  * - NOTE: Are there any other tasks to put in this list?
*)
val endpoint_touch : checker * unit -> (LigoOp.operation list * checker)

(* FOR TESTING. *)
val touch_with_index : checker -> Ligo.tez -> (LigoOp.operation list * checker)

(* FOR TESTING. *)
val calculate_touch_reward : Ligo.timestamp -> kit

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(** Create and return a new burrow containing the given tez as collateral,
  * minus the creation deposit. Fail if the tez is not enough to cover the
  * creation deposit. Additionally, return an Admin permission ticket to the
  * sender. *)
val endpoint_create_burrow : checker * Ligo.key_hash option -> (LigoOp.operation list * checker)

(** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
  * the burrow does not exist, or if the burrow does not allow deposits from
  * anyone and the permission ticket given is insufficient. *)
val endpoint_deposit_tez : checker * (permission_redacted_content option * burrow_id) -> (LigoOp.operation list * checker)

(** Withdraw a non-negative amount of tez from a burrow. Fail if the burrow
  * does not exist, if this action would overburrow it, or if the permission
  * ticket given is insufficient. *)
val endpoint_withdraw_tez : checker * (permission_redacted_content * Ligo.tez * burrow_id) -> LigoOp.operation list * checker

(** Mint kits from a specific burrow. Fail if the burrow does not exist, if
  * there is not enough collateral, or if the permission ticket given is
  * insufficient. *)
val endpoint_mint_kit : checker * (permission_redacted_content * burrow_id * kit) -> LigoOp.operation list * checker

(** Deposit/burn a non-negative amount of kit to a burrow. If there is
  * excess kit, simply store it into the burrow. Fail if the burrow does not
  * exist, or if the burrow does not allow kit burnings from anyone and the
  * permission ticket given is insufficient. *)
val endpoint_burn_kit : checker * (permission_redacted_content option * burrow_id * kit) -> (LigoOp.operation list * checker)

(** Activate a currently inactive burrow. Fail if the burrow does not exist,
  * if the burrow is already active, if the amount of tez given is less than
  * the creation deposit, or if the permission ticket given is not an admin
  * ticket. *)
val endpoint_activate_burrow : checker * (permission_redacted_content * burrow_id) -> LigoOp.operation list * checker

(** Deativate a currently active burrow. Fail if the burrow does not exist,
  * if it is already inactive, if it is overburrowed, if it has kit
  * outstanding, if it has collateral sent off to auctions, or if the
  * permission ticket given is not an admin ticket. If deactivation is
  * successful, make a tez payment to the sender. *)
val endpoint_deactivate_burrow : checker * (permission_redacted_content * burrow_id) -> LigoOp.operation list * checker

(** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
  * liquidation or if the burrow does not exist. If successful, return the
  * reward, to be credited to the liquidator. *)
val endpoint_mark_for_liquidation : checker * burrow_id -> (LigoOp.operation list * checker)

(** Process the liquidation slices on completed liquidation auctions. Invalid
  * leaf_ptr's fail, and slices that correspond to incomplete liquidations are
  * ignored. *)
val endpoint_touch_liquidation_slices : checker * leaf_ptr list -> (LigoOp.operation list * checker)

(** Cancel the liquidation of a slice. The burden is on the caller to provide
  * both the burrow_id and the leaf_ptr. This operation can fail for several
  * reasons:
  * - If the leaf_ptr does not refer to the burrow_id given,
  * - if the permission given is insufficient for this operation,
  * - if the slice is already at the current auction,
  * - if the slice is part of an already completed auction,
  * - if the burrow is overburrowed at the moment.
*)
val endpoint_cancel_liquidation_slice : checker * (permission_redacted_content * leaf_ptr) -> LigoOp.operation list * checker

(** Perform maintainance tasks for the burrow. *)
val endpoint_touch_burrow : checker * burrow_id -> LigoOp.operation list * checker

(** Set the delegate of a burrow. *)
val endpoint_set_burrow_delegate : checker * (permission_redacted_content * burrow_id * Ligo.key_hash option) -> (LigoOp.operation list * checker)

(** Requires admin. Create a new permission for a burrow. *)
val endpoint_make_permission : checker * (permission_redacted_content * burrow_id * rights) -> (LigoOp.operation list * checker)

(** Requires admin. Increments a counter so that all previous permissions are
  * now invalid and returns a new admin permission. This makes it easy to
  * transfer an admin permission to another party. *)
val endpoint_invalidate_all_permissions : checker * (permission_redacted_content * burrow_id) -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

(** Buy some kit from the uniswap contract. Fail if the desired amount of kit
  * cannot be bought or if the deadline has passed. *)
val endpoint_buy_kit : checker * (kit * Ligo.timestamp) -> LigoOp.operation list * checker

(** Sell some kit to the uniswap contract. Fail if the desired amount of tez
  * cannot be bought or if the deadline has passed. *)
val endpoint_sell_kit : checker * (kit * Ligo.tez * Ligo.timestamp) -> LigoOp.operation list * checker

(** Buy some liquidity (liquidity tokens) from the uniswap contract, by
  * giving it some tez and some kit. If the given amounts do not have the
  * right ratio, the uniswap contract keeps as much of the given tez and kit
  * as possible with the right ratio, and returns the leftovers, along with
  * the liquidity tokens. *)
val endpoint_add_liquidity : checker * (kit * Ligo.nat * Ligo.timestamp) -> LigoOp.operation list * checker

(** Sell some liquidity (liquidity tokens) to the uniswap contract in
  * exchange for the corresponding tez and kit of the right ratio. *)
val endpoint_remove_liquidity : checker * (Ligo.nat * Ligo.tez * kit * Ligo.timestamp) -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

(** Bid in current liquidation auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a ticket which can be used to
  * reclaim the kit when outbid. *)
val endpoint_liquidation_auction_place_bid : checker * kit -> LigoOp.operation list * checker

(** Reclaim a failed bid for the current or a completed liquidation auction. *)
val endpoint_liquidation_auction_reclaim_bid : checker * liquidation_auction_bid -> LigoOp.operation list * checker

(** Claim a winning bid for the current or a completed liquidation auction. *)
val endpoint_liquidation_auction_claim_win : checker * liquidation_auction_bid -> LigoOp.operation list * checker

(* (\** Increase a failed bid for the current auction. *\)
 * val increase_bid : checker -> address:Ligo.address -> increase:kit -> bid_ticket:liquidation_auction_bid_ticket
 *   -> liquidation_auction_bid_ticket *)

(** Receive a liquidation slice from a burrow; we gather the slices in the
  * checker contract, and the checker contract is responsible for transfering
  * the lot to the liquidation auction winner. *)
val endpoint_receive_slice_from_burrow : checker * unit -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

(** Bid in current auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a token which can be used to either
  * reclaim the tez when outbid, or claim the auction result. *)
val endpoint_delegation_auction_place_bid : checker * unit -> (LigoOp.operation list * checker)

(** Claim a win in the last cycle in order to become the delegate for this one. *)
val endpoint_delegation_auction_claim_win : checker * (delegation_auction_bid * Ligo.key_hash) -> (LigoOp.operation list * checker)

(** Reclaim a failed bid for the current or a completed auction. *)
val endpoint_delegation_auction_reclaim_bid : checker * delegation_auction_bid -> (LigoOp.operation list * checker)

(* ************************************************************************* *)
(**                           CHECKER PARAMETERS                             *)
(* ************************************************************************* *)

(** User-facing checker parameters. These include non-serializable tickets. *)
type checker_params =
    Touch of unit
  | CreateBurrow of Ligo.key_hash option
  | DepositTez of (permission option * burrow_id)
  | WithdrawTez of (permission * Ligo.tez * burrow_id)
  | MintKit of (permission * burrow_id * kit)
  | BurnKit of (permission option * burrow_id * kit_token)
  | ActivateBurrow of (permission * burrow_id)
  | DeactivateBurrow of (permission * burrow_id)
  | MarkForLiquidation of burrow_id
  | TouchLiquidationSlices of leaf_ptr list
  | CancelLiquidationSlice of (permission * leaf_ptr)
  | TouchBurrow of burrow_id
  | SetBurrowDelegate of (permission * burrow_id * Ligo.key_hash option)
  | MakePermission of (permission * burrow_id * rights)
  | InvalidateAllPermissions of (permission * burrow_id)
  | BuyKit of (kit * Ligo.timestamp)
  | SellKit of (kit_token * Ligo.tez * Ligo.timestamp)
  | AddLiquidity of (kit_token * Ligo.nat * Ligo.timestamp)
  | RemoveLiquidity of (liquidity * Ligo.tez * kit * Ligo.timestamp)
  | LiquidationAuctionPlaceBid of kit_token
  | LiquidationAuctionReclaimBid of liquidation_auction_bid_ticket
  | LiquidationAuctionClaimWin of liquidation_auction_bid_ticket
  | ReceiveSliceFromBurrow of unit
  | DelegationAuctionPlaceBid of unit
  | DelegationAuctionClaimWin of (delegation_auction_bid_ticket * Ligo.key_hash)
  | DelegationAuctionReclaimBid of delegation_auction_bid_ticket

val deticketify_touch : unit -> unit
val deticketify_create_burrow : Ligo.key_hash option -> Ligo.key_hash option
val deticketify_deposit_tez : permission option * burrow_id -> permission_redacted_content option * burrow_id
val deticketify_withdraw_tez : permission * Ligo.tez * burrow_id -> permission_redacted_content * Ligo.tez * burrow_id
val deticketify_mint_kit : permission * burrow_id * kit -> permission_redacted_content * burrow_id * kit
val deticketify_burn_kit : permission option * burrow_id * kit_token -> permission_redacted_content option * burrow_id * kit
val deticketify_activate_burrow : permission * burrow_id -> permission_redacted_content * burrow_id
val deticketify_deactivate_burrow : permission * burrow_id -> permission_redacted_content * burrow_id
val deticketify_mark_for_liquidation : burrow_id -> burrow_id
val deticketify_touch_liquidation_slices : leaf_ptr list -> leaf_ptr list
val deticketify_cancel_liquidation_slice : permission * leaf_ptr -> permission_redacted_content * leaf_ptr
val deticketify_touch_burrow : burrow_id -> burrow_id
val deticketify_set_burrow_delegate : permission * burrow_id * Ligo.key_hash option -> permission_redacted_content * burrow_id * Ligo.key_hash option
val deticketify_make_permission : permission * burrow_id * rights -> permission_redacted_content * burrow_id * rights
val deticketify_invalidate_all_permissions : permission * burrow_id -> permission_redacted_content * burrow_id
val deticketify_buy_kit : kit * Ligo.timestamp -> kit * Ligo.timestamp
val deticketify_sell_kit : kit_token * Ligo.tez * Ligo.timestamp -> kit * Ligo.tez * Ligo.timestamp
val deticketify_add_liquidity : kit_token * Ligo.nat * Ligo.timestamp -> kit * Ligo.nat * Ligo.timestamp
val deticketify_remove_liquidity : liquidity * Ligo.tez * kit * Ligo.timestamp -> Ligo.nat * Ligo.tez * kit * Ligo.timestamp
val deticketify_liquidation_auction_place_bid : kit_token -> kit
val deticketify_liquidation_auction_reclaim_bid : liquidation_auction_bid_ticket -> liquidation_auction_bid
val deticketify_liquidation_auction_claim_win : liquidation_auction_bid_ticket -> liquidation_auction_bid
val deticketify_receive_slice_from_burrow : unit -> unit
val deticketify_delegation_auction_place_bid : unit -> unit
val deticketify_delegation_auction_claim_win : delegation_auction_bid_ticket * Ligo.key_hash -> delegation_auction_bid * Ligo.key_hash
val deticketify_delegation_auction_reclaim_bid : delegation_auction_bid_ticket -> delegation_auction_bid
