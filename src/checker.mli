open Ptr

(* TODO: Actually, at the end, this should be a Michelson address, which we
 * receive when we originate the burrow contract (Tezos.create_ticket_contract). *)
type burrow_id = Ptr.t

type t =
  { burrows : (ptr, Burrow.t) Ligo.big_map;
    uniswap : Uniswap.t;
    parameters : Parameters.t;
    liquidation_auctions : LiquidationAuction.auctions;
    delegation_auction : DelegationAuction.t;
    delegate : Ligo.address option;
  }

type Error.error +=
  | InvalidPermission
  | MissingPermission
  | InsufficientPermission
  | NonExistentBurrow of burrow_id
  | NotLiquidationCandidate of burrow_id
  | BurrowHasCompletedLiquidation
  | UnwarrantedCancellation
  | SlicePointsToDifferentBurrow
  | UnwantedTezGiven

(** Make a fresh state. *)
val initialize : Tezos.t -> t

(** Perform housekeeping tasks on the contract state. This includes:
  * - Updating the system parameters
  * - Updating uniswap parameters (e.g. adding accrued burrowing fees to it)
  * - Update auction-related info (e.g. start a new auction)
  * - NOTE: Are there any other tasks to put in this list?
*)
val touch : t -> tezos:Tezos.t -> index:Ligo.tez -> (Kit.token * t)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(** Create and return a new burrow containing the given tez as collateral,
  * minus the creation deposit. Fail if the tez is not enough to cover the
  * creation deposit. Additionally, return an Admin permission ticket to the
  * sender. *)
val create_burrow :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  (burrow_id * Permission.t * t, Error.error) result

(** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
  * the burrow does not exist, or if the burrow does not allow deposits from
  * anyone and the permission ticket given is insufficient. *)
val deposit_tez :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:(Permission.t option) ->
  burrow_id:burrow_id ->
  (t, Error.error) result

(** Withdraw a non-negative amount of tez from a burrow. Fail if the burrow
  * does not exist, if this action would overburrow it, or if the permission
  * ticket given is insufficient. *)
val withdraw_tez :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  tez:Ligo.tez ->
  burrow_id:burrow_id ->
  (Tez.payment * t, Error.error) result

(** Mint kits from a specific burrow. Fail if the burrow does not exist, if
  * there is not enough collateral, or if the permission ticket given is
  * insufficient. *)
val mint_kit :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  kit:Kit.t ->
  (Kit.token * t, Error.error) result

(** Deposit/burn a non-negative amount of kit to a burrow. If there is
  * excess kit, simply store it into the burrow. Fail if the burrow does not
  * exist, or if the burrow does not allow kit burnings from anyone and the
  * permission ticket given is insufficient. *)
val burn_kit :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:(Permission.t option) ->
  burrow_id:burrow_id ->
  kit:Kit.token ->
  (t, Error.error) result

(** Activate a currently inactive burrow. Fail if the burrow does not exist,
  * if the burrow is already active, if the amount of tez given is less than
  * the creation deposit, or if the permission ticket given is not an admin
  * ticket. *)
val activate_burrow :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  (t, Error.error) result

(** Deativate a currently active burrow. Fail if the burrow does not exist,
  * if it is already inactive, if it is overburrowed, if it has kit
  * outstanding, if it has collateral sent off to auctions, or if the
  * permission ticket given is not an admin ticket. If deactivation is
  * successful, make a tez payment to the given address. *)
val deactivate_burrow :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  recipient:Ligo.address ->
  (Tez.payment * t, Error.error) result

(** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
  * liquidation or if the burrow does not exist. If successful, return the
  * reward, to be credited to the liquidator. *)
val mark_for_liquidation :
  t ->
  call:Call.t ->
  burrow_id:burrow_id ->
  (Tez.payment * t, Error.error) result

(** Process the liquidation slices on completed liquidation auctions. Invalid
  * leaf_ptr's fail, and slices that correspond to incomplete liquidations are
  * ignored. *)
val touch_liquidation_slices : t -> Avl.leaf_ptr list -> t

(** Cancel the liquidation of a slice. The burden is on the caller to provide
  * both the burrow_id and the leaf_ptr. This operation can fail for several
  * reasons:
  * - If the leaf_ptr does not refer to the burrow_id given,
  * - if the permission given is insufficient for this operation,
  * - if the slice is already at the current auction,
  * - if the slice is part of an already completed auction,
  * - if the burrow is overburrowed at the moment.
*)
val cancel_liquidation_slice :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  Avl.leaf_ptr ->
  (t, Error.error) result

(** Perform maintainance tasks for the burrow. *)
val touch_burrow : t -> burrow_id -> (t, Error.error) result

(** Set the delegate of a burrow. *)
val set_burrow_delegate :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  delegate:Ligo.address ->
  (t, Error.error) result

(** Requires admin. Create a new permission for a burrow. *)
val make_permission :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  rights:Permission.rights ->
  (Permission.t, Error.error) result

(** Requires admin. Increments a counter so that all previous permissions are
  * now invalid and returns a new admin permission. This makes it easy to
  * transfer an admin permission to another party. *)
val invalidate_all_permissions :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  permission:Permission.t ->
  burrow_id:burrow_id ->
  (Permission.t * t, Error.error) result

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

(** Buy some kit from the uniswap contract. Fail if the desired amount of kit
  * cannot be bought or if the deadline has passed. *)
val buy_kit :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  min_kit_expected:Kit.t ->
  deadline:Ligo.timestamp ->
  (Kit.token * t, Error.error) result

(** Sell some kit to the uniswap contract. Fail if the desired amount of tez
  * cannot be bought or if the deadline has passed. *)
val sell_kit :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  kit:Kit.token ->
  min_tez_expected:Ligo.tez ->
  deadline:Ligo.timestamp ->
  (Tez.payment * t, Error.error) result

(** Buy some liquidity (liquidity tokens) from the uniswap contract, by
  * giving it some tez and some kit. If the given amounts do not have the
  * right ratio, the uniswap contract keeps as much of the given tez and kit
  * as possible with the right ratio, and returns the leftovers, along with
  * the liquidity tokens. *)
val add_liquidity :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  max_kit_deposited:Kit.token ->
  min_lqt_minted:Ligo.nat ->
  deadline:Ligo.timestamp ->
  (Uniswap.liquidity * Kit.token * t, Error.error) result

(** Sell some liquidity (liquidity tokens) to the uniswap contract in
  * exchange for the corresponding tez and kit of the right ratio. *)
val remove_liquidity :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  lqt_burned:Uniswap.liquidity ->
  min_tez_withdrawn:Ligo.tez ->
  min_kit_withdrawn:Kit.t ->
  deadline:Ligo.timestamp ->
  (Tez.payment * Kit.token * t, Error.error) result

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

(** Bid in current liquidation auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a ticket which can be used to
  * reclaim the kit when outbid. *)
val liquidation_auction_place_bid :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  kit:Kit.token ->
  (LiquidationAuction.bid_ticket * t, Error.error) result

(** Reclaim a failed bid for the current or a completed liquidation auction. *)
val liquidation_auction_reclaim_bid :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  bid_ticket:LiquidationAuction.bid_ticket ->
  (Kit.token, Error.error) result

(** Reclaim a winning bid for the current or a completed liquidation auction. *)
val liquidation_auction_reclaim_winning_bid :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  bid_ticket:LiquidationAuction.bid_ticket ->
  (Tez.payment * t, Error.error) result

(* (\** Increase a failed bid for the current auction. *\)
 * val increase_bid : t -> address:Ligo.address -> increase:Kit.t -> bid_ticket:LiquidationAuction.bid_ticket
 *   -> (LiquidationAuction.bid_ticket, Error.error) result *)

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

(** Bid in current auction. Fail if the auction is closed, or if the bid is
  * too low. If successful, return a token which can be used to either
  * reclaim the tez when outbid, or claim the auction result. *)
val delegation_auction_place_bid :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  (DelegationAuction.bid_ticket * t, Error.error) result

(** Claim a win in the last cycle in order to become the delegate for this one. *)
val delegation_auction_claim_win :
  t ->
  tezos:Tezos.t ->
  bid_ticket:DelegationAuction.bid_ticket ->
  (t, Error.error) result

(** Reclaim a failed bid for the current or a completed auction. *)
val delegation_auction_reclaim_bid :
  t ->
  tezos:Tezos.t ->
  call:Call.t ->
  bid_ticket:DelegationAuction.bid_ticket ->
  (Tez.payment * t, Error.error) result
