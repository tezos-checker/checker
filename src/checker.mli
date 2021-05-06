open Ctez
open Kit
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes
open CheckerTypes
open Fa12Types
open Fa2Interface

(** Perform housekeeping tasks on the contract state. This includes:
    - Updating the system parameters
    - Updating cfmm parameters (e.g. adding accrued burrowing fees to it)
    - Update auction-related info (e.g. start a new auction)
    - NOTE: the list is not exhaustive at the moment.
*)
val entrypoint_touch : checker * unit -> (LigoOp.operation list * checker)

(**/**)
(* FOR TESTING. *)
val assert_checker_invariants : checker -> unit
(**/**)

(**/**)
(* FOR TESTING. *)
val touch_with_index : checker -> Ligo.tez -> (LigoOp.operation list * checker)
(**/**)

(**/**)
(* FOR TESTING. *)
val calculate_touch_reward : Ligo.timestamp -> kit
(**/**)

(*****************************************************************************)
(**                            External calls                                *)
(*****************************************************************************)

val get_transfer_ctez_entrypoint : external_contracts -> fa12_transfer Ligo.contract
val get_oracle_entrypoint : external_contracts -> (Ligo.nat Ligo.contract) Ligo.contract

(*****************************************************************************)
(**                            {1 BURROWS}                                   *)
(*****************************************************************************)

(** Create and return a new burrow containing the given tez as collateral,
    minus the creation deposit. Fail if the tez is not enough to cover the
    creation deposit.

    Parameters:
    - An optional delegate address for the freshly-originated burrow contract
*)
val entrypoint_create_burrow : checker * (Ligo.nat * Ligo.key_hash option) -> (LigoOp.operation list * checker)

(** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
    the burrow does not exist, or if the sender is not the burrow owner.

    Parameters:
    - The ID of the burrow into which the collateral will be deposited
*)
val entrypoint_deposit_tez : checker * Ligo.nat -> (LigoOp.operation list * checker)

(** Withdraw a non-negative amount of tez from a burrow. Fail if the burrow
    does not exist, if this action would overburrow it, or if the sender is not
    the burrow owner.

    Parameters:
    - The amount of tez to withdraw
    - The ID of the burrow from which the collateral should be withdrawn
*)
val entrypoint_withdraw_tez : checker * (Ligo.tez * Ligo.nat) -> LigoOp.operation list * checker

(** Mint kits from a specific burrow. Fail if the burrow does not exist, if
    there is not enough collateral, or if the sender is not the burrow owner.

    Parameters:
    - The ID of the burrow from which to mint kit
    - The amount of kit to mint
*)
val entrypoint_mint_kit : checker * (Ligo.nat * kit) -> LigoOp.operation list * checker

(** Deposit/burn a non-negative amount of kit to a burrow. If there is excess
    kit, simply store it into the burrow. Fail if the burrow does not exist, or
    if the sender is not the burrow owner.

    Parameters:
    - The ID of the burrow in which to burn the kit
    - The amount of kit to burn (supplied as a ticket)
*)
val entrypoint_burn_kit : checker * (Ligo.nat * kit) -> (LigoOp.operation list * checker)

(** Activate a currently inactive burrow. Fail if the burrow does not exist,
    if the burrow is already active, if the amount of tez given is less than
    the creation deposit, or if the sender is not the burrow owner.

    Parameters:
    - The ID of the burrow to activate
*)
val entrypoint_activate_burrow : checker * Ligo.nat -> LigoOp.operation list * checker

(** Deativate a currently active burrow. Fails if the burrow does not exist,
    if it is already inactive, if it is overburrowed, if it has kit
    outstanding, if it has collateral sent off to auctions, or if the sender is
    not the burrow owner. If deactivation is successful, make a tez payment to
    the given address.

    Parameters:
    - The ID of the burrow to deactivate
    - The address to make the tez payment to
*)
val entrypoint_deactivate_burrow : checker * (Ligo.nat * Ligo.address) -> LigoOp.operation list * checker

(** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
    liquidation or if the burrow does not exist. If successful, the reward is
    credited to the liquidator.

    Parameters:
    - The ID of the burrow to mark for liquidation
*)
val entrypoint_mark_for_liquidation : checker * burrow_id -> (LigoOp.operation list * checker)

(** Process the liquidation slices on completed liquidation auctions. Invalid
    leaf_ptr's fail, and slices that correspond to incomplete liquidations are
    ignored. *)
val entrypoint_touch_liquidation_slices : checker * leaf_ptr list -> (LigoOp.operation list * checker)

(** Cancel the liquidation of a slice of collateral that has been queued for
    auction. This operation can fail for several reasons:

    - if the sender is not the burrow owner,
    - if the slice is already at the current auction,
    - if the slice is part of an already completed auction, or
    - if the burrow is overburrowed at the moment.

    Parameters:
    - The liquidation slice to cancel
*)
val entrypoint_cancel_liquidation_slice : checker * leaf_ptr -> LigoOp.operation list * checker

(** Perform maintenance tasks for the burrow.

    Parameters:
    - The ID of the burrow
*)
val entrypoint_touch_burrow : checker * burrow_id -> LigoOp.operation list * checker

(** Set the delegate of a burrow. Fail if if the sender is not the burrow
    owner.

    Parameters:
    - The ID of the burrow to modify
    - The key hash of the new delegate's address, or None to unset the delegate
*)
val entrypoint_set_burrow_delegate : checker * (Ligo.nat * Ligo.key_hash option) -> (LigoOp.operation list * checker)

(*****************************************************************************)
(**                              {1 CFMM}                                    *)
(*****************************************************************************)

(** Buy some kit from the cfmm contract. Fail if the desired amount of kit
    cannot be bought or if the deadline has passed.

    Parameters:
    - The amount of ctez to be sold for kit
    - The minimum amount of kit expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_buy_kit : checker * (ctez * kit * Ligo.timestamp) -> LigoOp.operation list * checker

(** Sell some kit to the cfmm contract. Fail if the desired amount of ctez
    cannot be bought or if the deadline has passed.

    Parameters:
    - The amount of kit to be sold for ctez
    - The minimum amount of ctez expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_sell_kit : checker * (kit * ctez * Ligo.timestamp) -> LigoOp.operation list * checker

(** Buy some liquidity (liquidity tokens) from the cfmm contract, by
    giving it some ctez and some kit. If the given amounts do not have the
    right ratio, the cfmm contract keeps as much of the given ctez and kit
    as possible with the right ratio, and returns the leftovers, along with
    the liquidity tokens.

    Parameters:
    - The amount of ctez to be sold
    - The amount of kit to be sold
    - The minimum number of liquidity tokens expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_add_liquidity : checker * (ctez * kit * Ligo.nat * Ligo.timestamp) -> LigoOp.operation list * checker

(** Sell some liquidity (liquidity tokens) to the cfmm contract in
    exchange for the corresponding ctez and kit of the right ratio.

    Parameters:
    - The number of liquidity tokens to be sold
    - The minimum amount of ctez expected to be bought
    - The minimum amount of kit expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_remove_liquidity : checker * (Ligo.nat * ctez * kit * Ligo.timestamp) -> (LigoOp.operation list * checker)

(*****************************************************************************)
(**                      {1 LIQUIDATION AUCTIONS}                            *)
(*****************************************************************************)

(** Bid in current liquidation auction. Fail if the auction is closed, or if the bid is
    too low. If successful, return a ticket which can be used to
    reclaim the kit when outbid. *)
val entrypoint_liquidation_auction_place_bid : checker * kit -> LigoOp.operation list * checker

(** Claim a winning bid for the current or a completed liquidation auction. *)
val entrypoint_liquidation_auction_claim_win : checker * liquidation_auction_id -> LigoOp.operation list * checker

(** Receive a liquidation slice from a burrow; we gather the slices in the
    checker contract, and the checker contract is responsible for transfering
    the lot to the liquidation auction winner. *)
val entrypoint_receive_slice_from_burrow : checker * burrow_id -> (LigoOp.operation list * checker)

(*****************************************************************************)
(**                            {1 ORACLE}                                    *)
(*****************************************************************************)

(** Receive a price from the oracle. *)
val entrypoint_receive_price : checker * Ligo.nat -> (LigoOp.operation list * checker)

(*****************************************************************************)
(**                             {1 FA2}                                      *)
(*****************************************************************************)

val strict_entrypoint_transfer : checker * fa2_transfer list -> LigoOp.operation list * checker
val strict_entrypoint_balance_of : checker * fa2_balance_of_param -> LigoOp.operation list * checker
val entrypoint_update_operators : checker * fa2_update_operator list -> LigoOp.operation list * checker
