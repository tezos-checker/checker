open Ctok
open Kit
open Lqt
open Tok
open FixedPoint
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes
open CheckerTypes
open Fa2Interface

(** Perform housekeeping tasks on the contract state. This includes:
    - Updating the system parameters
    - Accruing burrowing fees to the cfmm
    - Updating auction-related info (completing an old / starting a new auction)
    - Processing a limited number of liquidation slices from completed auctions
    - Updating the index by consulting the oracle
*)
val entrypoint_touch : checker * unit -> (LigoOp.operation list * checker)

(**/**)
(* ONLY EXPOSED FOR TESTING REASONS. *)
val assert_checker_invariants : checker -> unit
val touch_with_index : checker -> fixedpoint -> (LigoOp.operation list * checker)
val calculate_touch_reward : Ligo.timestamp -> kit
val find_burrow : burrow_map -> burrow_id -> Burrow.burrow
val compute_outstanding_dissonance : checker -> kit (* "real" *) * kit (* approximation *)
(**/**)

(*****************************************************************************)
(**                            {1 BURROWS}                                   *)
(*****************************************************************************)

(** Create and return a new burrow containing the given amount of token as
    collateral, minus the creation deposit. Fail if the collateral given is not
    enough to cover the creation deposit, if the sender does not own said
    collateral, or if Checker is not authorized to transfer said collateral.

    Parameters:
    - A new ID to be used for the newly-created burrow, that is distinct from
      all previously-used ones for this burrow owner
    - An optional delegate address for the freshly-originated burrow contract
    - The amount of FA2 token to be deposited
*)
val entrypoint_create_burrow : checker * (Ligo.nat * Ligo.key_hash option * tok) -> (LigoOp.operation list * checker)

(** Deposit an amount of token as collateral. Fail if the burrow does not
    exist, if the sender does not own said collateral, or if Checker is not
    authorized to transfer said collateral.

    Parameters:
    - The ID of the burrow into which the collateral will be deposited
    - The amount of FA2 token to be deposited
*)
val entrypoint_deposit_collateral : checker * (Ligo.nat * tok) -> (LigoOp.operation list * checker)

(** Withdraw an amount of collateral from a burrow. Fail if the burrow does not
    exist, if this action would overburrow it, or if the sender is not the
    burrow owner.

    Parameters:
    - The ID of the burrow from which the collateral should be withdrawn
    - The amount of collateral to withdraw
*)
val entrypoint_withdraw_collateral : checker * (Ligo.nat * tok) -> (LigoOp.operation list * checker)

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

(** Activate a currently inactive burrow. Fail if the burrow does not exist, if
    the burrow is already active, if the amount of collateral given is not
    enough to cover the creation deposit, if the sender does not own said
    collateral, or if Checker is not authorized to transfer said collateral.

    Parameters:
    - The ID of the burrow to activate
    - The amount of FA2 token to be deposited as collateral (including the creation deposit)
*)
val entrypoint_activate_burrow : checker * (Ligo.nat * tok) -> (LigoOp.operation list * checker)

(** Deativate a currently active burrow. Fails if the burrow does not exist,
    if it is already inactive, if it is overburrowed, if it has kit
    outstanding, if it has collateral sent off to auctions, or if the sender is
    not the burrow owner. If deactivation is successful, make an FA2 payment to
    the given address.

    Parameters:
    - The ID of the burrow to deactivate
    - The address to make the FA2 transfer to
*)
val entrypoint_deactivate_burrow : checker * (Ligo.nat * Ligo.address) -> (LigoOp.operation list * checker)

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
    - The amount of ctok to be sold for kit
    - The minimum amount of kit expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_buy_kit : checker * (ctok * kit * Ligo.timestamp) -> LigoOp.operation list * checker

(** Sell some kit to the cfmm contract. Fail if the desired amount of ctok
    cannot be bought or if the deadline has passed.

    Parameters:
    - The amount of kit to be sold for ctok
    - The minimum amount of ctok expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_sell_kit : checker * (kit * ctok * Ligo.timestamp) -> LigoOp.operation list * checker

(** Buy some liquidity (liquidity tokens) from the cfmm contract, by
    giving it some ctok and some kit. If the given amounts do not have the
    right ratio, the cfmm contract keeps as much of the given ctok and kit
    as possible with the right ratio, and returns the leftovers, along with
    the liquidity tokens.

    Parameters:
    - The amount of ctok to be sold
    - The amount of kit to be sold
    - The minimum number of liquidity tokens expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_add_liquidity : checker * (ctok * kit * lqt * Ligo.timestamp) -> LigoOp.operation list * checker

(** Sell some liquidity (liquidity tokens) to the cfmm contract in
    exchange for the corresponding ctok and kit of the right ratio.

    Parameters:
    - The number of liquidity tokens to be sold
    - The minimum amount of ctok expected to be bought
    - The minimum amount of kit expected to be bought
    - The deadline for the transaction to be valid
*)
val entrypoint_remove_liquidity : checker * (lqt * ctok * kit * Ligo.timestamp) -> (LigoOp.operation list * checker)

(*****************************************************************************)
(**                      {1 LIQUIDATION AUCTIONS}                            *)
(*****************************************************************************)

(** Bid in the current liquidation auction. Fail if there is no ongoing
    auction, or if the bid is too low.

    Parameters:
    - identifier of the current liquidation auction
    - The amount of kit to be bid
*)
val entrypoint_liquidation_auction_place_bid : checker * (liquidation_auction_id * kit) -> LigoOp.operation list * checker

(** Claim the rewards of a completed liquidation auction. Fails if the sender
    is not the auction winner, if the auction is still ongoing, or if the
    completed auction still has unprocessed liquidation slices.

    Parameters:
    - The id of the completed auction
*)
val entrypoint_liquidation_auction_claim_win : checker * liquidation_auction_id -> LigoOp.operation list * checker

(*****************************************************************************)
(**                            {1 ORACLE}                                    *)
(*****************************************************************************)

(** (INTERNAL) Receive a price from the oracle.

    Parameters:
    - The current index of tok/chf or tok/ctok, as a fraction
*)
val entrypoint_receive_price : checker * (Ligo.nat * Ligo.nat) -> (LigoOp.operation list * checker)

(** (INTERNAL) Receive a price from the ctez (CFMM) contract.

    Parameters:
    - The current price of tez/ctez as a fraction
*)
val entrypoint_receive_ctez_marginal_price : checker * (Ligo.nat * Ligo.nat) -> (LigoOp.operation list * checker)

(*****************************************************************************)
(**                             {1 FA2}                                      *)
(*****************************************************************************)

val strict_entrypoint_transfer : checker * fa2_transfer list -> LigoOp.operation list * checker
val strict_entrypoint_balance_of : checker * fa2_balance_of_param -> LigoOp.operation list * checker
val entrypoint_update_operators : checker * fa2_update_operator list -> LigoOp.operation list * checker

(*****************************************************************************)
(**                            {1 VIEWS}                                     *)
(*****************************************************************************)

val view_buy_kit_min_kit_expected : (ctok * checker) -> kit
val view_sell_kit_min_ctok_expected : (kit * checker) -> ctok
val view_add_liquidity_max_kit_deposited : (ctok * checker) -> kit
val view_add_liquidity_min_lqt_minted : (ctok * checker) -> lqt
val view_remove_liquidity_min_ctok_withdrawn : (lqt * checker) -> ctok
val view_remove_liquidity_min_kit_withdrawn : (lqt * checker) -> kit

val view_current_liquidation_auction_details: (unit * checker) -> view_current_liquidation_auction_details_result
val view_burrow_max_mintable_kit : (burrow_id * checker) -> kit
val view_is_burrow_overburrowed : (burrow_id * checker) -> bool
val view_is_burrow_liquidatable : (burrow_id * checker) -> bool

val view_get_balance : ((Ligo.address * fa2_token_id) * checker) -> Ligo.nat
val view_total_supply : (fa2_token_id * checker) -> Ligo.nat
val view_all_tokens : (unit * checker) -> fa2_token_id list
val view_is_operator : ((Ligo.address * (Ligo.address * fa2_token_id)) * checker) -> bool
