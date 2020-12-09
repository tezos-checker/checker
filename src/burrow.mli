(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
type liquidation_slices =
  { oldest: Avl.leaf_ptr; youngest: Avl.leaf_ptr }

val show_liquidation_slices : liquidation_slices -> string
val pp_liquidation_slices : Format.formatter -> liquidation_slices -> unit

(** Representation of a burrow contract. *)
type t

val show : t -> string
val pp : Format.formatter -> t -> unit

(* Burrow API *)
val liquidation_slices : t -> liquidation_slices option
val set_liquidation_slices : t -> liquidation_slices option -> t
val collateral_at_auction : t -> Tez.t
val active : t -> bool

val permission_version : t -> int
val allow_all_tez_deposits : t -> bool
val allow_all_kit_burnings : t -> bool

val make_for_test :
    active:bool ->
    permission_version:int ->
    allow_all_tez_deposits:bool ->
    allow_all_kit_burnings:bool ->
    delegate:(Address.t option) ->
    collateral:Tez.t ->
    outstanding_kit:Kit.t ->
    excess_kit:Kit.t ->
    adjustment_index:FixedPoint.t ->
    collateral_at_auction:Tez.t ->
    liquidation_slices:(liquidation_slices option) ->
    last_touched:Timestamp.t ->
    t

type Error.error +=
  | InsufficientFunds of Tez.t
  | WithdrawTezFailure
  | MintKitFailure
  | BurrowIsAlreadyActive
  | DeactivatingAnOverburrowedBurrow
  | DeactivatingAnInactiveBurrow
  | DeactivatingWithOutstandingKit
  | DeactivatingWithCollateralAtAuctions

(** Check whether a burrow is overburrowed. A burrow is overburrowed if
  *
  *   tez_collateral < fminting * kit_outstanding * minting_price
  *
  * The quantity tez_collateral / (fminting * minting_price) we call the burrowing
  * limit (normally kit_outstanding <= burrowing_limit).
*)
val is_overburrowed : Parameters.t -> t -> bool

(** NOTE: For testing only. Check whether a burrow is overburrowed, assuming
  * that all collateral that is in auctions at the moment will be sold at the
  * current minting price, but that all these liquidations were actually
  * warranted. *)
val is_optimistically_overburrowed : Parameters.t -> t -> bool

(** Check whether a burrow can be marked for liquidation. A burrow can be
  * marked for liquidation if:
  *
  *   tez_collateral < fliquidation * kit_outstanding * liquidation_price
  *
  * The quantity tez_collateral / (fliquidation * liquidation_price) we call the
  * liquidation limit. Note that for this check we optimistically take into
  * account the expected kit from pending auctions (using the current minting
  * price) when computing the outstanding kit. Note that only active burrows
  * can be liquidated; inactive ones are dormant, until either all pending
  * auctions finish or if their creation deposit is restored. *)
val is_liquidatable : Parameters.t -> t -> bool

(** Perform housekeeping tasks on the burrow. This includes:
  * - Updating the outstanding kit to reflect accrued burrow fees and imbalance adjustment.
  * - Update the last observed adjustment index
  * - Update the last observed timestamp.
  * - Rebalance outstanding_kit/excess_kit
  * - NOTE: Are there any other tasks to put in this list?
*)
val touch : Parameters.t -> t -> t

(** Return some kit that we have received from an auction to the burrow. *)
val return_kit_from_auction : Tez.t -> Kit.t -> t -> t

(** Return some tez that was part of a liquidation slice back to the burrow
  * (due to a liquidation cancellation). *)
val return_tez_from_auction : Tez.t -> t -> t

(** Given an amount of tez as collateral (including a creation deposit, not
  * counting towards that collateral), create a burrow. Fail if the tez given
  * is less than the creation deposit. *)
val create : Parameters.t -> Tez.t -> (t, Error.error) result

(** Add non-negative collateral to a burrow. *)
val deposit_tez : Parameters.t -> Tez.t -> t -> t

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
val withdraw_tez : Parameters.t -> Tez.t -> t -> (t * Tez.t, Error.error) result

(** Mint a non-negative amount of kit from the burrow, as long as this will
  * not overburrow it *)
val mint_kit : Parameters.t -> Kit.t -> t -> (t * Kit.t, Error.error) result

(** Deposit/burn a non-negative amount of kit to the burrow. If there is
  * excess kit, simply store it into the burrow. *)
val burn_kit : Parameters.t -> Kit.t -> t -> t

(** Activate a currently inactive burrow. This operation will fail if either
  * the burrow is already active, or if the amount of tez given is less than
  * the creation deposit. *)
val activate : Parameters.t -> Tez.t -> t -> (t, Error.error) result

(** Deativate a currently active burrow. This operation will fail if the burrow
  * (a) is already inactive, or (b) is overburrowed, or (c) has kit
  * outstanding, or (d) has collateral sent off to auctions. *)
val deactivate : Parameters.t -> t -> (t * Tez.t, Error.error) result

(** Compute the least number of tez that needs to be auctioned off (given the
  * current expected minting price) so that the burrow can return to a state
  * when it is no longer overburrowed or having a risk of liquidation. *)
val compute_tez_to_auction : Parameters.t -> t -> Tez.t

(** Given the number of tez to be auctioned off, compute the expected return
  * in kit, given the current minting price. Assume that the liquidation was
  * warranted, so the liquidation penalty is subtracted *)
val compute_expected_kit : Parameters.t -> Tez.t -> Kit.t

(** Set the delegate of a burrow. *)
val set_delegate : Parameters.t -> Address.t -> t -> t

(* ************************************************************************* *)
(*                           Permission-related                              *)
(* ************************************************************************* *)

(** Requires admin. Sets whether or not to accept all tez deposits without
  * permissions. *)
val set_allow_all_tez_deposits : Parameters.t -> t -> bool -> t

(** Requires admin. Sets whether or not to accept all kit burns without
  * permissions. *)
val set_allow_all_kit_burns : Parameters.t -> t -> bool -> t

(** Requires admin. Increases the permission version so that all previous
  * permissions are now invalid. Returns the new permission version. *)
val increase_permission_version : Parameters.t -> t -> (int * t)

(* ************************************************************************* *)
(*                          Liquidation-related                              *)
(* ************************************************************************* *)
(* Some notes:
 * - Notes about the formulas live in docs/burrow-state-liquidations.md
 * - If we deplete the collateral then the next liquidation will close the burrow
 *   (unless the owner collateralizes it).
*)

type liquidation_details =
  { liquidation_reward : Tez.t;
    tez_to_auction : Tez.t;
    expected_kit : Kit.t;
    min_kit_for_unwarranted : Kit.t; (* If we get this many kit or more, the liquidation was unwarranted *)
    burrow_state : t;
  }

val show_liquidation_details : liquidation_details -> string
val pp_liquidation_details : Format.formatter -> liquidation_details -> unit

type liquidation_result =
  (* the burrow does not qualify for liquidation *)
  | Unnecessary
  (* partial: some collateral remains in the burrow *)
  | Partial of liquidation_details
  (* complete: deplete the collateral *)
  | Complete of liquidation_details
  (* complete: deplete the collateral AND the creation deposit *)
  | Close of liquidation_details

val show_liquidation_result : liquidation_result -> string
val pp_liquidation_result : Format.formatter -> liquidation_result -> unit

val request_liquidation : Parameters.t -> t -> liquidation_result
val oldest_liquidation_ptr : t -> Avl.leaf_ptr option

val assert_invariants : t -> unit
