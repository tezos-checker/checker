
open Error

open Address
open FixedPoint
open Kit
open Parameters
open Tez
open Timestamp

(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
module Burrow : sig
  type t =
    { (* Whether the creation deposit for the burrow has been paid. If the
       * creation deposit has been paid, the burrow is considered "active" and
       * "closed"/inactive otherwise. Paying the creation deposit re-activates
       * a "closed" burrow. *)
      has_creation_deposit : bool;
      (* The owner of the burrow. Set once during creation. *)
      owner : Address.t;
      delegate : Address.t option;
      (* Collateral currently stored in the burrow. *)
      collateral : Tez.t;
      (* Outstanding kit minted out of the burrow. *)
      outstanding_kit : Kit.t;
      (* The imbalance adjustment index observed the last time the burrow was
       * touched. *)
      adjustment_index : FixedPoint.t;
      (* Collateral that has been sent off to auctions. For all intents and
       * purposes, this collateral can be considered gone, but depending on the
       * outcome of the auctions we expect some kit in return. *)
      collateral_at_auction : Tez.t;
      (* The last time the burrow was touched. *)
      (* TODO: George: in our past discussions we always assumed that this
       * field existed, but since we regulate the adjustment index (we actually
       * factor down the burrow_fee_index and the imbalance_index) to take into
       * account the time interval, do we actually need this here as well?
       * Perhaps it could save some gas in cases where the burrow is up-to-date
       * and we see that no bookkeeping is required. We'll see. *)
      last_touched : Timestamp.t;
    }

  type Error.error +=
    | Overburrowed of t
    | InsufficientFunds of Tez.t
    | WithdrawTezFailure
    | MintKitFailure

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fplus * kit_outstanding * minting_price
    *
    * The quantity tez_collateral / (fplus * minting_price) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit).
  *)
  val is_overburrowed : Parameters.t -> t -> bool

  (** NOTE: For testing only. Check whether a burrow is overburrowed, assuming
    * that all collateral that is in auctions at the moment will be sold at the
    * current minting price. *)
  val is_optimistically_overburrowed : Parameters.t -> t -> bool

  (** Check whether a burrow can be marked for liquidation. A burrow can be
    * marked for liquidation if:
    *
    *   tez_collateral < fminus * kit_outstanding * liquidation_price
    *
    * The quantity tez_collateral / (fminus * liquidation_price) we call the
    * liquidation limit. Note that for this check we optimistically take into
    * account the expected kit from pending auctions (using the current minting
    * price) when computing the outstanding kit. *)
  val is_liquidatable : Parameters.t -> t -> bool

  (** Perform housekeeping tasks on the burrow. This includes:
    * - Updating the outstanding kit to reflect accrued burrow fees and imbalance adjustment.
    * - Update the last observed adjustment index
    * - Update the last observed timestamp.
    * - NOTE: Are there any other tasks to put in this list?
  *)
  val touch : Parameters.t -> t -> t

  (** Given an address (owner) and amount of tez as collateral (including a
    * creation deposit, not counting towards that collateral), create a burrow.
    * Fail if the tez given is less than the creation deposit. *)
  val create : Parameters.t -> Address.t -> Tez.t -> (t, Error.error) result

  (** Add non-negative collateral to a burrow. *)
  val deposit_tez : Parameters.t -> Tez.t -> t -> t

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  val withdraw_tez : Parameters.t -> Tez.t -> t -> (t * Tez.t, Error.error) result

  (** Mint a non-negative amount of kit from the burrow, as long as this will
    * not overburrow it *)
  val mint_kit : Parameters.t -> Kit.t -> t -> (t * Kit.t, Error.error) result

  (** Deposit/burn a non-negative amount of kit to the burrow. Return any
    * excess kit balance. *)
  val burn_kit : Parameters.t -> Kit.t -> t -> t * Kit.t

  (** Compute the least number of tez that needs to be auctioned off (given the
    * current expected minting price) so that the burrow can return to a state
    * when it is no longer overburrowed or having a risk of liquidation. *)
  val compute_tez_to_auction : Parameters.t -> t -> Tez.t

  (** Given the number of tez to be auctioned off, compute the expected return
    * in kit, given the current minting price. *)
  val compute_expected_kit : Parameters.t -> Tez.t -> Kit.t
end = struct
  type t =
    { has_creation_deposit : bool;
      owner : Address.t;
      delegate : Address.t option;
      collateral : Tez.t;
      outstanding_kit : Kit.t;
      adjustment_index : FixedPoint.t;
      (* TODO: use this field in some calculations *)
      collateral_at_auction : Tez.t;
      last_touched : Timestamp.t;
    }
  [@@deriving show]

  type Error.error +=
    | Overburrowed of t
    | InsufficientFunds of Tez.t
    | WithdrawTezFailure
    | MintKitFailure

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fplus * kit_outstanding * minting_price
    *
    * The quantity tez_collateral / (fplus * minting_price) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit). NOTE: for the
    * purposes of minting/checking overburrowedness, we do not take into
    * account expected kit from pending auctions; for all we know, this could
    * be lost forever.
    * NOTE: IT ASSUMES THAT THE BURROW IS UP-TO-DATE (that touch has been called, that is)
  *)
  let is_overburrowed (p : Parameters.t) (b : t) : bool =
    Tez.to_fp b.collateral < FixedPoint.(Constants.fplus * Kit.to_fp b.outstanding_kit * Parameters.minting_price p)

  (* Update the outstanding kit, update the adjustment index, and the timestamp *)
  let touch (p: Parameters.t) (b: t) : t =
    { b with
      (* current_outstanding_kit = last_outstanding_kit * (adjustment_index / last_adjustment_index) *)
      outstanding_kit = Kit.of_fp FixedPoint.(Kit.to_fp b.outstanding_kit * Parameters.compute_adjustment_index p / b.adjustment_index);
      adjustment_index = Parameters.compute_adjustment_index p;
      last_touched = p.last_touched;
    }

  let create (p: Parameters.t) (address: Address.t) (tez: Tez.t) : (t, Error.error) result =
    if tez < Constants.creation_deposit
    then Error (InsufficientFunds tez)
    else Ok
        { has_creation_deposit = true;
          owner = address;
          delegate = None;
          collateral = Tez.(tez - Constants.creation_deposit);
          outstanding_kit = Kit.zero;
          adjustment_index = Parameters.compute_adjustment_index p;
          collateral_at_auction = Tez.zero;
          last_touched = p.last_touched; (* NOTE: If checker is up-to-date, the timestamp should be _now_. *)
        }

  (** Add non-negative collateral to a burrow. *)
  let deposit_tez (p: Parameters.t) (t: Tez.t) (burrow: t) : t =
    assert (t >= Tez.zero);
    let b = touch p burrow in
    { b with collateral = Tez.(b.collateral + t) }

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  let withdraw_tez (p: Parameters.t) (t: Tez.t) (burrow: t) : (t * Tez.t, Error.error) result =
    assert (t >= Tez.zero);
    let b = touch p burrow in
    let new_burrow = { b with collateral = Tez.(b.collateral - t) } in
    if is_overburrowed p new_burrow
    then Error WithdrawTezFailure
    else Ok (new_burrow, t)

  (** Mint a non-negative amount of kits from the burrow, as long as this will
    * not overburrow it *)
  let mint_kit (p: Parameters.t) (kit: Kit.t) (burrow: t) : (t * Kit.t, Error.error) result =
    assert (kit >= Kit.zero);
    let b = touch p burrow in
    let new_burrow = { b with outstanding_kit = Kit.(b.outstanding_kit + kit) } in
    if is_overburrowed p new_burrow
    then Error MintKitFailure
    else Ok (new_burrow, kit)

  (** Deposit/burn a non-negative amount of kit to the burrow. Return any
    * excess kit balance. *)
  let burn_kit (p: Parameters.t) (k: Kit.t) (burrow: t) : t * Kit.t =
    assert (k >= Kit.zero);
    let b = touch p burrow in
    let kit_to_burn = min b.outstanding_kit k in
    let kit_to_return = Kit.(k - kit_to_burn) in
    let new_burrow = { b with outstanding_kit = Kit.(b.outstanding_kit - kit_to_burn) } in
    (new_burrow, kit_to_return)

  (* ************************************************************************* *)
  (**                          LIQUIDATION-RELATED                             *)
  (* ************************************************************************* *)

  (** Compute the number of tez that needs to be auctioned off so that the burrow
    * can return to a state when it is no longer overburrowed or having a risk of
    * liquidation (assuming price minting_price). If we auction tez_to_auction,
    * and we receive repaid_kit for it, the following is expected to hold
    *
    *   tez_to_auction = repaid_kit * minting_price                            <=>
    *
    *   repaid_kit = tez_to_auction / minting_price                            (1)
    *
    * Furthermore, after liquidation, the burrow must not be neither
    * liquidatable, nor overburrowed anymore. Since by design the burrowing limit
    * is below the liquidation limit, during liquidation we target the burrowing
    * limit to ensure both are respected:
    *
    *   (tez - tez_to_auction) = (kit - repaid_kit) * fplus * minting_price   (2)
    *
    * Solving (1) and (2) gives:
    *
    *   tez_to_auction = (kit * fplus * minting_price - tez ) / (fplus - 1)
    *   repaid_kit     = tez_to_auction / minting_price
  *)
  (* TODO: Ensure that it's skewed on the safe side (overapprox.). It currently isn't. *)
  (* TODO: Shall we take into account the kit that we expect to receive from
   * the tez that currently lives in auction queues or not here? *)
  (* NOTE: This function computes the actual needed tez that needs to be
   * auctioned off to bring the burrow in a collateralized state. The 10%
   * majoration (as a penalty in case of a warranted liquidation) is added at
   * the call sites. *)
  let compute_tez_to_auction (p : Parameters.t) (b : t) : Tez.t =
    let collateral = Tez.to_fp b.collateral in
    let outstanding_kit = Kit.to_fp b.outstanding_kit in
    Tez.(scale one FixedPoint.((outstanding_kit * Constants.fplus * Parameters.minting_price p - collateral) / (Constants.fplus - one)))

  (* TODO: And ensure that it's skewed on the safe side (underapprox.). *)
  let compute_expected_kit (p : Parameters.t) (tez_to_auction: Tez.t) : Kit.t =
    Kit.of_fp FixedPoint.(Tez.to_fp tez_to_auction / Parameters.minting_price p)

  (** Check whether a burrow can be marked for liquidation. A burrow can be
    * marked for liquidation if:
    *
    *   tez_collateral < fminus * kit_outstanding * liquidation_price
    *
    * The quantity tez_collateral / (fminus * liquidation_price) we call the
    * liquidation limit. Note that for this check we optimistically take into
    * account the expected kit from pending auctions (using the current minting
    * price) when computing the outstanding kit.
    * NOTE: IT ASSUMES THAT THE BURROW IS UP-TO-DATE (that touch has been called, that is)
  *)
  let is_liquidatable (p : Parameters.t) (b : t) : bool =
    let expected_kit = compute_expected_kit p b.collateral_at_auction in
    let optimistic_outstanding = Kit.(b.outstanding_kit - expected_kit) in
    Tez.to_fp b.collateral < FixedPoint.(Constants.fminus * Kit.to_fp optimistic_outstanding * Parameters.liquidation_price p)

  (** NOTE: For testing only. Check whether a burrow is overburrowed, assuming
    * that all collateral that is in auctions at the moment will be sold at the
    * current minting price. *)
  let is_optimistically_overburrowed (p: Parameters.t) (b: t) : bool =
    let expected_kit = compute_expected_kit p b.collateral_at_auction in
    let optimistic_outstanding = Kit.(b.outstanding_kit - expected_kit) in
    Tez.to_fp b.collateral < FixedPoint.(Constants.fplus * Kit.to_fp optimistic_outstanding * Parameters.minting_price p)
end
