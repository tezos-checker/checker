
open Error

open Address
open Constants
include Constants
open FixedPoint
open Kit
open Parameters
include Parameters
open Tez

(* TODOs for burrows:
   - Limit access to the representation of the burrow. Instead give access to
     contents through functions. For example currently
     Huxian.request_liquidation completely ignores the burrowing fee, the
     accumulated imbalance fee, etc. which is wrong.
*)

(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
module Burrow : sig
  type burrow =
    { (* Whether the creation deposit for the burrow has been paid. If the
       * creation deposit has been paid, the burrow is considered "active" and
       * "closed"/inactive otherwise. Paying the creation deposit re-activates
       * a "closed" burrow. *)
      has_creation_deposit : bool;
      (* The owner of the burrow. Set once during creation. *)
      owner : Address.t;
      delegate : Address.t option;
      (* Collateral currently stored in the burrow. *)
      collateral : Tez.t [@printer Tez.pp];
      (* Outstanding kit minted out of the burrow. *)
      minted_kit : Kit.t [@printer Kit.pp];
      (* The imbalance adjustment index observed the last time the burrow was
       * touched. *)
      adjustment_index : FixedPoint.t [@printer FixedPoint.pp];
      (* Collateral that has been sent off to auctions. For all intents and
       * purposes, this collateral can be considered gone, but depending on the
       * outcome of the auctions we expect some kit in return. *)
      collateral_at_auction : Tez.t [@printer Tez.pp];
      (* TODO: also keep track of the last time the burrow was touched *)
    }

  type Error.error +=
    | Overburrowed of burrow
    | InsufficientFunds of Tez.t
    | WithdrawTezFailure
    | MintKitFailure

  val show_burrow : burrow -> string
  val pp_burrow : Format.formatter -> burrow -> unit

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fplus * kit_outstanding * minting_price
    *
    * The quantity tez_collateral / (fplus * minting_price) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit).
  *)
  val is_overburrowed : parameters -> burrow -> bool

  (** Check whether a burrow can be marked for liquidation. A burrow can be
    * marked for liquidation if:
    *
    *   tez_collateral < fminus * kit_outstanding * liquidation_price
    *
    * The quantity tez_collateral / (fminus * liquidation_price) we call the
    * liquidation limit. Note that for this check we optimistically take into
    * account the expected kit from pending auctions (using the current minting
    * price) when computing the outstanding kit. *)
  val is_liquidatable : parameters -> burrow -> bool

  (** Ask the current outstanding number of kit of a burrow. Since the last
    * time the burrow was touched the balance can have changed, accruing burrow
    * fees and imbalance adjustments, so the amount of current outstanding kit
    * is thus computed
    *
    *   current_outstanding_kit = last_outstanding_kit * (adjustment_index / last_adjustment_index)
  *)
  val get_outstanding_kit : parameters -> burrow -> Kit.t

  (** Perform housekeeping tasks on the burrow. This includes:
    * - Updating the outstanding kit to reflect accrued burrow fees and imbalance adjustment.
    * - NOTE: Are there any other tasks to put in this list?
  *)
  val touch : parameters -> burrow -> burrow

  (** Given an address (owner) and amount of tez as collateral (including a
    * creation deposit, not counting towards that collateral), create a burrow.
    * Fail if the tez given is less than the creation deposit. *)
  val create_burrow : parameters -> Address.t -> Tez.t -> (burrow, Error.error) result

  (** Add non-negative collateral to a burrow. TODO: Pass a Tez.utxo instead? *)
  val deposit_tez : parameters -> Tez.t -> burrow -> burrow

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  val withdraw_tez : parameters -> Tez.t -> burrow -> (burrow * Tez.utxo, Error.error) result

  (** Mint a non-negative amount of kit from the burrow, as long as this will
    * not overburrow it *)
  val mint_kit : parameters -> Kit.t -> burrow -> (burrow * Kit.utxo, Error.error) result

  (** Deposit/burn a non-negative amount of kit to the burrow. Return any
    * excess kit balance. TODO: Pass a Kit.utxo instead? *)
  val burn_kit : parameters -> Kit.t -> burrow -> burrow * Kit.t

  (** Compute the least number of tez that needs to be auctioned off (given the
    * current expected minting price) so that the burrow can return to a state
    * when it is no longer overburrowed or having a risk of liquidation.
  *)
  val compute_tez_to_auction : parameters -> burrow -> Tez.t

  (** Given the number of tez to be auctioned off (as computed by
    * compute_tez_to_auction), compute the expected return in kit, given the
    * current minting price.
  *)
  val compute_expected_kit : parameters -> Tez.t -> Kit.t
end =
struct
  type burrow =
    { has_creation_deposit : bool;
      owner : Address.t;
      delegate : Address.t option;
      collateral : Tez.t [@printer Tez.pp];
      minted_kit : Kit.t [@printer Kit.pp];
      adjustment_index : FixedPoint.t [@printer FixedPoint.pp];
      (* TODO: use this field in some calculations *)
      collateral_at_auction : Tez.t [@printer Tez.pp];
    }
  [@@deriving show]

  type Error.error +=
    | Overburrowed of burrow
    | InsufficientFunds of Tez.t
    | WithdrawTezFailure
    | MintKitFailure

  (** Ask the current outstanding number of kit of a burrow. Since the last
    * time the burrow was touched the balance can have changed, accruing burrow
    * fees and imbalance adjustments, so the amount of current outstanding kit
    * is thus computed
    *
    *   current_outstanding_kit = last_outstanding_kit * (adjustment_index / last_adjustment_index)
  *)
  let get_outstanding_kit (p : parameters) (b : burrow) : Kit.t =
    Kit.of_fp FixedPoint.(Kit.to_fp b.minted_kit * compute_adjustment_index p / b.adjustment_index)

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
  let is_overburrowed (p : parameters) (b : burrow) : bool =
    Tez.to_fp b.collateral < FixedPoint.(fplus * Kit.to_fp b.minted_kit * minting_price p)

  (* Update the outstanding kit, update the adjustment index, TODO: and the timestamp? *)
  let touch (p: parameters) (b: burrow) : burrow =
    { b with
      minted_kit = get_outstanding_kit p b;
      adjustment_index = compute_adjustment_index p;
    }

  let create_burrow (p: parameters) (address: Address.t) (tez: Tez.t) : (burrow, Error.error) result =
    if tez < creation_deposit
    then Error (InsufficientFunds tez)
    else Ok
        { has_creation_deposit = true;
          owner = address;
          delegate = None;
          collateral = Tez.(tez - creation_deposit);
          minted_kit = Kit.zero;
          adjustment_index = compute_adjustment_index p;
          collateral_at_auction = Tez.zero;
        }

  (** Add non-negative collateral to a burrow. *)
  let deposit_tez (p: parameters) (t: Tez.t) (burrow: burrow) : burrow =
    assert (t >= Tez.zero);
    let b = touch p burrow in
    { b with collateral = Tez.(b.collateral + t) }

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  let withdraw_tez (p: parameters) (t: Tez.t) (burrow: burrow) : (burrow * Tez.utxo, Error.error) result =
    assert (t >= Tez.zero);
    let b = touch p burrow in
    let new_burrow = { b with collateral = Tez.(b.collateral - t) } in
    let tez_utxo = Tez.{ destination = b.owner; amount = t } in
    if is_overburrowed p new_burrow
    then Error WithdrawTezFailure
    else Ok (new_burrow, tez_utxo)

  (** Mint a non-negative amount of kits from the burrow, as long as this will
    * not overburrow it *)
  (* TODO: This should update the parameters; more kit is now in circulation! *)
  let mint_kit (p: parameters) (kit: Kit.t) (burrow: burrow) : (burrow * Kit.utxo, Error.error) result =
    assert (kit >= Kit.zero);
    let b = touch p burrow in
    let new_burrow = { b with minted_kit = Kit.(b.minted_kit + kit) } in
    let kit_utxo = Kit.{ destination = b.owner; amount = kit } in
    if is_overburrowed p new_burrow
    then Error MintKitFailure
    else Ok (new_burrow, kit_utxo)

  (** Deposit/burn a non-negative amount of kit to the burrow. Return any
    * excess kit balance. *)
  (* TODO: This should update the parameters; less kit is now in circulation! *)
  let burn_kit (p: parameters) (k: Kit.t) (burrow: burrow) : burrow * Kit.t =
    assert (k >= Kit.zero);
    let b = touch p burrow in
    let kit_to_burn = min b.minted_kit k in
    let kit_to_return = Kit.(k - kit_to_burn) in
    let new_burrow = { b with minted_kit = Kit.(b.minted_kit - kit_to_burn) } in
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
  (* TODO: Don't forget to add the 10% majoration here, so that we can take a
   * 10% penalty in case of a warranted liquidation. *)
  let compute_tez_to_auction (p : parameters) (b : burrow) : Tez.t =
    let collateral = Tez.to_fp b.collateral in
    let outstanding_kit = Kit.to_fp b.minted_kit in
    Tez.of_fp FixedPoint.((outstanding_kit * fplus * minting_price p - collateral) / (fplus - one))

  (* TODO: And ensure that it's skewed on the safe side (underapprox.). *)
  let compute_expected_kit (p : parameters) (tez_to_auction: Tez.t) : Kit.t =
    Kit.of_fp FixedPoint.(Tez.to_fp tez_to_auction / minting_price p)

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
  let is_liquidatable (p : parameters) (b : burrow) : bool =
    let expected_kit = compute_expected_kit p b.collateral_at_auction in
    let outstanding_kit = Kit.((get_outstanding_kit p b) - expected_kit) in
    Tez.to_fp b.collateral < FixedPoint.(fminus * Kit.to_fp outstanding_kit * liquidation_price p)
end
