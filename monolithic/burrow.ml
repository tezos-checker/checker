
open Error

open Common
open Constants
include Constants
open FixedPoint
open Kit
open Parameters
include Parameters
open Tez

include Common

(* TODOs for burrows:
   - Limit access to the representation of the burrow. Instead give access to
     contents through functions. For example currently
     Huxian.request_liquidation completely ignores the burrowing fee, the
     accumulated imbalance fee, etc. which is wrong.
   - Fix/add all relevant interfaces (create, mint, burn, deposit, close)
   - Remove accumulated_fee and accumulated_imbalance from the burrow state.
     Instead, whenever you touch the burrow, the minted_kit state variable will
     be changed to reflect the accumulated fee and accumulated imbalance. What
     you actually need to need store as part of the burrow state is the last
     time it was touched, and what the global imbalance index was at the time.
     Note: Figure out how to extrapolate for this calculation (we don't want to
     be too far off I assume, but we cannot depend on all intermediate values I
     either; that'd be too expensive.
*)

(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
module Burrow : sig
  type burrow =
    { (* The owner of the burrow. Set once during creation. *)
      owner : Common.address;
      delegate : Common.address option;
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
      auctioned_collateral : Tez.t [@printer Tez.pp];
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
  val get_minted_kit : parameters -> burrow -> Kit.t

  (** Given an address (owner) and amount of tez as collateral (including a
    * creation deposit, not counting towards that collateral), create a burrow.
    * Fail if the tez given is less than the creation deposit. *)
  val create_burrow : parameters -> Common.address -> Tez.t -> (burrow, Error.error) result

  (** A pretty much empty burrow. NOTE: This is just for testing. To create a
    * burrow we need more than that (at least 1 tez creation deposit, and a
    * valid address. *)
  val default_burrow : unit -> burrow

  (** Add non-negative collateral to a burrow. TODO: Pass a Tez.utxo instead? *)
  val deposit_tez : Tez.t -> burrow -> burrow

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  val withdraw_tez : parameters -> Tez.t -> burrow -> (burrow * Tez.utxo, Error.error) result

  (** Mint a non-negative amount of kit from the burrow, as long as this will
    * not overburrow it *)
  val mint_kit : parameters -> Kit.t -> burrow -> (burrow * Kit.utxo, Error.error) result

  (** Deposit/burn a non-negative amount of kit to the burrow. Return any
    * excess kit balance. TODO: Pass a Kit.utxo instead? *)
  val burn_kit : Kit.t -> burrow -> burrow * Kit.t

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
    { owner : Common.address;
      delegate : Common.address option;
      collateral : Tez.t [@printer Tez.pp];
      minted_kit : Kit.t [@printer Kit.pp];
      adjustment_index : FixedPoint.t [@printer FixedPoint.pp];
      auctioned_collateral : Tez.t [@printer Tez.pp];
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
  (* TODO: shall we update the burrow to reflect the change here? *)
  let get_minted_kit (p : parameters) (b : burrow) : Kit.t =
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
  *)
  let is_overburrowed (p : parameters) (b : burrow) : bool =
    let outstanding_kit = get_minted_kit p b in
    Tez.to_fp b.collateral < FixedPoint.(fplus * Kit.to_fp outstanding_kit * minting_price p)

  let create_burrow (p: parameters) (address: Common.address) (tez: Tez.t) : (burrow, Error.error) result =
    if tez < creation_deposit
    then Error (InsufficientFunds tez)
    else Ok
      { owner = address;
        delegate = None;
        collateral = Tez.sub tez creation_deposit;
        minted_kit = Kit.zero;
        adjustment_index = compute_adjustment_index p;
        auctioned_collateral = Tez.zero;
      }

  (** A pretty much empty burrow. NOTE: This is just for testing. To create a
    * burrow we need more than that (at least 1 tez creation deposit, and a
    * valid address. *)
  let default_burrow () : burrow =
    { owner = Common.of_string "";
      delegate = None;
      collateral = Tez.zero;
      minted_kit = Kit.zero;
      adjustment_index = FixedPoint.one;
      auctioned_collateral = Tez.zero;
    }

  (** Add non-negative collateral to a burrow. *)
  let deposit_tez (t : Tez.t) (b : burrow) : burrow =
    assert (t >= Tez.zero);
    { b with collateral = Tez.add b.collateral t }

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  let withdraw_tez (p : parameters) (t : Tez.t) (b : burrow) : (burrow * Tez.utxo, Error.error) result =
    assert (t >= Tez.zero);
    let new_burrow = { b with collateral = Tez.sub b.collateral t } in
    let tez_utxo = Tez.{ destination = b.owner; amount = t } in
    if is_overburrowed p new_burrow
    then Error WithdrawTezFailure
    else Ok (new_burrow, tez_utxo)

  (** Mint a non-negative amount of kits from the burrow, as long as this will
    * not overburrow it *)
  let mint_kit (p : parameters) (k : Kit.t) (b : burrow) : (burrow * Kit.utxo, Error.error) result =
    assert (k >= Kit.zero);
    let new_burrow = { b with minted_kit = Kit.add b.minted_kit k } in
    let kit_utxo = Kit.{ destination = b.owner; amount = k } in
    if is_overburrowed p new_burrow
    then Error MintKitFailure
    else Ok (new_burrow, kit_utxo)

  (** Deposit/burn a non-negative amount of kit to the burrow. Return any
    * excess kit balance. *)
  let burn_kit (k : Kit.t) (b : burrow) : burrow * Kit.t =
    assert (k >= Kit.zero);
    let kit_to_burn = min b.minted_kit k in
    let kit_to_return = Kit.sub k kit_to_burn in
    let new_burrow = { b with minted_kit = Kit.sub b.minted_kit kit_to_burn } in
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
  (* TODO: Don't go through float, and ensure that it's skewed on the safe side (overapprox.). *)
  (* TODO: Shall we take into account the kit that we expect to receive from
   * the tez that currently lives in auction queues or not here? *)
  let compute_tez_to_auction (p : parameters) (b : burrow) : Tez.t =
    Tez.of_fp
      FixedPoint.((Kit.to_fp b.minted_kit * fplus * minting_price p - Tez.to_fp b.collateral)
       / (fplus - FixedPoint.one))

  (* TODO: Don't go through float, and ensure that it's skewed on the safe side (underapprox.). *)
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
    * price) when computing the outstanding kit. *)
  (* TODO: Should we compute imbalance adjustment and burrow fee on the
   * expected_kit too or not? *)
  let is_liquidatable (p : parameters) (b : burrow) : bool =
    let expected_kit = compute_expected_kit p b.auctioned_collateral in
    let outstanding_kit = Kit.sub (get_minted_kit p b) expected_kit in
    Tez.to_fp b.collateral < FixedPoint.(fminus * Kit.to_fp outstanding_kit * liquidation_price p)
end
