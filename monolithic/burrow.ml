
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
      (* Collateral that has been sent off to auctions. For all intents and
       * purposes, this collateral can be considered gone, but depending on the
       * outcome of the auctions we expect some kit in return. *)
      auctioned_collateral : Tez.t [@printer Tez.pp];
      (* Accumulated burrow fee (in kit). NOTE: we should somehow keep track of
       * when was this last updated, and update accordingly, yet efficiently
       * (not in O(n)). *)
      accumulated_fee : Kit.t [@printer Kit.pp];
      (* Accumulated imbalance adjustment fee/bonus (in kit). NOTE: we should
       * somehow keep track of when was this last updated, and update
       * accordingly, yet efficiently (not in O(n)). *)
      accumulated_imbalance : Kit.t [@printer Kit.pp];
      (* TODO: Keep track also of the lot numbers for tez currently being
       * auctioned off. *)
    }

  type Error.error +=
    | Overburrowed of burrow
    | InsufficientFunds of Tez.t

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
    * liquidation limit.
  *)
  val is_liquidatable : parameters -> burrow -> bool

  (** Given an address (owner) and amount of tez as collateral (including a
    * creation deposit, not counting towards that collateral), create a burrow.
    * Fail if the tez given is less than the creation deposit. *)
  val create_burrow : Common.address -> Tez.t -> (burrow, Error.error) result

  (** A pretty much empty burrow. NOTE: This is just for testing. To create a
    * burrow we need more than that (at least 1 tez creation deposit, and a
    * valid address. *)
  val default_burrow : unit -> burrow

  (** Add non-negative collateral to a burrow. *)
  val deposit_tez : Tez.t -> burrow -> burrow

  (** Check whether a burrow is overburrowed. *)
  val overburrow_check : parameters -> burrow -> (burrow, Error.error) result

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  val withdraw_tez : parameters -> Tez.t -> burrow -> (burrow, Error.error) result

  (** Mint a non-negative amount of kit from the burrow, as long as this will
    * not overburrow it *)
  val mint_kit_from_burrow : parameters -> Kit.t -> burrow -> (burrow, Error.error) result

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
      auctioned_collateral : Tez.t [@printer Tez.pp];
      accumulated_fee : Kit.t [@printer Kit.pp];
      accumulated_imbalance : Kit.t [@printer Kit.pp];
    }
  [@@deriving show]

  type Error.error +=
    | Overburrowed of burrow
    | InsufficientFunds of Tez.t

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
    let outstanding_kit = Kit.add b.minted_kit (Kit.add b.accumulated_fee b.accumulated_imbalance) in
    Tez.to_fp b.collateral < FixedPoint.(fplus * Kit.to_fp outstanding_kit * minting_price p)

  let create_burrow (address: Common.address) (tez: Tez.t) : (burrow, Error.error) result =
    if tez < creation_deposit
    then Error (InsufficientFunds tez)
    else Ok
      { owner = address;
        delegate = None;
        collateral = Tez.sub tez creation_deposit;
        minted_kit = Kit.zero;
        auctioned_collateral = Tez.zero;
        accumulated_fee = Kit.zero;
        accumulated_imbalance = Kit.zero;
      }

  (** A pretty much empty burrow. NOTE: This is just for testing. To create a
    * burrow we need more than that (at least 1 tez creation deposit, and a
    * valid address. *)
  let default_burrow () : burrow =
    { owner = Common.of_string "";
      delegate = None;
      collateral = Tez.zero;
      minted_kit = Kit.zero;
      auctioned_collateral = Tez.zero;
      accumulated_fee = Kit.zero;
      accumulated_imbalance = Kit.zero;
    }

  (** Add non-negative collateral to a burrow. *)
  let deposit_tez (t : Tez.t) (b : burrow) : burrow =
    assert (t >= Tez.zero);
    { b with collateral = Tez.add b.collateral t }

  let overburrow_check  (p : parameters) (burrow : burrow) : (burrow, Error.error) result =
    if is_overburrowed p burrow
    then Error (Overburrowed burrow)
    else Ok burrow

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  let withdraw_tez (p : parameters) (t : Tez.t) (b : burrow) : (burrow, Error.error) result =
    assert (t >= Tez.zero);
    overburrow_check p { b with collateral = Tez.sub b.collateral t }

  (** Mint a non-negative amount of kits from the burrow, as long as this will
    * not overburrow it *)
  let mint_kit_from_burrow (p : parameters) (k : Kit.t) (b : burrow) =
    assert (k >= Kit.zero);
    overburrow_check p { b with minted_kit = Kit.add b.minted_kit k }

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
    * liquidation limit. NOTE: for the purposes of liquidation, we also take
    * into account expected kit from pending auctions, optimistically (using
    * the current minting price). *)
  let is_liquidatable (p : parameters) (b : burrow) : bool =
    let expected_kit = compute_expected_kit p b.auctioned_collateral in
    let outstanding_kit = Kit.sub (Kit.add b.minted_kit (Kit.add b.accumulated_fee b.accumulated_imbalance)) expected_kit in
    Tez.to_fp b.collateral < FixedPoint.(fminus * Kit.to_fp outstanding_kit * liquidation_price p)
end
