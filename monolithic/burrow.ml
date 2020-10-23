
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
     contents through functions.
   - Fix (currently incorrect) calculation of is_overburrowed and
     is_liquidatable; they should take into account all the fields, not just
     minted_kit.
   - Fix/add all relevant interfaces (create, mint, burn, deposit, close)
*)

(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
module Burrow : sig
  type burrow =
    { (* The owner of the burrow. Set once during creation. *)
      owner : Common.address;
      delegate : Common.address option;
      (* Confirmed collateral currently stored in the burrow. *)
      collateral : Tez.t [@printer Tez.pp];
      (* Outstanding kit minted out of the burrow. *)
      minted_kit : Kit.t [@printer Kit.pp];
      (* Kit expected to be received from auctions; not confirmed yet. NOTE:
       * Calculated when the corresponding collateral was sent to auction;
       * prices might have changed in the meantime. *)
      expected_kit : Kit.t [@printer Kit.pp];
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
      expected_kit : Kit.t [@printer Kit.pp];
      accumulated_fee : Kit.t [@printer Kit.pp];
      accumulated_imbalance : Kit.t [@printer Kit.pp];
    }
  [@@deriving show]

  type Error.error +=
    | Overburrowed of burrow

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fplus * kit_outstanding * minting_price
    *
    * The quantity tez_collateral / (fplus * minting_price) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit).
  *)
  let is_overburrowed (p : parameters) (b : burrow) : bool =
    Tez.to_fp b.collateral < FixedPoint.(fplus * Kit.to_fp b.minted_kit * minting_price p)

  (** A pretty much empty burrow. NOTE: This is just for testing. To create a
    * burrow we need more than that (at least 1 tez creation deposit, and a
    * valid address. *)
  let default_burrow () : burrow =
    { owner = Common.of_string "";
      delegate = None;
      collateral = Tez.zero;
      minted_kit = Kit.zero;
      expected_kit = Kit.zero;
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

  (** Check whether a burrow can be marked for liquidation. A burrow can be
    * marked for liquidation if:
    *
    *   tez_collateral < fminus * kit_outstanding * liquidation_price
    *
    * The quantity tez_collateral / (fminus * liquidation_price) we call the
    * liquidation limit.
  *)
  let is_liquidatable (p : parameters) (b : burrow) : bool =
    Tez.to_fp b.collateral < FixedPoint.(fminus * Kit.to_fp b.minted_kit * liquidation_price p)

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
  let compute_tez_to_auction (p : parameters) (b : burrow) : Tez.t =
    Tez.of_fp
      FixedPoint.((Kit.to_fp b.minted_kit * fplus * minting_price p - Tez.to_fp b.collateral)
       / (fplus - FixedPoint.one))

  (* TODO: Don't go through float, and ensure that it's skewed on the safe side (underapprox.). *)
  let compute_expected_kit (p : parameters) (tez_to_auction: Tez.t) : Kit.t =
    Kit.of_fp FixedPoint.(Tez.to_fp tez_to_auction / minting_price p)
end
