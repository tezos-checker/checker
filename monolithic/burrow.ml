
open Error

open Constants
include Constants
open FixedPoint
open Kit
open Parameters
include Parameters
open Tez

(* TODOs for burrows:
   - Add owner (delegate)
   - Add field for incurred fee
   - Fix all relevant interfaces (create, mint, burn, deposit, close)
*)

(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
module Burrow : sig
  type burrow =
    { collateral : Tez.t [@printer Tez.pp];
      minted_kit : Kit.t [@printer Kit.pp];
    }

  type Error.error +=
    | Overburrowed of burrow

  val show_burrow : burrow -> string
  val pp_burrow : Format.formatter -> burrow -> unit

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fplus * kit_outstanding * (q * tz_mint)
    *
    * The quantity tez_collateral / (fplus * (q * tz_mint)) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit).
  *)
  val is_overburrowed : parameters -> burrow -> bool

  (** Check whether a burrow can be marked for liquidation. A burrow can be
    * marked for liquidation if:
    *
    *   tez_collateral < fminus * kit_outstanding * (q * tz_liquidation)
    *
    * The quantity tez_collateral / (fminus * (q * tz_liquidation)) we call the
    * liquidation limit.
  *)
  val is_liquidatable : parameters -> burrow -> bool

  (** Create a burrow without any tez collateral or outstanding kit. *)
  val create_burrow : unit -> burrow

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
    { collateral : Tez.t [@printer Tez.pp];
      minted_kit : Kit.t [@printer Kit.pp];
    }
  [@@deriving show]

  type Error.error +=
    | Overburrowed of burrow

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fplus * kit_outstanding * (q * tz_mint)
    *
    * The quantity tez_collateral / (fplus * (q * tz_mint)) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit).
  *)
  let is_overburrowed (p : parameters) (b : burrow) : bool =
    Tez.to_fp b.collateral < FixedPoint.(fplus * Kit.to_fp b.minted_kit * (p.q * Tez.to_fp (tz_minting p)))

  (** Create a burrow without any tez collateral or outstanding kit. *)
  let create_burrow () : burrow =
    { collateral = Tez.of_float 0.0;
      minted_kit = Kit.of_float 0.0;
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
    *   tez_collateral < fminus * kit_outstanding * (q * tz_liquidation)
    *
    * The quantity tez_collateral / (fminus * (q * tz_liquidation)) we call the
    * liquidation limit.
  *)
  let is_liquidatable (p : parameters) (b : burrow) : bool =
    Tez.to_fp b.collateral < FixedPoint.(fminus * Kit.to_fp b.minted_kit * (p.q * Tez.to_fp (tz_liquidation p)))

  (** Compute the number of tez that needs to be auctioned off so that the burrow
    * can return to a state when it is no longer overburrowed or having a risk of
    * liquidation.
    *
    * The tez/kit price we expect to get when we liquidate is (q * tz_minting).
    * So if we auction tez_to_auction, and we receive repaid_kit for it, the
    * following is expected to hold
    *
    *   tez_to_auction = repaid_kit * (q * tz_minting)                         <=>
    *
    *   repaid_kit = tez_to_auction / (q * tz_minting)                         (1)
    *
    * Furthermore, after liquidation, the burrow must not be neither
    * liquidatable, nor overburrowed anymore. Since by design the burrowing limit
    * is below the liquidation limit, during liquidation we target the burrowing
    * limit to ensure both are respected:
    *
    *   (tez - tez_to_auction) = (kit - repaid_kit) * fplus * q * tz_minting   (2)
    *
    * Solving (1) and (2) gives:
    *
    *   tez_to_auction = (kit * fplus * q * tz_minting - tez ) / (fplus - 1)
    *   repaid_kit     = tez_to_auction / (q * tz_minting)
  *)
  (* TODO: Don't go through float, and ensure that it's skewed on the safe side (overapprox.). *)
  let compute_tez_to_auction (p : parameters) (b : burrow) : Tez.t =
    Tez.of_fp
      FixedPoint.((Kit.to_fp b.minted_kit * fplus * p.q * Tez.to_fp (tz_minting p) - Tez.to_fp b.collateral)
       / (fplus - FixedPoint.one))

  (* TODO: Don't go through float, and ensure that it's skewed on the safe side (underapprox.). *)
  let compute_expected_kit (p : parameters) (tez_to_auction: Tez.t) : Kit.t =
    Kit.of_fp FixedPoint.(Tez.to_fp tez_to_auction / (p.q * Tez.to_fp (tz_minting p)))
end
