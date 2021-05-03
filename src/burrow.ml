open FixedPoint
open Ratio
open Kit
open Parameters
open LiquidationAuctionPrimitiveTypes
open Constants
open Error
open Common

type burrow =
  { (* Whether the creation deposit for the burrow has been paid. If the
     * creation deposit has been paid, the burrow is considered "active" and
     * "closed"/inactive otherwise. Paying the creation deposit re-activates
     * a "closed" burrow. *)
    active : bool;
    (* Address of the contract holding burrows collaeral. *)
    address: Ligo.address;
    (* The delegate for the tez (collateral + creation_deposit) the burrow
     * holds. *)
    delegate : Ligo.key_hash option;
    (* Collateral currently stored in the burrow. *)
    collateral : Ligo.tez;
    (* Outstanding kit minted out of the burrow. *)
    outstanding_kit : kit;
    (* Excess kit returned by auctions. *)
    excess_kit : kit;
    (* The imbalance adjustment index observed the last time the burrow was
     * touched. *)
    adjustment_index : fixedpoint;
    (* Collateral that has been sent off to auctions. For all intents and
     * purposes, this collateral can be considered gone, but depending on the
     * outcome of the auctions we expect some kit in return. *)
    collateral_at_auction : Ligo.tez;
    (* The last time the burrow was touched. *)
    last_touched : Ligo.timestamp;
  }
[@@deriving show]

type liquidation_details =
  { liquidation_reward : Ligo.tez;
    tez_to_auction : Ligo.tez;
    burrow_state : burrow;
  }
[@@deriving show]

type liquidation_type =
  (* partial: some collateral remains in the burrow *)
  | Partial
  (* complete: deplete the collateral *)
  | Complete
  (* complete: deplete the collateral AND the creation deposit *)
  | Close
[@@deriving show]

type liquidation_result = (liquidation_type * liquidation_details) option
[@@deriving show]

let[@inline] ensure_uptodate_burrow (p: parameters) (b: burrow) : unit =
  if p.last_touched = b.last_touched
  then ()
  else (Ligo.failwith error_OperationOnUntouchedBurrow : unit)

let[@inline] assert_burrow_invariants (_b: burrow) : unit =
  assert (_b.outstanding_kit = kit_zero || _b.excess_kit = kit_zero);
  ()

let[@inline] burrow_address (b: burrow) : Ligo.address =
  assert_burrow_invariants b;
  b.address

(** Computes the total amount of tez associated with a burrow. This includes
  * the collateral, collateral_at_auction, and the creation_deposit if the
  * burrow is active. *)
let burrow_total_associated_tez (b: burrow) : Ligo.tez =
  Ligo.add_tez_tez
    (Ligo.add_tez_tez b.collateral b.collateral_at_auction)
    (if b.active then creation_deposit else Ligo.tez_from_literal "0mutez")

let[@inline] burrow_collateral_at_auction (b: burrow) : Ligo.tez =
  assert_burrow_invariants b;
  b.collateral_at_auction

(** Under-collateralization condition: tez < f * kit * price. *)
let[@inline] undercollateralization_condition (f: ratio) (price: ratio) (tez: ratio) (kit: ratio) : bool =
  let { num = num_f; den = den_f; } = f in
  let { num = num_p; den = den_p; } = price in
  let { num = num_tz; den = den_tz; } = tez in
  let { num = num_kt; den = den_kt; } = kit in
  let lhs =
    Ligo.mul_int_int
      (Ligo.mul_int_int num_tz den_f)
      (Ligo.mul_int_int den_kt den_p) in
  let rhs =
    Ligo.mul_int_int
      (Ligo.mul_int_int num_f num_kt)
      (Ligo.mul_int_int den_tz num_p) in
  Ligo.lt_int_int lhs rhs

(** Check whether a burrow is overburrowed. A burrow is overburrowed if
  *
  *   tez_collateral < fminting * kit_outstanding * minting_price
  *
  * The quantity tez_collateral / (fminting * minting_price) we call the burrowing
  * limit (normally kit_outstanding <= burrowing_limit). NOTE: for the
  * purposes of minting/checking overburrowedness, we do not take into
  * account expected kit from pending auctions; for all we know, this could
  * be lost forever.
*)
let burrow_is_overburrowed (p : parameters) (b : burrow) : bool =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let tez = { num = tez_to_mutez b.collateral; den = Ligo.int_from_literal "1_000_000"; } in
  let kit = { num = kit_to_mukit_int b.outstanding_kit; den = kit_scaling_factor_int; } in
  undercollateralization_condition fminting (minting_price p) tez kit

(** Rebalance the kit inside the burrow so that either outstanding_kit is zero
  * or b.outstanding_kit is zero. *)
let rebalance_kit (b: burrow) : burrow =
  let kit_to_move = kit_min b.outstanding_kit b.excess_kit in
  { b with
    outstanding_kit = kit_sub b.outstanding_kit kit_to_move;
    excess_kit = kit_sub b.excess_kit kit_to_move;
  }

(** Update the outstanding kit (and excess_kit), update the adjustment index,
  * and the timestamp. When updating the outstanding kit we round up, to
  * avoid someone touching their burrow all the time to prevent the fee from
  * accumulating. *)
let burrow_touch (p: parameters) (burrow: burrow) : burrow =
  assert_burrow_invariants burrow;
  let burrow_out = if p.last_touched = burrow.last_touched
    then
      burrow
    else
      let current_adjustment_index = compute_adjustment_index p in
      { burrow with
        outstanding_kit =
          kit_of_fraction_ceil
            (Ligo.mul_int_int
               (kit_to_mukit_int burrow.outstanding_kit)
               (fixedpoint_to_raw current_adjustment_index)
            )
            (Ligo.mul_int_int
               kit_scaling_factor_int
               (fixedpoint_to_raw burrow.adjustment_index)
            );
        adjustment_index = current_adjustment_index;
        last_touched = p.last_touched;
      }
  in
  burrow_out

let burrow_return_slice_from_auction
    (slice: liquidation_slice_contents)
    (burrow: burrow)
  : burrow =
  assert_burrow_invariants burrow;
  assert burrow.active;
  assert (burrow.collateral_at_auction >= slice.tez);
  let burrow_out =
    { burrow with
      collateral = Ligo.add_tez_tez burrow.collateral slice.tez;
      collateral_at_auction = Ligo.sub_tez_tez burrow.collateral_at_auction slice.tez;
    } in
  burrow_out

let burrow_return_kit_from_auction
    (slice: liquidation_slice_contents)
    (kit: kit)
    (burrow: burrow) : burrow =
  assert_burrow_invariants burrow;
  assert (burrow.collateral_at_auction >= slice.tez);
  let burrow_out =
    rebalance_kit
      { burrow with
        excess_kit = kit_add burrow.excess_kit kit;
        collateral_at_auction = Ligo.sub_tez_tez burrow.collateral_at_auction slice.tez;
      }
  in
  burrow_out

let burrow_create (p: parameters) (addr: Ligo.address) (tez: Ligo.tez) (delegate_opt: Ligo.key_hash option) : burrow =
  if tez < creation_deposit
  then (Ligo.failwith error_InsufficientFunds : burrow)
  else
    { active = true;
      address = addr;
      delegate = delegate_opt;
      collateral = Ligo.sub_tez_tez tez creation_deposit;
      outstanding_kit = kit_zero;
      excess_kit = kit_zero;
      adjustment_index = compute_adjustment_index p;
      collateral_at_auction = Ligo.tez_from_literal "0mutez";
      last_touched = p.last_touched; (* NOTE: If checker is up-to-date, the timestamp should be _now_. *)
    }

(** Add non-negative collateral to a burrow. *)
let[@inline] burrow_deposit_tez (p: parameters) (t: Ligo.tez) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow_out = { b with collateral = Ligo.add_tez_tez b.collateral t } in
  burrow_out

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let burrow_withdraw_tez (p: parameters) (t: Ligo.tez) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow = { b with collateral = Ligo.sub_tez_tez b.collateral t } in
  let burrow_out = if burrow_is_overburrowed p burrow
    then (Ligo.failwith error_WithdrawTezFailure : burrow)
    else burrow
  in
  burrow_out

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let burrow_mint_kit (p: parameters) (kit: kit) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow_out =
    let burrow = rebalance_kit { b with outstanding_kit = kit_add b.outstanding_kit kit } in
    if burrow_is_overburrowed p burrow
    then (Ligo.failwith error_MintKitFailure : burrow)
    else burrow
  in
  burrow_out

(** Deposit/burn a non-negative amount of kit to the burrow. If there is
  * excess kit, simply store it into the burrow. *)
let[@inline] burrow_burn_kit (p: parameters) (k: kit) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow_out = rebalance_kit { b with excess_kit = kit_add b.excess_kit k } in
  burrow_out

(** Activate a currently inactive burrow. This operation will fail if either
  * the burrow is already active, or if the amount of tez given is less than
  * the creation deposit. *)
let burrow_activate (p: parameters) (tez: Ligo.tez) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow_out =
    if tez < creation_deposit then
      (Ligo.failwith error_InsufficientFunds : burrow)
    else if b.active then
      (Ligo.failwith error_BurrowIsAlreadyActive : burrow)
    else
      { b with
        active = true;
        collateral = Ligo.sub_tez_tez tez creation_deposit;
      }
  in
  burrow_out

(** Deativate a currently active burrow. This operation will fail if the burrow
  * (a) is already inactive, or (b) is overburrowed, or (c) has kit
  * outstanding, or (d) has collateral sent off to auctions. *)
let burrow_deactivate (p: parameters) (b: burrow) : (burrow * Ligo.tez) =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow_out, return =
    if burrow_is_overburrowed p b then
      (Ligo.failwith error_DeactivatingAnOverburrowedBurrow : (burrow * Ligo.tez))
    else if (not b.active) then
      (Ligo.failwith error_DeactivatingAnInactiveBurrow : (burrow * Ligo.tez))
    else if (b.outstanding_kit > kit_zero) then
      (Ligo.failwith error_DeactivatingWithOutstandingKit : (burrow * Ligo.tez))
    else if (b.collateral_at_auction > Ligo.tez_from_literal "0mutez") then
      (Ligo.failwith error_DeactivatingWithCollateralAtAuctions : (burrow * Ligo.tez))
    else
      let return = Ligo.add_tez_tez b.collateral creation_deposit in
      let updated_burrow =
        { b with
          active = false;
          collateral = Ligo.tez_from_literal "0mutez";
        } in
      (updated_burrow, return)
  in
  burrow_out, return

let burrow_set_delegate (p: parameters) (new_delegate: Ligo.key_hash option) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow_out = { b with delegate = new_delegate; } in
  burrow_out

(* ************************************************************************* *)
(**                          LIQUIDATION-RELATED                             *)
(* ************************************************************************* *)

(** Compute the number of tez that needs to be auctioned off so that the burrow
  * can return to a state when it is no longer overburrowed or having a risk of
  * liquidation (assuming the current expected minting price). For its
  * calculation, see docs/burrow-state-liquidations.md.  Note that it's skewed
  * on the safe side (overapproximation). This ensures that after a partial
  * liquidation we are no longer "optimistically overburrowed".
  * Returns the number of tez in mutez *)
let compute_tez_to_auction (p: parameters) (b: burrow) : Ligo.int =
  assert_burrow_invariants b;

  let { num = num_fm; den = den_fm; } = fminting in
  let { num = num_mp; den = den_mp; } = minting_price p in
  (* Note that num_lp and den_lp here are actually = 1 - liquidation_penalty *)
  let { num = num_lp; den = den_lp; } =
    let { num = num_lp; den = den_lp; } = liquidation_penalty in
    { num = Ligo.sub_int_int den_lp num_lp; den = den_lp; }
  in

  (* numerator = tez_sf * den_lp * num_fm * num_mp * outstanding_kit
     - kit_sf * den_mp * (num_lp * num_fm * collateral_at_auctions + den_lp * den_fm * collateral) *)
  let numerator =
    Ligo.sub_int_int
      (Ligo.mul_int_int
         (Ligo.int_from_literal "1_000_000")
         (Ligo.mul_int_int
            den_lp
            (Ligo.mul_int_int
               num_fm
               (Ligo.mul_int_int
                  num_mp
                  (kit_to_mukit_int b.outstanding_kit)
               )
            )
         )
      )
      (Ligo.mul_int_int
         (Ligo.mul_int_int kit_scaling_factor_int den_mp)
         (Ligo.add_int_int
            (Ligo.mul_int_int num_lp (Ligo.mul_int_int num_fm (tez_to_mutez b.collateral_at_auction)))
            (Ligo.mul_int_int den_lp (Ligo.mul_int_int den_fm (tez_to_mutez b.collateral)))
         )
      ) in
  (* denominator = (kit_sf * den_mp * tez_sf) * (num_lp * num_fm - den_lp * den_fm) *)
  let denominator =
    Ligo.mul_int_int
      kit_scaling_factor_int
      (Ligo.mul_int_int
         den_mp
         (Ligo.mul_int_int
            (Ligo.int_from_literal "1_000_000")
            (Ligo.sub_int_int
               (Ligo.mul_int_int num_lp num_fm)
               (Ligo.mul_int_int den_lp den_fm)
            )
         )
      ) in
  cdiv_int_int (Ligo.mul_int_int numerator (Ligo.int_from_literal "1_000_000")) denominator

(** Compute the amount of kit we expect to receive from auctioning off an
  * amount of tez, using the current minting price. Since this is an artifice,
  * a mere expectation, we neither floor nor ceil, but instead return the
  * lossless fraction as is. *)
let compute_expected_kit (p: parameters) (tez_to_auction: Ligo.tez) : ratio =
  let { num = num_lp; den = den_lp; } = liquidation_penalty in
  let { num = num_mp; den = den_mp; } = minting_price p in
  let numerator =
    Ligo.mul_int_int
      (tez_to_mutez tez_to_auction)
      (Ligo.mul_int_int
         (Ligo.sub_int_int den_lp num_lp)
         den_mp
      ) in
  let denominator =
    Ligo.mul_int_int
      (Ligo.int_from_literal "1_000_000")
      (Ligo.mul_int_int den_lp num_mp) in
  { num = numerator; den = denominator; }

(** Check whether a burrow can be marked for liquidation. A burrow can be
  * marked for liquidation if:
  *
  *   tez_collateral < fliquidation * (kit_outstanding - expected_kit_from_auctions) * liquidation_price
  *
  * The quantity tez_collateral / (fliquidation * liquidation_price) we call the
  * liquidation limit. Note that for this check we optimistically take into
  * account the expected kit from pending auctions (using the current minting
  * price) when computing the outstanding kit. Note that only active burrows
  * can be liquidated; inactive ones are dormant, until either all pending
  * auctions finish or if their creation deposit is restored. *)
let burrow_is_liquidatable (p: parameters) (b: burrow) : bool =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;

  let tez = { num = tez_to_mutez b.collateral; den = Ligo.int_from_literal "1_000_000"; } in
  let kit = (* kit = kit_outstanding - expected_kit_from_auctions *)
    let { num = num_ek; den = den_ek; } = compute_expected_kit p b.collateral_at_auction in
    { num =
        Ligo.sub_int_int
          (Ligo.mul_int_int (kit_to_mukit_int b.outstanding_kit) den_ek)
          (Ligo.mul_int_int kit_scaling_factor_int num_ek);
      den = Ligo.mul_int_int kit_scaling_factor_int den_ek;
    } in
  b.active && undercollateralization_condition fliquidation (liquidation_price p) tez kit

(** Check whether the return of a slice to its burrow (cancellation) is
  * warranted. For the cancellation to be warranted, it must be the case that
  * after returning the slice to the burrow, the burrow is optimistically
  * non-overburrowed (i.e., if all remaining collateral at auction sells at the
  * current price but with penalties paid, the burrow becomes underburrowed):
  *
  *   collateral + slice >= fminting * (outstanding - compute_expected_kit (collateral_at_auction - slice)) * minting_price
  *
  * Note that only active burrows can be liquidated; inactive ones are dormant,
  * until either all pending auctions finish or if their creation deposit is
  * restored. *)
let burrow_is_cancellation_warranted (p: parameters) (b: burrow) (slice_tez: Ligo.tez) : bool =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  assert (Ligo.geq_tez_tez b.collateral_at_auction slice_tez);

  let tez = (* tez = collateral + slice *)
    { num = tez_to_mutez (Ligo.add_tez_tez b.collateral slice_tez);
      den = Ligo.int_from_literal "1_000_000";
    } in
  let kit = (* kit = outstanding - compute_expected_kit (collateral_at_auction - slice) *)
    let { num = num_ek; den = den_ek; } =
      compute_expected_kit p (Ligo.sub_tez_tez b.collateral_at_auction slice_tez) in
    { num =
        Ligo.sub_int_int
          (Ligo.mul_int_int (kit_to_mukit_int b.outstanding_kit) den_ek)
          (Ligo.mul_int_int kit_scaling_factor_int num_ek);
      den = Ligo.mul_int_int kit_scaling_factor_int den_ek;
    } in

  b.active && not (undercollateralization_condition fminting (minting_price p) tez kit)

(** Compute the minumum amount of kit to receive for considering the
  * liquidation unwarranted, calculated as (see
  * docs/burrow-state-liquidations.md for the derivation of this formula):
  *
  *   tez_to_auction * (fliquidation * (outstanding_kit - expected_kit_from_auctions)) / collateral
  *
  * If the burrow has no collateral left in it (e.g., right after a successful
  * Complete-liquidation) then we have two cases:
  * (a) If the outstanding kit is non-zero then there is no way for this
  *     liquidation to be considered unwarranted. outstanding_kit is infinitely
  *     many times greater than the collateral.
  * (b) If the outstanding kit is also zero then the liquidation in question
  *     shouldn't have happened (so it is by definition unwarranted). I think
  *     that this is impossible in practice, but it's probably best to account
  *     for it so that the function is not partial.
*)
let[@inline] compute_min_kit_for_unwarranted (p: parameters) (b: burrow) (tez_to_auction: Ligo.tez) : kit option =
  let _ = ensure_uptodate_burrow p b in

  if b.collateral = Ligo.tez_from_literal "0mutez" (* NOTE: division by zero. *)
  then
    if b.outstanding_kit <> kit_of_mukit (Ligo.nat_from_literal "0n")
    then (None: kit option) (* (a): infinity, basically *)
    else (Some kit_zero) (* (b): zero *)
  else
    let { num = num_fl; den = den_fl; } = fliquidation in
    let { num = num_ek; den = den_ek; } = compute_expected_kit p b.collateral_at_auction in

    (* numerator = max 0 (tez_to_auction * num_fl * (den_ek * outstanding_kit - kit_sf * num_ek)) *)
    let numerator =
      let numerator =
        Ligo.mul_int_int
          (Ligo.mul_int_int (tez_to_mutez tez_to_auction) num_fl)
          (Ligo.sub_int_int
             (Ligo.mul_int_int den_ek (kit_to_mukit_int b.outstanding_kit))
             (Ligo.mul_int_int kit_scaling_factor_int num_ek)
          ) in
      max_int (Ligo.int_from_literal "0") numerator in

    (* denominator = collateral * den_fl * kit_sf * den_ek *)
    let denominator =
      Ligo.mul_int_int
        (Ligo.mul_int_int (tez_to_mutez b.collateral) den_fl)
        (Ligo.mul_int_int kit_scaling_factor_int den_ek) in

    Some (kit_of_fraction_ceil numerator denominator) (* Round up here; safer for the system, less so for the burrow *)

let burrow_request_liquidation (p: parameters) (b: burrow) : liquidation_result =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let partial_reward =
    let { num = num_lrp; den = den_lrp; } = liquidation_reward_percentage in
    fraction_to_tez_floor
      (Ligo.mul_int_int (tez_to_mutez b.collateral) num_lrp)
      (Ligo.mul_int_int (Ligo.int_from_literal "1_000_000") den_lrp)
  in
  if not (burrow_is_liquidatable p b) then
    (* Case 1: The outstanding kit does not exceed the liquidation limit, or
     * the burrow is already without its creation deposit, inactive; we
     * shouldn't liquidate the burrow. *)
    (None : liquidation_result)
  else
    let liquidation_reward = Ligo.add_tez_tez creation_deposit partial_reward in
    if Ligo.sub_tez_tez b.collateral partial_reward < creation_deposit then
      (* Case 2a: Cannot even refill the creation deposit; liquidate the whole
       * thing (after paying the liquidation reward of course). *)
      let tez_to_auction = Ligo.sub_tez_tez b.collateral partial_reward in
      let final_burrow =
        { b with
          active = false;
          collateral = Ligo.tez_from_literal "0mutez";
          collateral_at_auction = Ligo.add_tez_tez b.collateral_at_auction tez_to_auction;
        } in
      Some
        ( Close,
          { liquidation_reward = liquidation_reward;
            tez_to_auction = tez_to_auction;
            burrow_state = final_burrow; }
        )
    else
      (* Case 2b: We can replenish the creation deposit. Now we gotta see if it's
       * possible to liquidate the burrow partially or if we have to do so
       * completely (deplete the collateral). *)
      let b_without_reward = { b with collateral = Ligo.sub_tez_tez (Ligo.sub_tez_tez b.collateral partial_reward) creation_deposit } in
      let tez_to_auction = compute_tez_to_auction p b_without_reward in

      if (Ligo.lt_int_int tez_to_auction (Ligo.int_from_literal "0")) || (Ligo.gt_int_int tez_to_auction (tez_to_mutez b_without_reward.collateral)) then
        (* Case 2b.1: With the current price it's impossible to make the burrow
         * not undercollateralized; pay the liquidation reward, stash away the
         * creation deposit, and liquidate all the remaining collateral, even if
         * it is not expected to repay enough kit. *)
        let tez_to_auction = b_without_reward.collateral in (* OVERRIDE *)
        let final_burrow =
          { b with
            collateral = Ligo.tez_from_literal "0mutez";
            collateral_at_auction = Ligo.add_tez_tez b.collateral_at_auction tez_to_auction;
          } in
        Some
          ( Complete,
            { liquidation_reward = liquidation_reward;
              tez_to_auction = tez_to_auction;
              burrow_state = final_burrow; }
          )
      else
        (* Case 2b.2: Recovery is possible; pay the liquidation reward, stash away the
         * creation deposit, and liquidate the collateral needed to underburrow
         * the burrow (assuming that the past auctions will be successful but
         * warranted, and that the liquidation we are performing will also be
         * deemed warranted). If---when the auction is over---we realize that the
         * liquidation was not really warranted, we shall return the auction
         * earnings in their entirety. If not, then only 90% of the earnings
         * shall be returned. *)
        let tez_to_auction = match Ligo.is_nat tez_to_auction with
          | Some mutez -> Ligo.mul_nat_tez mutez (Ligo.tez_from_literal "1mutez")
          | None -> (failwith "tez_to_auction was negative, which should be impossible in this branch" : Ligo.tez)
        in
        let final_burrow =
          { b with
            collateral = Ligo.sub_tez_tez b_without_reward.collateral tez_to_auction;
            collateral_at_auction = Ligo.add_tez_tez b.collateral_at_auction tez_to_auction;
          } in
        Some
          ( Partial,
            { liquidation_reward = liquidation_reward;
              tez_to_auction = tez_to_auction;
              burrow_state = final_burrow; }
          )

(* BEGIN_OCAML *)
let burrow_collateral (b: burrow) : Ligo.tez =
  assert_burrow_invariants b;
  b.collateral

let burrow_active (b: burrow) : bool =
  assert_burrow_invariants b;
  b.active

let make_burrow_for_test
    ~active
    ~address
    ~delegate
    ~collateral
    ~outstanding_kit
    ~excess_kit
    ~adjustment_index
    ~collateral_at_auction
    ~last_touched =
  { delegate = delegate;
    address = address;
    active = active;
    collateral = collateral;
    outstanding_kit = outstanding_kit;
    excess_kit = excess_kit;
    adjustment_index = adjustment_index;
    collateral_at_auction = collateral_at_auction;
    last_touched = last_touched;
  }

(** NOTE: For testing only. Check whether a burrow is overburrowed, assuming
  * that all collateral that is in auctions at the moment will be sold at the
  * current minting price, and that all these liquidations were warranted
  * (i.e. liquidation penalties have been paid).
  *
  *   tez_collateral < fminting * (kit_outstanding - expected_kit_from_auctions) * minting_price
*)
let burrow_is_optimistically_overburrowed (p: parameters) (b: burrow) : bool =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let { num = num_fm; den = den_fm; } = fminting in
  let { num = num_mp; den = den_mp; } = minting_price p in
  let { num = num_ek; den = den_ek; } = compute_expected_kit p b.collateral_at_auction in

  (* lhs = collateral * den_fm * kit_sf * den_ek * den_mp *)
  let lhs =
    Ligo.mul_int_int
      (tez_to_mutez b.collateral)
      (Ligo.mul_int_int
         (Ligo.mul_int_int den_fm kit_scaling_factor_int)
         (Ligo.mul_int_int den_ek den_mp)
      ) in

  (* rhs = num_fm * (kit_outstanding * den_ek - kit_sf * num_ek) * num_mp * tez_sf *)
  let rhs =
    Ligo.mul_int_int
      num_fm
      (Ligo.mul_int_int
         (Ligo.sub_int_int
            (Ligo.mul_int_int (kit_to_mukit_int b.outstanding_kit) den_ek)
            (Ligo.mul_int_int kit_scaling_factor_int num_ek)
         )
         (Ligo.mul_int_int num_mp (Ligo.int_from_literal "1_000_000"))
      ) in

  Ligo.lt_int_int lhs rhs

let burrow_outstanding_kit (b: burrow) : kit = b.outstanding_kit

let burrow_excess_kit (b: burrow) : kit = b.excess_kit

(* END_OCAML *)
