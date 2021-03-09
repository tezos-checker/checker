open FixedPoint
open Ratio
open Kit
open Parameters
open LiquidationAuctionPrimitiveTypes
open Constants
open Error
open Common

type liquidation_slices = {oldest: leaf_ptr; youngest: leaf_ptr;}
[@@deriving show]

type burrow =
  { (* Whether the creation deposit for the burrow has been paid. If the
     * creation deposit has been paid, the burrow is considered "active" and
     * "closed"/inactive otherwise. Paying the creation deposit re-activates
     * a "closed" burrow. *)
    active : bool;
    (* Permission-related *)
    permission_version : Ligo.nat;
    allow_all_tez_deposits : bool;
    allow_all_kit_burnings : bool;
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
    (* Pointer to liquidation slices in auction queue. *)
    liquidation_slices : liquidation_slices option;
    (* The last time the burrow was touched. *)
    last_touched : Ligo.timestamp;
  }
[@@deriving show]

let[@inline] ensure_uptodate_burrow (p: parameters) (b: burrow) : unit =
  if p.last_touched = b.last_touched
  then ()
  else (Ligo.failwith error_OperationOnUntouchedBurrow : unit)

let[@inline] assert_burrow_invariants (b: burrow) : unit =
  assert (b.outstanding_kit = kit_zero || b.excess_kit = kit_zero);
  ()

let[@inline] burrow_liquidation_slices (b: burrow) : liquidation_slices option =
  assert_burrow_invariants b;
  b.liquidation_slices

let[@inline] burrow_set_liquidation_slices (b: burrow) (s: liquidation_slices option) : burrow =
  assert_burrow_invariants b;
  {b with liquidation_slices = s}

let[@inline] burrow_collateral_at_auction (b: burrow) : Ligo.tez =
  assert_burrow_invariants b;
  b.collateral_at_auction

let[@inline] burrow_permission_version (b: burrow) : Ligo.nat =
  assert_burrow_invariants b;
  b.permission_version

let[@inline] burrow_allow_all_tez_deposits (b: burrow) : bool =
  assert_burrow_invariants b;
  b.allow_all_tez_deposits

let[@inline] burrow_allow_all_kit_burnings (b: burrow) : bool =
  assert_burrow_invariants b;
  b.allow_all_kit_burnings

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

  let { num = num_fm; den = den_fm; } = fminting in
  let { num = num_mp; den = den_mp; } = minting_price p in
  let outstanding_kit = Ligo.int (kit_to_mukit b.outstanding_kit) in

  let lhs =
    Ligo.mul_int_int
      (tez_to_mutez b.collateral)
      (Ligo.mul_int_int den_fm (Ligo.mul_int_int kit_scaling_factor_int den_mp)) in
  let rhs =
    Ligo.mul_int_int
      (Ligo.mul_int_int num_fm (Ligo.mul_int_int outstanding_kit num_mp))
      (Ligo.int_from_literal "1_000_000") in
  Ligo.lt_int_int lhs rhs

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
  if p.last_touched = burrow.last_touched
  then
    burrow
  else
    let b = rebalance_kit burrow in
    let current_adjustment_index = compute_adjustment_index p in
    let last_adjustment_index = fixedpoint_to_ratio b.adjustment_index in
    let kit_outstanding = kit_to_ratio b.outstanding_kit in
    { b with
      outstanding_kit =
        kit_of_ratio_ceil
          (div_ratio
             (mul_ratio
                kit_outstanding
                (fixedpoint_to_ratio current_adjustment_index)
             )
             last_adjustment_index
          );
      adjustment_index = current_adjustment_index;
      last_touched = p.last_touched;
    }

(* Notify a burrow that one of its liquidation slices has been removed from
 * auctions. This means that we have to (a) update the amount of
 * tez living in auctions; it is less now, and (b) update the pointers to the
 * youngest and the oldest liquidation slices that the burrow has. In most
 * cases these pointers will remain unaltered, but if the slice being removed
 * is the youngest or the oldest they won't. *)
let burrow_remove_liquidation_slice
    (burrow : burrow)
    (leaf_ptr: leaf_ptr)
    (leaf : liquidation_slice) (* NOTE: derived from the leaf_ptr *)
  : burrow =
  assert (burrow.collateral_at_auction >= leaf.tez);
  (* (a) the slice's tez is no longer in auctions, subtract it. *)
  let burrow = { burrow with
                 collateral_at_auction = Ligo.sub_tez_tez burrow.collateral_at_auction leaf.tez;
               } in
  (* (b) adjust the pointers *)
  match burrow.liquidation_slices with
  | None -> (failwith "the burrow must have at least leaf_ptr sent off to an auction" : burrow)
  | Some slices -> (
      match leaf.younger with
      | None -> (
          match leaf.older with
          | None ->
            assert (slices.youngest = leaf_ptr);
            assert (slices.oldest = leaf_ptr);
            { burrow with liquidation_slices = (None : liquidation_slices option); }
          | Some older ->
            assert (slices.youngest = leaf_ptr);
            { burrow with liquidation_slices = Some {slices with youngest = older} }
        )
      | Some younger -> (
          match leaf.older with
          | None ->
            assert (slices.oldest = leaf_ptr);
            { burrow with liquidation_slices = Some {slices with oldest = younger} }
          | Some _older ->
            assert (slices.oldest <> leaf_ptr);
            assert (slices.youngest <> leaf_ptr);
            burrow
        )
    )

let burrow_return_slice_from_auction
    (leaf_ptr: leaf_ptr)
    (leaf: liquidation_slice)
    (burrow: burrow)
  : burrow =
  assert_burrow_invariants burrow;
  assert burrow.active;
  (* (a) the slice's tez is no longer in auctions: subtract it and adjust the pointers *)
  let burrow = burrow_remove_liquidation_slice burrow leaf_ptr leaf in
  (* (b) return the tez into the burrow's collateral *)
  { burrow with collateral = Ligo.add_tez_tez burrow.collateral leaf.tez; }

let burrow_return_kit_from_auction
    (leaf_ptr: leaf_ptr)
    (leaf: liquidation_slice)
    (kit: kit)
    (burrow: burrow) : burrow =
  assert_burrow_invariants burrow;
  (* (a) the slice's tez is no longer in auctions: subtract it and adjust the pointers *)
  let burrow = burrow_remove_liquidation_slice burrow leaf_ptr leaf in
  (* (b) burn/deposit the kit received from auctioning the slice *)
  rebalance_kit { burrow with excess_kit = kit_add burrow.excess_kit kit; }

let burrow_create (p: parameters) (tez: Ligo.tez) (delegate_opt: Ligo.key_hash option) : burrow =
  if tez < creation_deposit
  then (Ligo.failwith error_InsufficientFunds : burrow)
  else
    { active = true;
      permission_version = Ligo.nat_from_literal "0n";
      allow_all_tez_deposits = false;
      allow_all_kit_burnings = false;
      delegate = delegate_opt;
      collateral = Ligo.sub_tez_tez tez creation_deposit;
      outstanding_kit = kit_zero;
      excess_kit = kit_zero;
      adjustment_index = compute_adjustment_index p;
      collateral_at_auction = Ligo.tez_from_literal "0mutez";
      last_touched = p.last_touched; (* NOTE: If checker is up-to-date, the timestamp should be _now_. *)
      liquidation_slices = (None : liquidation_slices option);
    }

(** Add non-negative collateral to a burrow. *)
let[@inline] burrow_deposit_tez (p: parameters) (t: Ligo.tez) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  { b with collateral = Ligo.add_tez_tez b.collateral t }

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let burrow_withdraw_tez (p: parameters) (t: Ligo.tez) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow = { b with collateral = Ligo.sub_tez_tez b.collateral t } in
  if burrow_is_overburrowed p burrow
  then (Ligo.failwith error_WithdrawTezFailure : burrow)
  else burrow

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let burrow_mint_kit (p: parameters) (kit: kit) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let burrow = { b with outstanding_kit = kit_add b.outstanding_kit kit } in
  if burrow_is_overburrowed p burrow
  then (Ligo.failwith error_MintKitFailure : burrow)
  else burrow

(** Deposit/burn a non-negative amount of kit to the burrow. If there is
  * excess kit, simply store it into the burrow. *)
let[@inline] burrow_burn_kit (p: parameters) (k: kit) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let kit_to_burn = kit_min b.outstanding_kit k in
  let kit_to_store = kit_sub k kit_to_burn in
  { b with
    outstanding_kit = kit_sub b.outstanding_kit kit_to_burn;
    excess_kit = kit_add b.excess_kit kit_to_store;
  }

(** Activate a currently inactive burrow. This operation will fail if either
  * the burrow is already active, or if the amount of tez given is less than
  * the creation deposit. *)
let burrow_activate (p: parameters) (tez: Ligo.tez) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  if tez < creation_deposit then
    (Ligo.failwith error_InsufficientFunds : burrow)
  else if b.active then
    (Ligo.failwith error_BurrowIsAlreadyActive : burrow)
  else
    { b with
      active = true;
      collateral = Ligo.sub_tez_tez tez creation_deposit;
    }

(** Deativate a currently active burrow. This operation will fail if the burrow
  * (a) is already inactive, or (b) is overburrowed, or (c) has kit
  * outstanding, or (d) has collateral sent off to auctions. *)
let burrow_deactivate (p: parameters) (b: burrow) : (burrow * Ligo.tez) =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
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

let burrow_set_delegate (p: parameters) (new_delegate: Ligo.key_hash option) (b: burrow) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  { b with delegate = new_delegate; }

(* ************************************************************************* *)
(*                           PERMISSION-RELATED                              *)
(* ************************************************************************* *)

let burrow_set_allow_all_tez_deposits (p: parameters) (b: burrow) (on: bool) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  { b with allow_all_tez_deposits = on; }

let burrow_set_allow_all_kit_burns (p: parameters) (b: burrow) (on: bool) : burrow =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  { b with allow_all_kit_burnings = on; }

let burrow_increase_permission_version (p: parameters) (b: burrow) : (Ligo.nat * burrow) =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let new_version = Ligo.add_nat_nat b.permission_version (Ligo.nat_from_literal "1n") in
  (new_version, {b with permission_version = new_version;})

(* ************************************************************************* *)
(**                          LIQUIDATION-RELATED                             *)
(* ************************************************************************* *)

(** Compute the number of tez that needs to be auctioned off so that the burrow
  * can return to a state when it is no longer overburrowed or having a risk of
  * liquidation (assuming the current expected minting price). For its
  * calculation, see docs/burrow-state-liquidations.md.  Note that it's skewed
  * on the safe side (overapproximation). This ensures that after a partial
  * liquidation we are no longer "optimistically overburrowed" *)
let compute_tez_to_auction (p: parameters) (b: burrow) : Ligo.tez =
  assert_burrow_invariants b;
  let oustanding_kit = kit_to_ratio b.outstanding_kit in
  let collateral = ratio_of_tez b.collateral in
  let collateral_at_auction = ratio_of_tez b.collateral_at_auction in
  let minting_price = minting_price p in
  ratio_to_tez_ceil
    (div_ratio
       (sub_ratio
          (sub_ratio
             (mul_ratio
                (mul_ratio oustanding_kit fminting)
                minting_price
             )
             (mul_ratio
                (mul_ratio
                   (sub_ratio one_ratio liquidation_penalty)
                   fminting
                )
                collateral_at_auction
             )
          )
          collateral
       )
       (sub_ratio
          (mul_ratio
             (sub_ratio one_ratio liquidation_penalty)
             fminting
          )
          one_ratio
       )
    )

(** Compute the amount of kit we expect to receive from auctioning off an
  * amount of tez, using the current minting price. Note that we are being
  * rather optimistic here (we overapproximate the expected kit). *)
let compute_expected_kit (p: parameters) (tez_to_auction: Ligo.tez) : kit =
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
  kit_of_ratio_ceil (make_real_unsafe numerator denominator)

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
let burrow_is_liquidatable (p: parameters) (b: burrow) : bool =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let expected_kit = compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = (* if more is stored in the burrow, we just use optimistic_outstanding = 0 *)
    if b.outstanding_kit < expected_kit
    then zero_ratio
    else kit_to_ratio (kit_sub b.outstanding_kit expected_kit) in
  let liquidation_price = liquidation_price p in
  let collateral = ratio_of_tez b.collateral in
  b.active && lt_ratio_ratio collateral (mul_ratio (mul_ratio fliquidation optimistic_outstanding) liquidation_price)

type liquidation_details =
  { liquidation_reward : Ligo.tez;
    tez_to_auction : Ligo.tez;
    expected_kit : kit;
    min_kit_for_unwarranted : kit; (* If we get this many kit or more, the liquidation was unwarranted *)
    burrow_state : burrow;
  }
[@@deriving show]

type liquidation_result =
  (* the burrow does not qualify for liquidation *)
  | Unnecessary
  (* partial: some collateral remains in the burrow *)
  | Partial of liquidation_details
  (* complete: deplete the collateral *)
  | Complete of liquidation_details
  (* complete: deplete the collateral AND the creation deposit *)
  | Close of liquidation_details
[@@deriving show]

let compute_min_kit_for_unwarranted (p: parameters) (b: burrow) (tez_to_auction: Ligo.tez) : kit =
  let _ = ensure_uptodate_burrow p b in
  assert (b.collateral <> Ligo.tez_from_literal "0mutez"); (* NOTE: division by zero *)
  let expected_kit = compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = (* if more is stored in the burrow, we just use optimistic_outstanding = 0 *)
    if b.outstanding_kit < expected_kit
    then zero_ratio
    else kit_to_ratio (kit_sub b.outstanding_kit expected_kit) in
  let collateral = ratio_of_tez b.collateral in
  kit_of_ratio_ceil (* Round up here; safer for the system, less so for the burrow *)
    (div_ratio
       (mul_ratio
          (ratio_of_tez tez_to_auction)
          (mul_ratio fliquidation optimistic_outstanding)
       )
       collateral
    )

let burrow_request_liquidation (p: parameters) (b: burrow) : liquidation_result =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let partial_reward =
    ratio_to_tez_floor
      (mul_ratio
         (ratio_of_tez b.collateral)
         (fixedpoint_to_ratio liquidation_reward_percentage)
      ) in
  (* Only applies if the burrow qualifies for liquidation; it is to be given to
   * the actor triggering the liquidation. *)
  let liquidation_reward = Ligo.add_tez_tez creation_deposit partial_reward in
  if not (burrow_is_liquidatable p b) then
    (* Case 1: The outstanding kit does not exceed the liquidation limit, or
     * the burrow is already without its creation deposit, inactive; we
     * shouldn't liquidate the burrow. *)
    Unnecessary
  else if Ligo.sub_tez_tez b.collateral partial_reward < creation_deposit then
    (* Case 2a: Cannot even refill the creation deposit; liquidate the whole
     * thing (after paying the liquidation reward of course). *)
    let tez_to_auction = Ligo.sub_tez_tez b.collateral partial_reward in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        active = false;
        collateral = Ligo.tez_from_literal "0mutez";
        collateral_at_auction = Ligo.add_tez_tez b.collateral_at_auction tez_to_auction;
      } in
    Close {
      liquidation_reward = liquidation_reward;
      tez_to_auction = tez_to_auction;
      expected_kit = expected_kit;
      min_kit_for_unwarranted = compute_min_kit_for_unwarranted p b tez_to_auction;
      burrow_state = final_burrow }
  else
    (* Case 2b: We can replenish the creation deposit. Now we gotta see if it's
     * possible to liquidate the burrow partially or if we have to do so
     * completely (deplete the collateral). *)
    let b_without_reward = { b with collateral = Ligo.sub_tez_tez (Ligo.sub_tez_tez b.collateral partial_reward) creation_deposit } in
    let tez_to_auction = compute_tez_to_auction p b_without_reward in

    if tez_to_auction < Ligo.tez_from_literal "0mutez" || tez_to_auction > b_without_reward.collateral then
      (* Case 2b.1: With the current price it's impossible to make the burrow
       * not undercollateralized; pay the liquidation reward, stash away the
       * creation deposit, and liquidate all the remaining collateral, even if
       * it is not expected to repay enough kit. *)
      let tez_to_auction = b_without_reward.collateral in (* OVERRIDE *)
      let expected_kit = compute_expected_kit p tez_to_auction in
      let final_burrow =
        { b with
          collateral = Ligo.tez_from_literal "0mutez";
          collateral_at_auction = Ligo.add_tez_tez b.collateral_at_auction tez_to_auction;
        } in
      Complete {
        liquidation_reward = liquidation_reward;
        tez_to_auction = tez_to_auction;
        expected_kit = expected_kit;
        min_kit_for_unwarranted = compute_min_kit_for_unwarranted p b tez_to_auction;
        burrow_state = final_burrow }
    else
      (* Case 2b.2: Recovery is possible; pay the liquidation reward, stash away the
       * creation deposit, and liquidate the collateral needed to underburrow
       * the burrow (assuming that the past auctions will be successful but
       * warranted, and that the liquidation we are performing will also be
       * deemed warranted). If---when the auction is over---we realize that the
       * liquidation was not really warranted, we shall return the auction
       * earnings in their entirety. If not, then only 90% of the earnings
       * shall be returned. *)
      let expected_kit = compute_expected_kit p tez_to_auction in
      let final_burrow =
        { b with
          collateral = Ligo.sub_tez_tez b_without_reward.collateral tez_to_auction;
          collateral_at_auction = Ligo.add_tez_tez b.collateral_at_auction tez_to_auction;
        } in
      Partial {
        liquidation_reward = liquidation_reward;
        tez_to_auction = tez_to_auction;
        expected_kit = expected_kit;
        min_kit_for_unwarranted = compute_min_kit_for_unwarranted p b tez_to_auction;
        burrow_state = final_burrow }

let[@inline] burrow_oldest_liquidation_ptr (b: burrow) : leaf_ptr option =
  assert_burrow_invariants b;
  match b.liquidation_slices with
  | None -> (None : leaf_ptr option)
  | Some i -> Some i.oldest

(* BEGIN_OCAML *)
let burrow_collateral (b: burrow) : Ligo.tez =
  assert_burrow_invariants b;
  b.collateral

let burrow_active (b: burrow) : bool =
  assert_burrow_invariants b;
  b.active

let make_burrow_for_test
    ~active
    ~permission_version
    ~allow_all_tez_deposits
    ~allow_all_kit_burnings
    ~delegate
    ~collateral
    ~outstanding_kit
    ~excess_kit
    ~adjustment_index
    ~collateral_at_auction
    ~liquidation_slices
    ~last_touched =
  { permission_version = permission_version;
    allow_all_tez_deposits = allow_all_tez_deposits;
    allow_all_kit_burnings = allow_all_kit_burnings;
    delegate = delegate;
    active = active;
    collateral = collateral;
    outstanding_kit = outstanding_kit;
    excess_kit = excess_kit;
    adjustment_index = adjustment_index;
    collateral_at_auction = collateral_at_auction;
    last_touched = last_touched;
    liquidation_slices = liquidation_slices;
  }

(** NOTE: For testing only. Check whether a burrow is overburrowed, assuming
  * that all collateral that is in auctions at the moment will be sold at the
  * current minting price, and that all these liquidations were warranted
  * (i.e. liquidation penalties have been paid). *)
let burrow_is_optimistically_overburrowed (p: parameters) (b: burrow) : bool =
  let _ = ensure_uptodate_burrow p b in
  assert_burrow_invariants b;
  let expected_kit = compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = (* if more is stored in the burrow, we just use optimistic_outstanding = 0 *)
    if b.outstanding_kit < expected_kit
    then zero_ratio
    else kit_to_ratio (kit_sub b.outstanding_kit expected_kit) in
  let collateral = ratio_of_tez b.collateral in
  let minting_price = minting_price p in
  lt_ratio_ratio collateral (mul_ratio (mul_ratio fminting optimistic_outstanding) minting_price)

(* END_OCAML *)
