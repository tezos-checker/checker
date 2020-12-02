(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
type liquidation_slices =
  { oldest: Avl.leaf_ptr; youngest: Avl.leaf_ptr }
[@@deriving show]

type t =
  { (* Whether the creation deposit for the burrow has been paid. If the
     * creation deposit has been paid, the burrow is considered "active" and
     * "closed"/inactive otherwise. Paying the creation deposit re-activates
     * a "closed" burrow. *)
    active : bool;
    (* Permission-related *)
    permission_version : int;
    allow_all_tez_deposits : bool;
    allow_all_kit_burnings : bool;
    delegate : Address.t option;
    (* Collateral currently stored in the burrow. *)
    collateral : Tez.t;
    (* Outstanding kit minted out of the burrow. *)
    outstanding_kit : Kit.t;
    (* Excess kit returned by auctions. *)
    excess_kit : Kit.t;
    (* The imbalance adjustment index observed the last time the burrow was
     * touched. *)
    adjustment_index : FixedPoint.t;
    (* Collateral that has been sent off to auctions. For all intents and
     * purposes, this collateral can be considered gone, but depending on the
     * outcome of the auctions we expect some kit in return. *)
    collateral_at_auction : Tez.t;
    (* Pointer to liquidation slices in auction queue. *)
    liquidation_slices : liquidation_slices option;
    (* The last time the burrow was touched. *)
    last_touched : Timestamp.t;
  }
[@@deriving show]

type Error.error +=
  | InsufficientFunds of Tez.t
  | WithdrawTezFailure
  | MintKitFailure
  | BurrowIsAlreadyActive
  | DeactivatingAnOverburrowedBurrow
  | DeactivatingAnInactiveBurrow
  | DeactivatingWithOutstandingKit
  | DeactivatingWithCollateralAtAuctions

let assert_invariants (b: t) : unit =
  assert (b.collateral >= Tez.zero);
  assert (b.collateral_at_auction >= Tez.zero);
  assert (b.outstanding_kit >= Kit.zero);
  assert (b.excess_kit >= Kit.zero);
  assert (b.outstanding_kit = Kit.zero || b.excess_kit = Kit.zero);
  ()

let liquidation_slices (b: t) =
  assert_invariants b;
  b.liquidation_slices

let set_liquidation_slices (b: t) (s: liquidation_slices option) =
  assert_invariants b;
  {b with liquidation_slices = s}

let collateral_at_auction (b: t) =
  assert_invariants b;
  b.collateral_at_auction

let active (b: t) =
  assert_invariants b;
  b.active

let permission_version (b: t) =
  assert_invariants b;
  b.permission_version

let allow_all_tez_deposits (b: t) =
  assert_invariants b;
  b.allow_all_tez_deposits

let allow_all_kit_burnings (b: t) =
  assert_invariants b;
  b.allow_all_kit_burnings

let make_for_test
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
let is_overburrowed (p : Parameters.t) (b : t) : bool =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  let collateral = Tez.to_q b.collateral in
  let minting_price = Parameters.minting_price p in
  let outstanding_kit = Kit.to_q b.outstanding_kit in
  Q.(collateral < Constants.fminting * outstanding_kit * minting_price)

(** Rebalance the kit inside the burrow so that either outstanding_kit is zero
  * or b.outstanding_kit is zero. *)
let rebalance_kit (b: t) : t =
  assert (b.outstanding_kit >= Kit.zero);
  assert (b.excess_kit >= Kit.zero);
  let kit_to_move = min b.outstanding_kit b.excess_kit in
  { b with
    outstanding_kit = Kit.(b.outstanding_kit - kit_to_move);
    excess_kit = Kit.(b.excess_kit - kit_to_move);
  }

(** Update the outstanding kit (and excess_kit), update the adjustment index,
  * and the timestamp. When updating the outstanding kit we round up, to
  * avoid someone touching their burrow all the time to prevent the fee from
  * accumulating. *)
let touch (p: Parameters.t) (burrow: t) : t =
  assert_invariants burrow;
  if p.last_touched = burrow.last_touched
  then
    burrow
  else
    let b = rebalance_kit burrow in
    let current_adjustment_index = Parameters.compute_adjustment_index p in
    let last_adjustment_index = FixedPoint.to_q b.adjustment_index in
    let kit_outstanding = Kit.to_q b.outstanding_kit in
    { b with
      outstanding_kit = Kit.of_q_ceil Q.(kit_outstanding * FixedPoint.to_q current_adjustment_index / last_adjustment_index);
      adjustment_index = current_adjustment_index;
      last_touched = p.last_touched;
    }

let return_tez_from_auction (tez: Tez.t) (burrow: t) : t =
  assert_invariants burrow;
  assert burrow.active;
  assert (tez >= Tez.zero);
  { burrow with
    collateral = Tez.(burrow.collateral + tez);
    collateral_at_auction = Tez.(burrow.collateral_at_auction - tez);
  }

(** Return some kit that we have received from an auction to the burrow. *)
let return_kit_from_auction (tez: Tez.t) (kit: Kit.t) (b: t) : t =
  assert_invariants b;
  assert (kit >= Kit.zero);
  rebalance_kit
    { b with
      excess_kit = Kit.(b.excess_kit + kit);
      collateral_at_auction = Tez.(b.collateral_at_auction - tez);
    }

let create (p: Parameters.t) (tez: Tez.t) : (t, Error.error) result =
  if tez < Constants.creation_deposit
  then Error (InsufficientFunds tez)
  else Ok
      { active = true;
        permission_version = 0;
        allow_all_tez_deposits = false;
        allow_all_kit_burnings = false;
        delegate = None;
        collateral = Tez.(tez - Constants.creation_deposit);
        outstanding_kit = Kit.zero;
        excess_kit = Kit.zero;
        adjustment_index = Parameters.compute_adjustment_index p;
        collateral_at_auction = Tez.zero;
        last_touched = p.last_touched; (* NOTE: If checker is up-to-date, the timestamp should be _now_. *)
        liquidation_slices = None;
      }

(** Add non-negative collateral to a burrow. *)
let deposit_tez (p: Parameters.t) (t: Tez.t) (b: t) : t =
  assert_invariants b;
  assert (t >= Tez.zero);
  assert (p.last_touched = b.last_touched);
  { b with collateral = Tez.(b.collateral + t) }

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let withdraw_tez (p: Parameters.t) (t: Tez.t) (b: t) : (t * Tez.t, Error.error) result =
  assert_invariants b;
  assert (t >= Tez.zero);
  assert (p.last_touched = b.last_touched);
  let new_burrow = { b with collateral = Tez.(b.collateral - t) } in
  if is_overburrowed p new_burrow
  then Error WithdrawTezFailure
  else Ok (new_burrow, t)

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let mint_kit (p: Parameters.t) (kit: Kit.t) (b: t) : (t * Kit.t, Error.error) result =
  assert_invariants b;
  assert (kit >= Kit.zero);
  assert (p.last_touched = b.last_touched);
  let new_burrow = { b with outstanding_kit = Kit.(b.outstanding_kit + kit) } in
  if is_overburrowed p new_burrow
  then Error MintKitFailure
  else Ok (new_burrow, kit)

(** Deposit/burn a non-negative amount of kit to the burrow. If there is
  * excess kit, simply store it into the burrow. *)
let burn_kit (p: Parameters.t) (k: Kit.t) (b: t) : t =
  assert_invariants b;
  assert (k >= Kit.zero);
  assert (p.last_touched = b.last_touched);
  let kit_to_burn = min b.outstanding_kit k in
  let kit_to_store = Kit.(k - kit_to_burn) in
  { b with
    outstanding_kit = Kit.(b.outstanding_kit - kit_to_burn);
    excess_kit = Kit.(b.excess_kit + kit_to_store);
  }

(** Activate a currently inactive burrow. This operation will fail if either
  * the burrow is already active, or if the amount of tez given is less than
  * the creation deposit. *)
let activate (p: Parameters.t) (tez: Tez.t) (b: t) : (t, Error.error) result =
  assert_invariants b;
  assert (tez >= Tez.zero);
  assert (p.last_touched = b.last_touched);
  if tez < Constants.creation_deposit then
    Error (InsufficientFunds tez)
  else if b.active then
    Error BurrowIsAlreadyActive
  else
    Ok { b with
         active = true;
         collateral = Tez.(tez - Constants.creation_deposit);
       }

(** Deativate a currently active burrow. This operation will fail if the burrow
  * (a) is already inactive, or (b) is overburrowed, or (c) has kit
  * outstanding, or (d) has collateral sent off to auctions. *)
let deactivate (p: Parameters.t) (b: t) : (t * Tez.t, Error.error) result =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  if (is_overburrowed p b) then
    Error DeactivatingAnOverburrowedBurrow
  else if (not b.active) then
    Error DeactivatingAnInactiveBurrow
  else if (b.outstanding_kit > Kit.zero) then
    Error DeactivatingWithOutstandingKit
  else if (b.collateral_at_auction > Tez.zero) then
    Error DeactivatingWithCollateralAtAuctions
  else
    let return = Tez.(b.collateral + Constants.creation_deposit) in
    let updated_burrow =
      { b with
        active = false;
        collateral = Tez.zero;
      } in
    Ok (updated_burrow, return)

let set_delegate (p: Parameters.t) (new_delegate: Address.t) (b: t) : t =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  { b with delegate = Some new_delegate; }

(* ************************************************************************* *)
(*                           PERMISSION-RELATED                              *)
(* ************************************************************************* *)

let set_allow_all_tez_deposits (p: Parameters.t) (b: t) (on: bool) =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  { b with allow_all_tez_deposits = on; }

let set_allow_all_kit_burns (p: Parameters.t) (b: t) (on: bool) =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  { b with allow_all_kit_burnings = on; }

let increase_permission_version (p: Parameters.t) (b: t) =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  let new_version = b.permission_version + 1 in
  (new_version, {b with permission_version = new_version;})

(* ************************************************************************* *)
(**                          LIQUIDATION-RELATED                             *)
(* ************************************************************************* *)

(** Compute the number of tez that needs to be auctioned off so that the burrow
  * can return to a state when it is no longer overburrowed or having a risk of
  * liquidation. For its calculation, see docs/burrow-state-liquidations.md.
  * Note that it's skewed on the safe side (overapproximation). This ensures
  * that after a partial liquidation we are no longer "optimistically
  * overburrowed" *)
let compute_tez_to_auction (p: Parameters.t) (b: t) : Tez.t =
  assert_invariants b;
  let oustanding_kit = Kit.to_q b.outstanding_kit in
  let collateral = Tez.to_q b.collateral in
  let collateral_at_auction = Tez.to_q b.collateral_at_auction in
  let minting_price = Parameters.minting_price p in
  Tez.of_q_ceil
    Q.(
      ((oustanding_kit * Constants.fminting * minting_price)
       - ((one - Constants.liquidation_penalty) * Constants.fminting * collateral_at_auction)
       - collateral
      )
      / ((one - Constants.liquidation_penalty) * Constants.fminting - one)
    )

(** Compute the amount of kit we expect to receive from auctioning off an
  * amount of tez, using the current minting price. Note that we being rather
  * optimistic here (we overapproximate the expected kit). NOTE: Make sure
  * this agrees with expectations. *)
let compute_expected_kit (p: Parameters.t) (tez_to_auction: Tez.t) : Kit.t =
  Kit.of_q_ceil
    Q.(Tez.to_q tez_to_auction * (one - Constants.liquidation_penalty) / Parameters.minting_price p)

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
let is_liquidatable (p: Parameters.t) (b: t) : bool =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  let expected_kit = compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = Kit.(to_q (b.outstanding_kit - expected_kit)) in
  let liquidation_price = Parameters.liquidation_price p in
  let collateral = Tez.to_q b.collateral in
  b.active && Q.(collateral < Constants.fliquidation * optimistic_outstanding * liquidation_price)

(** NOTE: For testing only. Check whether a burrow is overburrowed, assuming
  * that all collateral that is in auctions at the moment will be sold at the
  * current minting price, and that all these liquidations were warranted
  * (i.e. liquidation penalties have been paid). *)
let is_optimistically_overburrowed (p: Parameters.t) (b: t) : bool =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  let expected_kit = compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = Kit.(to_q (b.outstanding_kit - expected_kit)) in
  let collateral = Tez.to_q b.collateral in
  let minting_price = Parameters.minting_price p in
  Q.(collateral < Constants.fminting * optimistic_outstanding * minting_price)

type liquidation_details =
  { liquidation_reward : Tez.t;
    tez_to_auction : Tez.t;
    expected_kit : Kit.t;
    min_kit_for_unwarranted : Kit.t; (* If we get this many kit or more, the liquidation was unwarranted *)
    burrow_state : t;
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

let compute_min_kit_for_unwarranted (p: Parameters.t) (b: t) (tez_to_auction: Tez.t) : Kit.t =
  assert (b.collateral <> Tez.zero); (* NOTE: division by zero *)
  assert (p.last_touched = b.last_touched);
  let expected_kit = compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = Kit.(to_q (b.outstanding_kit - expected_kit)) in
  let collateral = Tez.to_q b.collateral in
  Kit.of_q_ceil (* Round up here; safer for the system, less so for the burrow *)
    Q.(Tez.to_q tez_to_auction * (Constants.fliquidation * optimistic_outstanding) / collateral)

let request_liquidation (p: Parameters.t) (b: t) : liquidation_result =
  assert_invariants b;
  assert (p.last_touched = b.last_touched);
  let liquidation_reward_percentage = FixedPoint.of_q_floor Constants.liquidation_reward_percentage in (* FLOOR-or-CEIL *)
  let partial_reward = Tez.scale b.collateral liquidation_reward_percentage in
  (* Only applies if the burrow qualifies for liquidation; it is to be given to
   * the actor triggering the liquidation. *)
  let liquidation_reward = Tez.(Constants.creation_deposit + partial_reward) in
  if not (is_liquidatable p b) then
    (* Case 1: The outstanding kit does not exceed the liquidation limit, or
     * the burrow is already without its creation deposit, inactive; we
     * shouldn't liquidate the burrow. *)
    Unnecessary
  else if Tez.(b.collateral - partial_reward) < Constants.creation_deposit then
    (* Case 2a: Cannot even refill the creation deposit; liquidate the whole
     * thing (after paying the liquidation reward of course). *)
    let tez_to_auction = Tez.(b.collateral - partial_reward) in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        active = false;
        collateral = Tez.zero;
        collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
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
    let b_without_reward = { b with collateral = Tez.(b.collateral - partial_reward - Constants.creation_deposit) } in
    let tez_to_auction = compute_tez_to_auction p b_without_reward in

    if tez_to_auction < Tez.zero || tez_to_auction > b_without_reward.collateral then
      (* Case 2b.1: With the current price it's impossible to make the burrow
       * not undercollateralized; pay the liquidation reward, stash away the
       * creation deposit, and liquidate all the remaining collateral, even if
       * it is not expected to repay enough kit. *)
      let tez_to_auction = b_without_reward.collateral in (* OVERRIDE *)
      let expected_kit = compute_expected_kit p tez_to_auction in
      let final_burrow =
        { b with
          collateral = Tez.zero;
          collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
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
          collateral = Tez.(b_without_reward.collateral - tez_to_auction);
          collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
        } in
      Partial {
        liquidation_reward = liquidation_reward;
        tez_to_auction = tez_to_auction;
        expected_kit = expected_kit;
        min_kit_for_unwarranted = compute_min_kit_for_unwarranted p b tez_to_auction;
        burrow_state = final_burrow }

let oldest_liquidation_ptr (b: t) : Avl.leaf_ptr option =
  assert_invariants b;
  Option.map (fun i -> i.oldest) b.liquidation_slices
