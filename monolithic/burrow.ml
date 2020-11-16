(* ************************************************************************* *)
(*                                Burrows                                    *)
(* ************************************************************************* *)
module Burrow : sig
  type burrow_liquidation_slices =
    { oldest: Avl.leaf_ptr; youngest: Avl.leaf_ptr }

  type t =
    { (* Whether the creation deposit for the burrow has been paid. If the
       * creation deposit has been paid, the burrow is considered "active" and
       * "closed"/inactive otherwise. Paying the creation deposit re-activates
       * a "closed" burrow. *)
      active : bool;
      (* The owner of the burrow. Set once during creation. *)
      owner : Address.t;
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
      liquidation_slices : burrow_liquidation_slices option;
      (* The last time the burrow was touched. *)
      last_touched : Timestamp.t;
    }

  type Error.error +=
    | InsufficientFunds of Tez.t
    | WithdrawTezFailure
    | MintKitFailure

  val show : t -> string
  val pp : Format.formatter -> t -> unit

  val show_burrow_liquidation_slices
    : burrow_liquidation_slices -> string
  val pp_burrow_liquidation_slices
    : Format.formatter -> burrow_liquidation_slices -> unit

  (** Check whether a burrow is overburrowed. A burrow is overburrowed if
    *
    *   tez_collateral < fminting * kit_outstanding * minting_price
    *
    * The quantity tez_collateral / (fminting * minting_price) we call the burrowing
    * limit (normally kit_outstanding <= burrowing_limit).
  *)
  val is_overburrowed : Parameters.t -> t -> bool

  (** Check if the owner matches. TODO: implement permissions properly. *)
  val is_owned_by : t -> Address.t -> bool

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

  (** Deposit/burn a non-negative amount of kit to the burrow. If there is
    * excess kit, simply store it into the burrow. *)
  val burn_kit : Parameters.t -> Kit.t -> t -> t

  (** Compute the least number of tez that needs to be auctioned off (given the
    * current expected minting price) so that the burrow can return to a state
    * when it is no longer overburrowed or having a risk of liquidation. *)
  val compute_tez_to_auction : Parameters.t -> t -> Tez.t

  (** Given the number of tez to be auctioned off, compute the expected return
    * in kit, given the current minting price. Assume that the liquidation was
    * warranted, so the liquidation penalty is subtracted *)
  val compute_expected_kit : Parameters.t -> Tez.t -> Kit.t

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

end = struct
  type burrow_liquidation_slices =
    { oldest: Avl.leaf_ptr; youngest: Avl.leaf_ptr }
  [@@deriving show]

  type t =
    { active : bool;
      owner : Address.t;
      delegate : Address.t option;
      collateral : Tez.t;
      outstanding_kit : Kit.t;
      excess_kit : Kit.t;
      adjustment_index : FixedPoint.t;
      collateral_at_auction : Tez.t;
      liquidation_slices : burrow_liquidation_slices option;
      last_touched : Timestamp.t;
    }
  [@@deriving show]

  type Error.error +=
    | InsufficientFunds of Tez.t
    | WithdrawTezFailure
    | MintKitFailure

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
    assert (p.last_touched = b.last_touched);
    let collateral = Tez.to_q b.collateral in
    let minting_price = Parameters.minting_price p in
    let outstanding_kit = Kit.to_q b.outstanding_kit in
    Q.(collateral < Constants.fminting * outstanding_kit * minting_price)

  let is_owned_by (b: t) (address: Address.t) = b.owner = address

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
    if p.last_touched = burrow.last_touched
    then
      burrow
    else
      let b = rebalance_kit burrow in
      let current_adjustment_index = Parameters.compute_adjustment_index p in
      let last_adjustment_index = FixedPoint.to_q b.adjustment_index in
      let kit_outstanding = Kit.to_q b.outstanding_kit in
      { b with
        outstanding_kit = Kit.of_q_ceil Q.(kit_outstanding * current_adjustment_index / last_adjustment_index);
        adjustment_index = FixedPoint.of_q_floor current_adjustment_index; (* TODO: round up or down here? *)
        last_touched = p.last_touched;
      }

  let create (p: Parameters.t) (address: Address.t) (tez: Tez.t) : (t, Error.error) result =
    if tez < Constants.creation_deposit
    then Error (InsufficientFunds tez)
    else Ok
        { active = true;
          owner = address;
          delegate = None;
          collateral = Tez.(tez - Constants.creation_deposit);
          outstanding_kit = Kit.zero;
          excess_kit = Kit.zero;
          adjustment_index = FixedPoint.of_q_floor (Parameters.compute_adjustment_index p); (* TODO: round up or down here? *)
          collateral_at_auction = Tez.zero;
          last_touched = p.last_touched; (* NOTE: If checker is up-to-date, the timestamp should be _now_. *)
          liquidation_slices = None;
        }

  (** Add non-negative collateral to a burrow. *)
  let deposit_tez (p: Parameters.t) (t: Tez.t) (b: t) : t =
    assert (t >= Tez.zero);
    assert (p.last_touched = b.last_touched);
    { b with collateral = Tez.(b.collateral + t) }

  (** Withdraw a non-negative amount of tez from the burrow, as long as this will
    * not overburrow it. *)
  let withdraw_tez (p: Parameters.t) (t: Tez.t) (b: t) : (t * Tez.t, Error.error) result =
    assert (t >= Tez.zero);
    assert (p.last_touched = b.last_touched);
    let new_burrow = { b with collateral = Tez.(b.collateral - t) } in
    if is_overburrowed p new_burrow
    then Error WithdrawTezFailure
    else Ok (new_burrow, t)

  (** Mint a non-negative amount of kits from the burrow, as long as this will
    * not overburrow it *)
  let mint_kit (p: Parameters.t) (kit: Kit.t) (b: t) : (t * Kit.t, Error.error) result =
    assert (kit >= Kit.zero);
    assert (p.last_touched = b.last_touched);
    let new_burrow = { b with outstanding_kit = Kit.(b.outstanding_kit + kit) } in
    if is_overburrowed p new_burrow
    then Error MintKitFailure
    else Ok (new_burrow, kit)

  (** Deposit/burn a non-negative amount of kit to the burrow. If there is
    * excess kit, simply store it into the burrow. *)
  let burn_kit (p: Parameters.t) (k: Kit.t) (b: t) : t =
    assert (k >= Kit.zero);
    assert (p.last_touched = b.last_touched);
    let kit_to_burn = min b.outstanding_kit k in
    let kit_to_store = Kit.(k - kit_to_burn) in
    { b with
      outstanding_kit = Kit.(b.outstanding_kit - kit_to_burn);
      excess_kit = Kit.(b.excess_kit + kit_to_store);
    }

  (* ************************************************************************* *)
  (**                          LIQUIDATION-RELATED                             *)
  (* ************************************************************************* *)

  (** Compute the number of tez that needs to be auctioned off so that the burrow
    * can return to a state when it is no longer overburrowed or having a risk of
    * liquidation. For its calculation, see docs/burrow-state-liquidations.md.
    * Note that it's skewed on the safe side (overapproximation). This ensures
    * that after a partial liquidation we are no longer "optimistically
    * overburrowed" *)
  let compute_tez_to_auction (p : Parameters.t) (b : t) : Tez.t =
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
  let is_liquidatable (p : Parameters.t) (b : t) : bool =
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
    assert (p.last_touched = b.last_touched);
    let partial_reward = Tez.scale b.collateral Constants.liquidation_reward_percentage in
    (* Only applies if the burrow qualifies for liquidation; it is to be given to
     * the actor triggering the liquidation. *)
    let liquidation_reward = Tez.(Constants.creation_deposit + partial_reward) in
    if not (is_liquidatable p b) then
      (* Case 1: The outstanding kit does not exceed the liquidation limit; we
       * shouldn't liquidate the burrow, it's solid. *)
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
end
