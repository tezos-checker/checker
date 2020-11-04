open Address
open Burrow
open Constants
open FixedPoint
open Parameters
open Kit
open Tez
open Uniswap

(* TODO: Things to consider / action items:
 *
 * * What if compute_tez_to_auction returns something positive?
 *   => Create a kit UTXO for the burrow owner.
 *
 * * Implement auctioning logic.
*)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

type checker =
  { burrows : Burrow.t Map.Make(Address).t;
    uniswap : Uniswap.t;
    parameters : Parameters.t;
  }

(* ************************************************************************* *)
(* ************************************************************************* *)

type liquidation_outcome =
  | Unnecessary
  | Partial
  | Complete (* complete: deplete the collateral *)
  | Close (* complete: "close" the burrow *)
[@@deriving show]

type liquidation_result =
  { outcome : liquidation_outcome;
    liquidation_reward : Tez.t;
    tez_to_auction : Tez.t;
    expected_kit : Kit.t;
    min_received_kit_for_unwarranted : Kit.t; (* If we get this many kit or more, the liquidation was unwarranted *)
    burrow_state : Burrow.t;
  }
[@@deriving show]

(* George: My notes/calculations about whether an auction is warranted:

   To check whether a burrow needs to be liquidated, we first optimistically
   assume that all the tez that have been sent off to auctions in previous
   liquidations will be sold at the current minting price:

   outstanding_kit = outstanding_kit - (collateral_at_auction / minting_price p)

   and then check if there is enough collateral in the burrow to satisfy the
   following:

   tez_collateral >= fminus * outstanding_kit * (liquidation_price p) <=>
   liquidation_price p <= tez_collateral / (fminus * kit_outstanding)      (1)

   That is, if (1) were satisfied we wouldn't be able to liquidate the burrow. So,
   let's assume that we sent tez_to_auction to be auctioned off, and we ended up
   receiving repaid_kit back for it. We have:

   maximum_non_liquidating_price = tez_collateral / (fminus * kit_outstanding)    <== would not have triggered the liquidation
   originally_assumed_price      = liquidation_price p                            <== triggered the liquidation
   real_price                    = tez_to_auction / repaid_kit                    <== derived from the auction

   If real_price <= maximum_non_liquidating_price then the liquidation was not
   warranted (i.e. originally_assumed_price was off) and we should return the kit
   we received from the auction in its entirety to the burrow:

   tez_to_auction / repaid_kit <= tez_collateral / (fminus * kit_outstanding) <=>
   tez_to_auction * (fminus * kit_outstanding) <= repaid_kit * tez_collateral <=>
   tez_to_auction * (fminus * kit_outstanding) / tez_collateral <= repaid_kit <=>
   repaid_kit >= tez_to_auction * (fminus * kit_outstanding) / tez_collateral

   So, if the kit that the auction yields is more than

   (tez_to_auction * (fminus * kit_outstanding) / tez_collateral)

   then it was unwarranted.
*)
let compute_min_received_kit_for_unwarranted (p: Parameters.t) (b: Burrow.t) (tez_to_auction: Tez.t) : Kit.t =
  assert (b.collateral <> Tez.zero); (* NOTE: division by zero *)
  let expected_kit = Burrow.compute_expected_kit p b.collateral_at_auction in
  let optimistic_outstanding = Kit.(b.minted_kit - expected_kit) in
  Kit.of_fp FixedPoint.(Tez.to_fp tez_to_auction * (Constants.fminus * Kit.to_fp optimistic_outstanding) / Tez.to_fp b.collateral)

(** Compute whether the liquidation of an auction slice was (retroactively)
  * unwarranted or not. *)
let was_slice_liquidation_unwarranted
    (* Original amount of tez sent to liquidation queue *)
    (tez_to_auction: Tez.t)
    (* Pre-calculated minimum amount of kit required to receive when selling
     * tez_to_auction to consider the liquidation unwarranted *)
    (min_received_kit_for_unwarranted: Kit.t)
    (* The slice of tez_to_auction that we have sold *)
    (liquidation_slice: Tez.t)
    (* The amount of kit we received for liquidation_slice *)
    (liquidation_earning: Kit.t)
  : bool =
  FixedPoint.(Tez.to_fp tez_to_auction * Kit.to_fp liquidation_earning >= Kit.to_fp min_received_kit_for_unwarranted * Tez.to_fp liquidation_slice)

let request_liquidation (p: Parameters.t) (b: Burrow.t) : liquidation_result =
  let partial_reward = Tez.scale b.collateral Constants.liquidation_reward_percentage in
  (* Only applies if the burrow qualifies for liquidation; it is to be given to
   * the actor triggering the liquidation. *)
  let liquidation_reward = Tez.(Constants.creation_deposit + partial_reward) in
  if not (Burrow.is_liquidatable p b) then
    (* Case 1: The outstanding kit does not exceed the liquidation limit; don't
     * liquidate. *)
    { outcome = Unnecessary;
      liquidation_reward = Tez.zero;
      tez_to_auction = Tez.zero;
      expected_kit = Kit.zero;
      min_received_kit_for_unwarranted = Kit.zero; (* todo: use better representation *)
      burrow_state = b }
  else if Tez.(b.collateral - partial_reward) < Constants.creation_deposit then
    (* Case 2: Cannot even refill the creation deposit; liquidate the whole
     * thing (after paying the liquidation reward of course). *)
    let tez_to_auction = Tez.(b.collateral - partial_reward) in
    let expected_kit = Burrow.compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        has_creation_deposit = false;
        collateral = Tez.zero;
        minted_kit = Kit.zero;
      } in
    { outcome = Close;
      liquidation_reward = liquidation_reward;
      tez_to_auction = tez_to_auction;
      expected_kit = expected_kit;
      min_received_kit_for_unwarranted = compute_min_received_kit_for_unwarranted p b tez_to_auction;
      burrow_state = final_burrow }
  else if FixedPoint.(Kit.to_fp b.minted_kit * Parameters.minting_price p) > Tez.(to_fp (b.collateral - partial_reward - Constants.creation_deposit)) then
    (* Case 3: With the current price it's impossible to make the burrow not
     * undercollateralized; pay the liquidation reward, stash away the creation
     * deposit, and liquidate all the remaining collateral, even if it is not
     * expected to repay enough kit. *)
    (* George: the way I see it though, the entire position will be liquidated
     * immediately afterwards, if the collateral remaining is zero. Hmmm. *)
    (* Note: the condition here checks basically whether compute_tez_to_auction
     * would return a negative amount of tez. *)
    let b_without_reward = { b with collateral = Tez.(b.collateral - partial_reward - Constants.creation_deposit) } in
    let tez_to_auction = b_without_reward.collateral in
    let expected_kit = Burrow.compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        collateral = Tez.zero;
        collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
      } in
    { outcome = Complete;
      liquidation_reward = liquidation_reward;
      tez_to_auction = tez_to_auction;
      expected_kit = expected_kit;
      min_received_kit_for_unwarranted = compute_min_received_kit_for_unwarranted p b tez_to_auction;
      burrow_state = final_burrow }
  else
    (* Case 4: Recovery is possible; pay the liquidation reward, stash away the
     * creation deposit, and liquidate only the amount of collateral needed to
     * underburrow the burrow (as approximated now). Actually, liquidate 10%
     * more, to punish the burrow for being liquidated. If---when the auction
     * is over---we realize that the liquidation was not really warranted, we
     * shall return the auction earnings in their entirety. If not, then only
     * 90% of the earnings shall be returned. *)
    let b_without_reward = { b with collateral = Tez.(b.collateral - partial_reward - Constants.creation_deposit) } in
    let tez_to_auction =
      let essential_tez = Burrow.compute_tez_to_auction p b_without_reward in
      let with_penalty = Tez.scale essential_tez FixedPoint.(one + Constants.liquidation_penalty_percentage) in
      min b_without_reward.collateral with_penalty (* Cannot be more than the burrow's collateral! *)
    in
    let expected_kit = Burrow.compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        collateral = Tez.(b_without_reward.collateral - tez_to_auction);
        collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
      } in
    { outcome = Partial;
      liquidation_reward = liquidation_reward;
      tez_to_auction = tez_to_auction;
      expected_kit = expected_kit;
      min_received_kit_for_unwarranted = compute_min_received_kit_for_unwarranted p b tez_to_auction;
      burrow_state = final_burrow }

