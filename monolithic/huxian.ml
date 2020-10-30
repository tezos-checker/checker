
open Address
open Burrow
include Burrow
open Constants
include Constants
open FixedPoint
open Kit
include Parameters
open Tez
open Uniswap
include Uniswap

(* TODO: Things to consider / action items:
 *
 * * What if compute_tez_to_auction returns something positive?
 *   => Create a kit UTXO for the burrow owner.
 *
 * * Implement auctioning logic.
 *
 * * Implement imbalance adjustment-related logic.
 *
 * * Implement burrowing fees-related logic.
*)

(* ************************************************************************* *)
(**                           UTILITY FUNCTIONS                              *)
(* ************************************************************************* *)

let clamp (v: 'a) (lower: 'a) (upper: 'a) : 'a =
  assert (lower <= upper);
  min upper (max v lower)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

type checker =
  { burrows : burrow Map.Make(Address).t;
    uniswap : uniswap;
    parameters : parameters;
  }

type duration = Seconds of int
let seconds_of_duration = function | Seconds s -> s

(* TODO: Not tested, take it with a grain of salt. *)
let step_parameters
    (time_passed: duration)
    (current_index: FixedPoint.t)
    (current_kit_in_tez: FixedPoint.t)
    (parameters: parameters)
  : Kit.t * parameters =
  (* Compute the new protected index, using the time interval, the current
   * index (given by the oracles right now), and the protected index of the
   * previous timestamp. *)
  let duration_in_seconds = FixedPoint.of_int (seconds_of_duration time_passed) in
  let upper_lim = FixedPoint.(exp (protected_index_epsilon * duration_in_seconds)) in
  let lower_lim = FixedPoint.(exp (neg protected_index_epsilon * duration_in_seconds)) in
  let current_protected_index =
    FixedPoint.(
      Tez.to_fp parameters.protected_index
      * clamp
        (current_index / Tez.to_fp parameters.protected_index)
        lower_lim
        upper_lim
    ) in
  let current_drift' = compute_drift_derivative parameters.target in
  let current_drift =
    FixedPoint.(
      parameters.drift
      + of_float (1. /. 2.)
        * (parameters.drift' + current_drift')
        * duration_in_seconds
    ) in

  let current_q =
    FixedPoint.(
      parameters.q
      * exp ( ( parameters.drift
                + of_float (1. /. 6.)
                  * (of_float 2. * (parameters.drift' + current_drift'))
                  * duration_in_seconds )
              * duration_in_seconds )
    ) in
  let current_target = FixedPoint.(current_q * current_index / current_kit_in_tez) in

  (* Update the indices *)
  let current_burrow_fee_index = FixedPoint.(parameters.burrow_fee_index * (one + burrow_fee_percentage)) in (* TODO: Yearly! *)
  let imbalance_percentage = compute_imbalance parameters.outstanding_kit parameters.circulating_kit in
  let current_imbalance_index = FixedPoint.(parameters.imbalance_index * (one + imbalance_percentage)) in (* TODO: Yearly! *)
  let with_burrow_fee = Kit.of_fp FixedPoint.(Kit.to_fp parameters.outstanding_kit * current_burrow_fee_index / parameters.burrow_fee_index) in
  let total_accrual_to_uniswap = Kit.(with_burrow_fee - parameters.outstanding_kit) in
  let current_outstanding_kit = Kit.of_fp FixedPoint.(Kit.to_fp with_burrow_fee * (current_imbalance_index / parameters.imbalance_index)) in
  let current_circulating_kit = Kit.(parameters.circulating_kit + total_accrual_to_uniswap) in
  (* TODO: Don't forget to actually add total_accrual_to_uniswap to the uniswap contract! *)
  ( total_accrual_to_uniswap
  , {
    index = Tez.of_fp current_index;
    protected_index = Tez.of_fp current_protected_index;
    target = current_target;
    drift = current_drift;
    drift' = current_drift';
    q = current_q;
    burrow_fee_index = current_burrow_fee_index;
    imbalance_index = current_imbalance_index;
    outstanding_kit = current_outstanding_kit;
    circulating_kit = current_circulating_kit;
  }
  )

(* ************************************************************************* *)
(* ************************************************************************* *)

type liquidation_outcome =
  (* TODO: careful with the terminology on warranted/unwarranted. There's also
   * https://tezos-dev.slack.com/archives/G01AAK05N86/p1603108889364300 *)
  | Unwarranted
  | Partial
  | Complete (* complete: deplete the collateral *)
  | Close (* complete: "close" the burrow *)
[@@deriving show]

type liquidation_result =
  { outcome : liquidation_outcome;
    liquidation_reward : Tez.t;
    tez_to_auction : Tez.t;
    expected_kit : Kit.t;
    burrow_state : burrow;
  }
[@@deriving show]

(* NOTE: George: The initial state of the burrow is the collateral C, the
 * oustanding kit K, and the implicit creation deposit D (1 tez). I say
 * implicit because it does not count towards the collateral. So, in honesty,
 * the 1 tez in the reward does not come from the collateral, but it is the
 * creation deposit D. But then, in order to bring the burrow in a good state,
 * we need to stash away (implicitly, again) 1 tez as creation deposit (this
 * will be used if (a) the owner wants to close the burrow or (b) we need to
 * liquidate the burrow again). This one does come out of the collateral
 * though. From the outside it all looks the same I guess, minus one plus one,
 * but I thought that this intricacy is worth pointing out.
*)
let request_liquidation (p: parameters) (b: burrow) : liquidation_result =
  let partial_reward = Tez.scale b.collateral liquidation_reward_percentage in
  (* Only applies if the burrow qualifies for liquidation; it is to be given to
   * the actor triggering the liquidation. *)
  let liquidation_reward = Tez.(creation_deposit + partial_reward) in
  (* Case 1: The outstanding kit does not exceed the liquidation limit; don't
   * liquidate. *)
  if not (is_liquidatable p b) then
    { outcome = Unwarranted; liquidation_reward = Tez.zero; tez_to_auction = Tez.zero; expected_kit = Kit.zero; burrow_state = b }
    (* Case 2: Cannot even refill the creation deposit; liquidate the whole
     * thing (after paying the liquidation reward of course). *)
  else if Tez.(b.collateral - partial_reward) < creation_deposit then
    let tez_to_auction = Tez.(b.collateral - partial_reward) in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        has_creation_deposit = false;
        collateral = Tez.zero;
        minted_kit = Kit.zero;
      } in
    { outcome = Close; liquidation_reward = liquidation_reward; tez_to_auction = tez_to_auction; expected_kit = expected_kit; burrow_state = final_burrow }
    (* Case 3: With the current price it's impossible to make the burrow not
     * undercollateralized; pay the liquidation reward, stash away the creation
     * deposit, and liquidate all the remaining collateral, even if it is not
     * expected to repay enough kit. *)
    (* George: the way I see it though, the entire position will be liquidated
     * immediately afterwards, if the collateral remaining is zero. Hmmm. *)
  else if FixedPoint.(Kit.to_fp b.minted_kit * minting_price p) > Tez.(to_fp (b.collateral - liquidation_reward)) then
    let b_without_reward = { b with collateral = Tez.(b.collateral - liquidation_reward) } in
    let tez_to_auction = b_without_reward.collateral in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        collateral = Tez.zero;
        collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
      } in
    { outcome = Complete; liquidation_reward = liquidation_reward; tez_to_auction = tez_to_auction; expected_kit = expected_kit; burrow_state = final_burrow }
    (* Case 4: Recovery is possible; pay the liquidation reward, stash away the
     * creation deposit, and liquidate only the amount of collateral needed to
     * underburrow the burrow (as approximated now). Actually, liquidate 10%
     * more, to punish the burrow for being liquidated. If---when the auction
     * is over---we realize that the liquidation was not really warranted, we
     * shall return the auction earnings in their entirety. If not, then only
     * 90% of the earnings shall be returned. *)
  else
    let b_without_reward = { b with collateral = Tez.(b.collateral - liquidation_reward) } in
    let tez_to_auction =
      let essential_tez = compute_tez_to_auction p b_without_reward in
      let with_penalty = Tez.scale essential_tez FixedPoint.(one + liquidation_penalty_percentage) in
      min b_without_reward.collateral with_penalty (* Cannot be more than the burrow's collateral! *)
    in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        collateral = Tez.(b_without_reward.collateral - tez_to_auction);
        collateral_at_auction = Tez.(b.collateral_at_auction + tez_to_auction);
      } in
    { outcome = Partial; liquidation_reward = liquidation_reward; tez_to_auction = tez_to_auction; expected_kit = expected_kit; burrow_state = final_burrow }

