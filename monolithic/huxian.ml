
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
 * * About switching to an integer representation for tez and kit: Currently 1
 *   tez (1 XTZ) is divisible to six decimal places, and the smallest unit is
 *   called a micro tez: 1 tez = 100 cents = 1,000,000 micro tez (mutez). Let's
 *   do the same for kit.
 *
 * * Implement auctioning logic.
 *
 * * Implement imbalance adjustment-related logic.
 *
 * * Implement burrowing fees-related logic.
 *
 * * Do not go through floating-point numbers anywhere.
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
  { burrows : burrow Map.Make(String).t; (* TODO: Create an 'address' type *)
    uniswap : uniswap;
    parameters : parameters;
  }

type duration = Seconds of int
let seconds_of_duration = function | Seconds s -> s

(* TODO: Not tested, take it with a grain of salt. *)
let step_parameters
    (time_passed: duration)
    (current_index: float)
    (current_kit_in_tez: float)
    (parameters: parameters)
  : Kit.t * parameters =
  (* Compute the new protected index, using the time interval, the current
   * index (given by the oracles right now), and the protected index of the
   * previous timestamp. *)
  let duration_in_seconds = float_of_int (seconds_of_duration time_passed) in
  let upper_lim = exp (protected_index_epsilon *. duration_in_seconds) in
  let lower_lim = exp (-. protected_index_epsilon *. duration_in_seconds) in
  let current_protected_index =
    Tez.to_float parameters.protected_index
    *. clamp
      (current_index /. Tez.to_float parameters.protected_index)
      lower_lim
      upper_lim in
  let current_drift' =
    FixedPoint.of_float (compute_drift_derivative (FixedPoint.to_float parameters.target)) in
  let current_drift =
    FixedPoint.(
      parameters.drift
      + (FixedPoint.of_float (1. /. 2.))
        * (parameters.drift' + current_drift')
        * FixedPoint.of_float duration_in_seconds
    ) in

  (* TODO: use integer arithmetic *)
  let current_q =
    FixedPoint.to_float parameters.q
    *. exp ( ( FixedPoint.to_float parameters.drift
               +. (1. /. 6.)
                  *. (2. *. FixedPoint.to_float FixedPoint.(parameters.drift' + current_drift'))
                  *. duration_in_seconds )
             *. duration_in_seconds ) in
  let current_target = current_q *. current_index /. current_kit_in_tez in

  (* Update the indices *)
  let current_burrow_fee_index = FixedPoint.(parameters.burrow_fee_index * (one + burrow_fee_percentage)) in
  let imbalance_percentage = compute_imbalance (failwith "TODO:burrowed") (failwith "TODO:minted") in
  let current_imbalance_index = FixedPoint.(parameters.imbalance_index * (one + imbalance_percentage)) in
  let with_burrow_fee = Kit.of_fp FixedPoint.(Kit.to_fp parameters.global_last_minted_kit * current_burrow_fee_index / parameters.burrow_fee_index) in
  let total_accrual_to_uniswap = Kit.sub with_burrow_fee parameters.global_last_minted_kit in
  let current_global_last_minted_kit = Kit.of_fp FixedPoint.(Kit.to_fp with_burrow_fee * (current_imbalance_index / parameters.imbalance_index)) in
  ( total_accrual_to_uniswap
  , {
      index = Tez.of_float current_index;
      protected_index = Tez.of_float current_protected_index;
      target = FixedPoint.of_float current_target;
      drift = current_drift;
      drift' = current_drift';
      q = FixedPoint.of_float current_q;
      burrow_fee_index = current_burrow_fee_index;
      imbalance_index = current_imbalance_index;
      global_last_minted_kit = current_global_last_minted_kit;
    }
  )

(* ************************************************************************* *)
(* ************************************************************************* *)

type liquidation_outcome =
  | Unwarranted
  | Partial
  | Complete
  | Close

(* TODO: More sharing here please *)
type liquidation_result
  = liquidation_outcome
    * Tez.t               (* liquidation reward *)
    * Tez.t               (* tez to auction *)
    * Kit.t               (* expected kit from selling the tez *)
    * burrow              (* current state of the burrow *)

let pp_liquidation_outcome (ppf: Format.formatter) (o: liquidation_outcome) =
  match o with
  | Unwarranted -> Format.fprintf ppf "Unwarranted"
  | Partial -> Format.fprintf ppf "Partial"
  | Complete -> Format.fprintf ppf "Complete (deplete the collateral)"
  | Close -> Format.fprintf ppf "Complete (\"close\" the burrow)"

let print_liquidation_outcome (o: liquidation_outcome) =
  pp_liquidation_outcome Format.std_formatter o

let pp_liquidation_result (ppf: Format.formatter) (r: liquidation_result) =
  match r with
  | (outcome, reward, tez_to_sell, expected_kit, burrow) ->
    Format.fprintf
      ppf
      "%a\nliquidation_reward: %a\ntez_to_sell: %a\nexpected_kit: %a\nburrow_state: %a\n"
      pp_liquidation_outcome outcome
      Tez.pp reward
      Tez.pp tez_to_sell
      Kit.pp expected_kit
      pp_burrow burrow

let print_liquidation_result (r: liquidation_result) =
  pp_liquidation_result Format.std_formatter r

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
(* TODO: Remove divisions in the conditions; use multiplication instead. *)
let request_liquidation (p: parameters) (b: burrow) : liquidation_result =
  let partial_reward = Tez.of_fp FixedPoint.(liquidation_reward_percentage * (Tez.to_fp b.collateral)) in
  (* The reward for triggering a liquidation. This amounts to the burrow's
   * creation deposit, plus the liquidation reward percentage of the burrow's
   * collateral. Of course, this only applies if the burrow qualifies for
   * liquidation. This reward is to be given to the ator triggering the
   * liquidation. *)
  let liquidation_reward = Tez.add creation_deposit partial_reward in
  (* Case 1: The outstanding kit does not exceed the liquidation limit; don't
   * liquidate. *)
  if not (is_liquidatable p b) then
    (Unwarranted, Tez.zero, Tez.zero, Kit.zero, b)
    (* Case 2: Cannot even refill the creation deposit; liquidate the whole
     * thing (after paying the liquidation reward of course). *)
  else if b.collateral < liquidation_reward then
    let tez_to_auction = Tez.sub b.collateral partial_reward in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        collateral = Tez.zero;
        minted_kit = Kit.zero; (* TODO: this needs fixing *)
      } in
    (Close, liquidation_reward, tez_to_auction, expected_kit, final_burrow)
    (* Case 3: With the current price it's impossible to make the burrow not
     * undercollateralized; pay the liquidation reward, stash away the creation
     * deposit, and liquidate all the remaining collateral, even if it is not
     * expected to repay enough kit. *)
  else if FixedPoint.(Kit.to_fp b.minted_kit * minting_price p) > Tez.to_fp (Tez.sub b.collateral liquidation_reward) then
    let b_without_reward = { b with collateral = Tez.sub b.collateral liquidation_reward } in
    let tez_to_auction = b_without_reward.collateral in
    let expected_kit = compute_expected_kit p tez_to_auction in
    (* NOTE: (Tez.sub b_without_reward.collateral tez_to_auction) should be
     * zero, but with the current state of affairs (i.e. using floats) the
     * expected assertion fails unpleasantly. TODO: add the assertion here once
     * you switch to the integer representation of tez. *)
    let final_burrow =
      { b with
        collateral = Tez.sub b_without_reward.collateral tez_to_auction;
        auctioned_collateral = Tez.add b.auctioned_collateral tez_to_auction;
      } in
    (Complete, liquidation_reward, tez_to_auction, expected_kit, final_burrow)
    (* Case 4: Recovery is possible; pay the liquidation reward, stash away the
     * creation deposit, and liquidate only the amount of collateral needed to
     * underburrow the burrow (as approximated now). No more, no less. *)
  else
    let b_without_reward = { b with collateral = Tez.sub b.collateral liquidation_reward } in
    let tez_to_auction = compute_tez_to_auction p b_without_reward in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { b with
        collateral = Tez.sub b_without_reward.collateral tez_to_auction;
        auctioned_collateral = Tez.add b.auctioned_collateral tez_to_auction;
      } in
    (Partial, liquidation_reward, tez_to_auction, expected_kit, final_burrow)
