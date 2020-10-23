
include Burrow
open Constants
include Constants
open FixedPoint
open Kit
include Parameters
open Tez
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

let cnp (i: FixedPoint.t) : FixedPoint.t = FixedPoint.(i / FixedPoint.of_float 100.0)

let sign (i: float) : float =
  if i > 0. then 1.
  else if i = 0. then 0.
  else -1.

let clamp (v: 'a) (lower: 'a) (upper: 'a) : 'a =
  assert (lower <= upper);
  min upper (max v lower)

(* ************************************************************************* *)
(**                               IMBALANCE                                  *)
(* ************************************************************************* *)

(** If we call burrowed the total amount of kit necessary to close all existing
  * burrows, and minted the total amount of kit in circulation, then the
  * imbalance fee/bonus is calculated as follows (per year):
  *
  *   min(   5 * burrowed, (burrowed - minted) ) * 1.0 cNp / burrowed , if burrowed >= minted
  *   max( - 5 * burrowed, (burrowed - minted) ) * 1.0 cNp / burrowed , otherwise
*)
let compute_imbalance (burrowed: Kit.t) (minted: Kit.t) : FixedPoint.t =
  assert (burrowed >= Kit.zero); (* Invariant *)
  assert (minted >= Kit.zero); (* Invariant *)
  let centinepers = cnp (FixedPoint.of_float 0.1) in (* TODO: per year! *)
  let burrowed_fivefold = Kit.scale burrowed (FixedPoint.of_float 5.0) in
  (* No kit in burrows or in circulation means no imbalance adjustment *)
  if burrowed = Kit.zero then
    (assert (minted = Kit.zero); FixedPoint.zero) (* George: Is it possible to have minted kit in circulation when nothing is burrowed? *)
  else if burrowed = minted then
    FixedPoint.zero (* George: I add this special case, to avoid rounding issues *)
  else if burrowed >= minted then
    Kit.div (Kit.scale (min burrowed_fivefold (Kit.sub burrowed minted)) centinepers) burrowed
  else
    FixedPoint.neg (Kit.div (Kit.scale (min burrowed_fivefold (Kit.sub minted burrowed)) centinepers) burrowed)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

type checker =
  { burrows : burrow Map.Make(String).t; (* TODO: Create an 'address' type *)
    uniswap : uniswap;
    parameters : parameters;
  }

(* Utku: Thresholds here are cnp / day^2, we should convert them to cnp /
 * second^2, assuming we're measuring time in seconds. My calculations might be
 * incorrect. *)
let compute_drift_derivative (target : float) : float =
  assert (target > 0.);
  let cnp_001 = FixedPoint.to_float (cnp (FixedPoint.of_float 0.01)) in
  let cnp_005 = FixedPoint.to_float (cnp (FixedPoint.of_float 0.05)) in

  let log_target = log target in
  let abs_log_target = Float.abs log_target in
  if abs_log_target < FixedPoint.to_float (cnp (FixedPoint.of_float 0.5)) then
    0.
  else if abs_log_target < FixedPoint.to_float (cnp (FixedPoint.of_float 5.0)) then
    sign log_target *. (cnp_001 /. (24. *. 3600.) ** 2.)
  else
    sign log_target *. (cnp_005 /. (24. *. 3600.) ** 2.)

(* George: Note that we don't really need to calculate the logs here (which can
 * be lossy); we can instead exponentiate the whole equation (exp is monotonic)
 * and win some precision, like this:
*)
let compute_drift_derivative_2 (target : float) : float =
  assert (target > 0.);
  let cnp_001 = FixedPoint.to_float (cnp (FixedPoint.of_float 0.01)) in
  let cnp_005 = FixedPoint.to_float (cnp (FixedPoint.of_float 0.05)) in
  match () with
  (* No acceleration (0) *)
  | () when exp (-. 0.5 /. 100.) < target && target < exp (0.5 /. 100.) -> 0.
  (* Low acceleration (-/+) *)
  | () when exp (-. 5.0 /. 100.) < target && target <= exp (-. 0.5 /. 100.) -> -. (cnp_001 /. (24. *. 3600.) ** 2.)
  | () when exp    (5.0 /. 100.) > target && target >= exp    (0.5 /. 100.) ->    (cnp_001 /. (24. *. 3600.) ** 2.)
  (* High acceleration (-/+) *)
  | () when target <= exp (-. 5.0 /. 100.) -> -. (cnp_005 /. (24. *. 3600.) ** 2.)
  | () when target >= exp    (5.0 /. 100.) ->    (cnp_005 /. (24. *. 3600.) ** 2.)
  | _ -> failwith "impossible"

type duration = Seconds of int
let seconds_of_duration = function | Seconds s -> s


(* TODO: Not tested, take it with a grain of salt. *)
let step_parameters
    (time_passed: duration)
    (current_index: float)
    (current_kit_in_tez: float)
    (parameters: parameters)
  : parameters =
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
  {
    index = Tez.of_float current_index;
    protected_index = Tez.of_float current_protected_index;
    target = FixedPoint.of_float current_target;
    drift = current_drift;
    drift' = current_drift';
    q = FixedPoint.of_float current_q
  }

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
  if not (should_burrow_be_liquidated p b) then
    (Unwarranted, Tez.zero, Tez.zero, Kit.zero, b)
    (* Case 2: Cannot even refill the creation deposit; liquidate the whole
     * thing (after paying the liquidation reward of course). *)
  else if b.collateral < liquidation_reward then
    let tez_to_auction = Tez.sub b.collateral partial_reward in
    let expected_kit = compute_expected_kit p tez_to_auction in
    (Close, liquidation_reward, tez_to_auction, expected_kit, create_burrow ())
    (* Case 3: With the current price it's impossible to make the burrow not
     * undercollateralized; pay the liquidation reward, stash away the creation
     * deposit, and liquidate all the remaining collateral, even if it is not
     * expected to repay enough kit. *)
  else if FixedPoint.(Kit.to_fp b.minted_kit * p.q * Tez.to_fp (tz_minting p)) > Tez.to_fp (Tez.sub b.collateral liquidation_reward) then
    let b_without_reward = { b with collateral = Tez.sub b.collateral liquidation_reward } in
    let tez_to_auction = b_without_reward.collateral in
    let expected_kit = compute_expected_kit p tez_to_auction in
    (* NOTE: (Tez.sub b_without_reward.collateral tez_to_auction) should be
     * zero, but with the current state of affairs (i.e. using floats) the
     * expected assertion fails unpleasantly. TODO: add the assertion here once
     * you switch to the integer representation of tez. *)
    let final_burrow =
      { collateral = Tez.sub b_without_reward.collateral tez_to_auction;
        minted_kit = Kit.sub b_without_reward.minted_kit expected_kit;
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
      { collateral = Tez.sub b_without_reward.collateral tez_to_auction;
        minted_kit = Kit.sub b_without_reward.minted_kit expected_kit;
      } in
    (Partial, liquidation_reward, tez_to_auction, expected_kit, final_burrow)
