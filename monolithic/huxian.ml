
open Constants
include Constants
open FixedPoint
open Kit
open Tez


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
(*                               BASIC TYPES                                 *)
(* ************************************************************************* *)
type tez = Tez.t;;
type kit = Kit.t;;
type liquidity = int;;

(* TODOs for burrows:
   - Add owner (delegate)
   - Add field for incurred fee
   - Fix all relevant interfaces (create, mint, burn, deposit, close)
   - Move to separate module
*)
type burrow =
  { collateral : tez [@printer Tez.pp];
    minted_kit : kit [@printer Kit.pp];
  }
[@@deriving show]

type checker_parameters =
  { q : FixedPoint.t [@printer FixedPoint.pp]; (* 1/kit, really *)
    index: tez [@printer Tez.pp];
    protected_index: tez [@printer Tez.pp];
    target: FixedPoint.t [@printer FixedPoint.pp];
    drift': FixedPoint.t [@printer FixedPoint.pp];
    drift: FixedPoint.t [@printer FixedPoint.pp];
  }
[@@deriving show]

(* tez. To get tez/kit must multiply with q. *)
let tz_minting (p: checker_parameters) : tez =
  max p.index p.protected_index

(* tez. To get tez/kit must multiply with q. *)
let tz_liquidation (p: checker_parameters) : tez =
  min p.index p.protected_index

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
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(** Check whether a burrow is overburrowed. A burrow is overburrowed if
  *
  *   tez_collateral < fplus * kit_outstanding * (q * tz_mint)
  *
  * The quantity tez_collateral / (fplus * (q * tz_mint)) we call the burrowing
  * limit (normally kit_outstanding <= burrowing_limit).
*)
let is_overburrowed (p : checker_parameters) (b : burrow) : bool =
  Tez.to_fp b.collateral < FixedPoint.(fplus * Kit.to_fp b.minted_kit * (p.q * Tez.to_fp (tz_minting p)))

(** Create a burrow without any tez collateral or outstanding kit. *)
let create_burrow () : burrow =
  { collateral = Tez.of_float 0.0;
    minted_kit = Kit.of_float 0.0;
  }

(** Add non-negative collateral to a burrow. *)
let deposit_tez (t : tez) (b : burrow) : burrow =
  assert (t >= Tez.zero);
  { b with collateral = Tez.add b.collateral t }

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let withdraw_tez (p : checker_parameters) (t : tez) (b : burrow) : burrow option =
  assert (t >= Tez.zero);
  let updated = { b with collateral = Tez.sub b.collateral t } in
  if is_overburrowed p updated
  then None
  else Some updated

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let mint_kits_from_burrow (p : checker_parameters) (k : kit) (b : burrow) =
  assert (k >= Kit.zero);
  let updated = { b with minted_kit = Kit.add b.minted_kit k } in
  if is_overburrowed p updated
  then None
  else Some updated

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
let compute_imbalance (burrowed: kit) (minted: kit) : FixedPoint.t =
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
let should_burrow_be_liquidated (p : checker_parameters) (b : burrow) : bool =
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
let compute_tez_to_auction (p : checker_parameters) (b : burrow) : tez =
  Tez.of_fp
    FixedPoint.((Kit.to_fp b.minted_kit * fplus * p.q * Tez.to_fp (tz_minting p) - Tez.to_fp b.collateral)
     / (fplus - FixedPoint.one))

(* TODO: Don't go through float, and ensure that it's skewed on the safe side (underapprox.). *)
let compute_expected_kit (p : checker_parameters) (tez_to_auction: tez) : kit =
  Kit.of_fp FixedPoint.(Tez.to_fp tez_to_auction / (p.q * Tez.to_fp (tz_minting p)))

(* ************************************************************************* *)
(**                               UNISWAP                                    *)
(* ************************************************************************* *)

(* The general concept of uniswap is that you have quantity a of an asset A
 * and b of an asset B and you process buy and sell requests by maintaining
 * the product a * b constant. So if someone wants to sell a quantity da of
 * asset A to the contract, the balance would become (a + da) so you can
 * give that person a quantity db of asset B in exchange such that (a +
 * da)(b - db) = a * b. Solving for db gives db  = da * b / (a + da). We
 * can rewrite this as db = da * (b / a) * (a / (a + da)) where (b / a)
 * represents the  "price" before the order and a / (a + da)  represents
 * the "slippage". Indeed, a property of uniswap is that with arbitrageurs
 * around, the ratio (a / b) gives you the market price of A in terms of B.
 *
 * On top of that, we can add some fees of 0.2 cNp. So the equation becomes
 * something like db = da * b / (a + da) * (1 - 0.2/100) (note that this
 * formula is a first-order approximation in the sense that two orders of size
 * da / 2 will give you a better price than one order of size da, but the
 * difference is far smaller than typical fees or any amount we care about.
*)

(* TODO: The state of uniswap should also (in the future) include an ongoing
 * auction to decide who to delegate to, possibly multiple tez balances, etc.
 * Just leaving this note here lest we forget. *)
type uniswap =
  { tez: tez;
    kit: kit;
    total_liquidity_tokens: int;
  }

let pp_uniswap (ppf: Format.formatter) (u : uniswap) =
  Format.fprintf
    ppf
    "{tez = %a;\n kit = %a;\n total_liquidity_tokens = %d}\n"
    Tez.pp u.tez
    Kit.pp u.kit
    u.total_liquidity_tokens

let print_uniswap (u : uniswap) = pp_uniswap Format.std_formatter u

let uniswap_non_empty(u: uniswap) =
  u.kit > Kit.zero && u.tez > Tez.zero

let sell_kit (uniswap: uniswap) (kit: kit) : tez * kit * uniswap =
  (* Utku: I think, as long as the contract has non-zero tez and kit this
   * always succeeds.
   *
   * So, I think the function should fail when the contract is missing either
   * currency. It will presumably be started with some amount of tez, and
   * the first minting fee will initialize the kit amount.
  *)
  assert (uniswap_non_empty uniswap);
  assert (kit > Kit.zero);

  let price = FixedPoint.(Tez.to_fp uniswap.tez / Kit.to_fp uniswap.kit) in
  let slippage = Kit.div uniswap.kit (Kit.add uniswap.kit kit) in
  let return = Tez.of_fp FixedPoint.(Kit.to_fp kit * price * slippage * (FixedPoint.one - uniswap_fee_percentage)) in
  let updated = { uniswap with
                  kit = Kit.add uniswap.kit kit;
                  tez = Tez.sub uniswap.tez return } in
  (return, Kit.zero, updated)

(* But where do the assets in uniswap come from? Liquidity providers, or
 * "LP" deposit can deposit a quantity la and lb of assets A and B in the
 * same proportion as the contract la / lb = a / b . Assuming there are n
 * "liquidity tokens" extant, they receive m = floor(n la / a) tokens and
 * there are now m +n liquidity tokens extant. They can redeem then at
 * anytime for a fraction of the assets A and B. The reason to do this in
 * uniswap is that usage of uniswap costs 0.3%, and that ultimately can
 * grow the balance of the assets in the contract. An additional reason
 * to do it in huxian is that the kit balance of the uniswap contract is
 * continuously credited with the burrow fee taken from burrow holders.
*)

let buy_liquidity (uniswap: uniswap) (tez: tez) (kit: kit)
  : liquidity * tez * kit * uniswap =
  (* Adding liquidity always succeeds, if the exchange has non-zero amount. *)
  assert (uniswap_non_empty uniswap);
  (* TODO: How to compute things without explicitly calculating the ratio
   * (using division) here? *)
  let ratio = FixedPoint.(Tez.to_fp uniswap.tez / Kit.to_fp uniswap.kit) in
  (* There is a chance that the given tez and kit have the wrong ratio,
   * so we liquidate as much as we can and return the leftovers. NOTE:
   * Alternatively, the LP can use the uniswap contract to get the right
   * ratio beforehand.
   * Invariant here is that (tez', kit') should have the correct ratio.
  *)
  let (tez', kit') = FixedPoint.(
    if Tez.to_fp tez * Kit.to_fp uniswap.kit > Kit.to_fp kit * Tez.to_fp uniswap.tez
    then (Tez.of_fp (Kit.to_fp kit * ratio), kit)
    else if Tez.to_fp tez * Kit.to_fp uniswap.kit < Kit.to_fp kit * Tez.to_fp uniswap.tez
    then (tez, Kit.of_fp (Tez.to_fp tez / ratio))
    else (tez, kit) ) in
  let liquidity =
    if uniswap.total_liquidity_tokens = 0
    then 1
    else int_of_float (floor (
        float_of_int uniswap.total_liquidity_tokens
        *. Tez.to_float tez'
        /. Tez.to_float uniswap.tez))
  in
  let updated =
    { kit = Kit.add uniswap.kit kit';
      tez = Tez.add uniswap.tez tez';
      total_liquidity_tokens =
        uniswap.total_liquidity_tokens + liquidity } in
  (liquidity, Tez.sub tez tez', Kit.sub kit kit', updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees.
*)
let sell_liquidity (uniswap: uniswap) (liquidity: liquidity)
  : tez * kit * uniswap =
  (* Since this requires a liquidity token, contract can not be empty *)
  assert(uniswap_non_empty(uniswap));
  let ratio =
    float_of_int liquidity /. float_of_int uniswap.total_liquidity_tokens in
  let tez = Tez.of_float (Tez.to_float uniswap.tez *. ratio) in
  let kit = Kit.of_float (Kit.to_float uniswap.kit *. ratio) in
  let updated = {
    tez = Tez.sub uniswap.tez tez;
    kit = Kit.sub uniswap.kit kit;
    total_liquidity_tokens = uniswap.total_liquidity_tokens - liquidity } in
  (tez, kit, updated)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

type checker =
  { burrows : burrow Map.Make(String).t; (* TODO: Create an 'address' type *)
    uniswap : uniswap;
    parameters : checker_parameters;
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
    (parameters: checker_parameters)
  : checker_parameters =
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
    * tez                 (* liquidation reward *)
    * tez                 (* tez to auction *)
    * kit                 (* expected kit from selling the tez *)
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
let request_liquidation (p: checker_parameters) (b: burrow) : liquidation_result =
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
