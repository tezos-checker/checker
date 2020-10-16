
open Format;;

(* TODO: Things to consider / action items:
 *
 * * What if compute_tez_to_auction returns something positive?
 *   => Create a kit UTXO for the burrow owner.
 *
 * * About switching to an integer representation for tez and kit: Currently 1
 *   tez (1 XTZ) is divisible to six decimal places, and the smallest unit is
 *   called a micro tez: 1 tez = 100 cents = 1,000,000 micro tez (mutez)
 *
 * * The auctioning logic is currently missing.
 *
 * * Find ways to test the system.
 *
 * * Deal with the case where the burrow needs to be liquidated in its entirety.
 *
 * * I don't think we have anything relating to imbalance or burrowing fees atm.
 *)

(* ************************************************************************* *)
(*                               BASIC TYPES                                 *)
(* ************************************************************************* *)
type tez = float;; (* TODO: Use int64 instead TODO: Use newtypes instead *)
type kit = float;; (* TODO: Use int64 instead TODO: Use newtypes instead *)
type liquidity = int;;

type burrow =
  { collateral_tez : tez;
    outstanding_kit : kit;
  }

let print_burrow (b : burrow) =
  printf
    "{collateral  = %.15f tez;\n outstanding = %.15f kit}\n"
    b.collateral_tez
    b.outstanding_kit

type checker_parameters =
  { q : float; (* 1/kit, really *)
    index: tez;
    protected_index: tez;
    target: float;
    drift': float;
    drift: float;
  }

let print_checker_parameters (p: checker_parameters) =
  printf "q: %f\n" p.q;
  printf "index: %f\n" p.index;
  printf "protected_index: %f\n" p.protected_index;
  printf "drift: %.15f\n" p.drift;
  printf "drift': %.15f\n" p.drift'

(* tez. To get tez/kit must multiply with q. *)
let tz_minting (p: checker_parameters) : tez =
  max p.index p.protected_index

(* tez. To get tez/kit must multiply with q. *)
let tz_liquidation (p: checker_parameters) : tez =
  min p.index p.protected_index

(* ************************************************************************* *)
(**                               CONSTANTS                                  *)
(* ************************************************************************* *)

let (fplus  : float) = 2.1;; (* dimensionless. Alternatively: f_minting *)
let (fminus : float) = 1.9;; (* dimensionless. Alternatively: f_liquidation *)
let (creation_deposit : tez) = 1.0;;
let (liquidation_reward_percentage : float) = 0.001;; (* TEZ% TODO: Use cNp *)

(** Percentage kept by the uniswap contract from the return asset. *)
let (uniswap_fee_percentage : float) = 0.002;; (* TODO: Use cNp *)

(* Protected index epsilon. The higher this value is, the faster the protected
 * index catches up with the actual index. *)
let protected_index_epsilon = 0.0005

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
  b.collateral_tez < fplus *. b.outstanding_kit *. (p.q *. tz_minting p)

(** Create a burrow without any tez collateral or outstanding kit. George: With
  * the current rules, this burrow is already undercollateralized, since 0 < 0
  * is false. *)
let create_burrow () : burrow =
  { collateral_tez = 0.0;
    outstanding_kit = 0.0;
  }

(** Add non-negative collateral to a burrow. *)
let deposit_tez (t : tez) (b : burrow) : burrow =
  assert (t >= 0.0);
  { b with collateral_tez = b.collateral_tez +. t }

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let withdraw_tez (p : checker_parameters) (t : tez) (b : burrow) : burrow option =
  assert (t >= 0.0);
  let updated = { b with collateral_tez = b.collateral_tez -. t } in
  if is_overburrowed p updated
  then None
  else Some updated

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let mint_kits_from_burrow (p : checker_parameters) (k : kit) (b : burrow) =
  assert (k >= 0.0);
  let updated = { b with outstanding_kit = b.outstanding_kit +. k } in
  if is_overburrowed p updated
  then None
  else Some updated

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
  b.collateral_tez < fminus *. b.outstanding_kit *. (p.q *. tz_liquidation p)

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
let compute_tez_to_auction (p : checker_parameters) (b : burrow) : tez =
  (b.outstanding_kit *. fplus *. p.q *. tz_minting p -. b.collateral_tez)
  /. (fplus -. 1.)

let compute_expected_kit (p : checker_parameters) (tez_to_auction: tez) : kit =
  tez_to_auction /. (p.q *. tz_minting p)

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

let print_uniswap (u : uniswap) =
  printf
    "{tez = %.15f tez;\n kit = %.15f; \n total_liquidity_tokens = %d}\n"
    u.tez
    u.kit
    u.total_liquidity_tokens

let uniswap_non_empty(u: uniswap) =
  u.kit > 0. && u.tez > 0.

let sell_kit (uniswap: uniswap) (kit: kit) : tez * kit * uniswap =
  (* Utku: I think, as long as the contract has non-zero tez and kit this
   * always succeeds.
   *
   * So, I think the function should fail when the contract is missing either
   * currency. It will presumably be started with some amount of tez, and
   * the first minting fee will initialize the kit amount.
  *)
  if not (uniswap_non_empty uniswap)
  then failwith "uniswap_non_empty"
  else
    assert (kit > 0.);

  let price = uniswap.tez /. uniswap.kit in
  let slippage = uniswap.kit /. (uniswap.kit +. kit) in
  let return = kit *. price *. slippage *. (1. -. uniswap_fee_percentage) in
  let updated = { uniswap with
                  kit = uniswap.kit +. kit;
                  tez = uniswap.tez -. return } in
  (return, 0.0, updated)

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
  if not (uniswap_non_empty uniswap)
  then failwith "uniswap_non_empty"
  else
    let ratio = uniswap.tez /. uniswap.kit in
    let given = tez /. kit in
    (* There is a chance that the given tez and kit have the wrong ratio,
     * so we liquidate as much as we can and return the leftovers. NOTE:
     * Alternatively, the LP can use the uniswap contract to get the right
     * ratio beforehand.
     * Invariant here is that (tez', kit') should have the correct ratio.
    *)
    let (tez', kit') =
      if given > ratio then (kit *. ratio, kit)
      else if given < ratio then (tez, tez /. ratio)
      else (tez, kit) in
    let liquidity =
      if uniswap.total_liquidity_tokens = 0
      then 1
      else int_of_float (floor (
          float_of_int uniswap.total_liquidity_tokens
          *. tez'
          /. uniswap.tez))
    in
    let updated =
      { kit = uniswap.kit +. kit';
        tez = uniswap.tez +. tez';
        total_liquidity_tokens =
          uniswap.total_liquidity_tokens + liquidity } in
    (liquidity, tez -. tez', kit -. kit', updated)

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
  let tez = uniswap.tez *. ratio in
  let kit = uniswap.kit *. ratio in
  let updated = {
    tez = uniswap.tez -. tez;
    kit = uniswap.kit -. kit;
    total_liquidity_tokens = uniswap.total_liquidity_tokens - liquidity } in
  (tez, kit, updated)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

let cnp (i: float) : float = i /. 100.

let sign (i: float) : float =
  if i > 0. then 1.
  else if i == 0. then 0.
  else -1.

let clamp (v: 'a) (lower: 'a) (upper: 'a) : float =
  assert (lower <= upper);
  min upper (max v lower)

type checker =
  { burrows : burrow Map.Make(String).t; (* TODO: Create an 'address' type *)
    uniswap : uniswap;
    parameters : checker_parameters;
  }

(* Utku: Thresholds here are cnp / day^2, we should convert them to cnp /
 * second^2, assuming we're measuring time in seconds. My calculations might be
 * incorrect. *)
(* George: Note that we don't really need to calculate the logs here (which can
 * be lossy); we can instead exponentiate the whole equation (exp is monotonic)
 * and win some precision, like this:
 *
    let compute_drift_derivative_2 (target : float) : float =
      assert (target > 0.);
      match () with
      (* No acceleration (0) *)
      | () when exp (-. 0.5 /. 100.) < target && target < exp (0.5 /. 100.) -> 0.
      (* Low acceleration (-/+) *)
      | () when exp (-. 5.0 /. 100.) < target && target <= exp (-. 0.5 /. 100.) -> -. (cnp 0.01 /. (24. *. 3600.) ** 2.)
      | () when exp    (5.0 /. 100.) > target && target >= exp    (0.5 /. 100.) ->    (cnp 0.01 /. (24. *. 3600.) ** 2.)
      (* High acceleration (-/+) *)
      | () when target <= exp (-. 5.0 /. 100.) -> -. (cnp 0.05 /. (24. *. 3600.) ** 2.)
      | () when target >= exp    (5.0 /. 100.) ->    (cnp 0.05 /. (24. *. 3600.) ** 2.)
      | _ -> failwith "impossible"
 *
 * NOTE: This implementation already gives different results on the swing
 * points that compute_drift_derivative does, possibly due to precision issues.
*)
let compute_drift_derivative (target : float) : float =
  assert (target > 0.);
  let log_target = log target in
  let abs_log_target = Float.abs log_target in
  if abs_log_target < cnp 0.5 then
    0.
  else if abs_log_target < cnp 5.0 then
    sign log_target *. (cnp 0.01 /. (24. *. 3600.) ** 2.)
  else
    sign log_target *. (cnp 0.05 /. (24. *. 3600.) ** 2.)

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
    parameters.protected_index
    *. clamp
      (current_index /. parameters.protected_index)
      lower_lim
      upper_lim in
  let current_drift' =
    compute_drift_derivative parameters.target in
  let current_drift =
    parameters.drift
    +. (1. /. 2.)
       *. (parameters.drift' +. current_drift')
       *. duration_in_seconds in

  (* TODO: use integer arithmetic *)
  let current_q =
    parameters.q
    *. exp ( ( parameters.drift
               +. (1. /. 6.)
                  *. (2. *. parameters.drift' +. current_drift')
                  *. duration_in_seconds )
             *. duration_in_seconds ) in
  let current_target = current_q *. current_index /. current_kit_in_tez in
  {
    index = current_index;
    protected_index = current_protected_index;
    target = current_target;
    drift = current_drift;
    drift' = current_drift';
    q = current_q
  }

(* ************************************************************************* *)
(* ************************************************************************* *)

type liquidation_result =
  | Unwarranted of burrow
  | Partial of float * tez * kit * burrow
  | Complete of float * tez * kit * burrow
  | Close of float * tez * kit

let print_liquidation_result (r: liquidation_result) =
  match r with
  | Unwarranted burrow ->
    printf "Unwarranted Liquidation\n";
    print_burrow burrow
  | Partial (reward, tez_to_sell, expected_kit, burrow) ->
    printf "Partial Liquidation\n";
    printf "liquidation_reward: %.15f\n" reward;
    printf "tez_to_sell: %.15f\n" tez_to_sell;
    printf "expected_kit: %.15f\n" expected_kit;
    print_burrow burrow
  | Complete (reward, tez_to_sell, expected_kit, burrow) ->
    printf "Complete Liquidation (deplete the collateral)\n";
    printf "liquidation_reward: %.15f\n" reward;
    printf "tez_to_sell: %.15f\n" tez_to_sell;
    printf "expected_kit: %.15f\n" expected_kit;
    print_burrow burrow
  | Close (reward, tez_to_sell, expected_kit) ->
    printf "Complete Liquidation (close the burrow)\n";
    printf "liquidation_reward: %.15f\n" reward;
    printf "tez_to_sell: %.15f\n" tez_to_sell;
    printf "expected_kit: %.15f\n" expected_kit

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
  let liquidation_limit = b.collateral_tez /. (fminus *. (p.q *. tz_liquidation p)) in
  let partial_reward = liquidation_reward_percentage *. b.collateral_tez in
  (* The reward for triggering a liquidation. This amounts to the burrow's
   * creation deposit, plus the liquidation reward percentage of the burrow's
   * collateral. Of course, this only applies if the burrow qualifies for
   * liquidation. This reward is to be given to the ator triggering the
   * liquidation. *)
  let liquidation_reward = creation_deposit +. partial_reward in
  (* Case 1: The outstanding kit does not exceed the liquidation limit; don't
   * liquidate. *)
  if b.outstanding_kit <= liquidation_limit then
    Unwarranted b
  (* Case 2: Cannot even refill the creation deposit; liquidate the whole
   * thing (after paying the liquidation reward of course). *)
  else if b.collateral_tez < liquidation_reward then
    let tez_to_auction = b.collateral_tez -. partial_reward in
    let expected_kit = compute_expected_kit p tez_to_auction in
    Close (liquidation_reward, tez_to_auction, expected_kit)
  (* Case 3: With the current price it's impossible to make the burrow not
   * undercollateralized; pay the liquidation reward, stash away the creation
   * deposit, and liquidate all the remaining collateral, even if it is not
   * expected to repay enough kit. *)
  else if b.outstanding_kit *. p.q *. tz_minting p > b.collateral_tez -. liquidation_reward then
    let b_without_reward = { b with collateral_tez = b.collateral_tez -. liquidation_reward } in
    let tez_to_auction = b_without_reward.collateral_tez in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { collateral_tez = b_without_reward.collateral_tez -. tez_to_auction; (* NOTE: SHOULD BE ZERO *)
        outstanding_kit = b_without_reward.outstanding_kit -. expected_kit;
      } in
    Complete (liquidation_reward, tez_to_auction, expected_kit, final_burrow)
  (* Case 4: Recovery is possible; pay the liquidation reward, stash away the
   * creation deposit, and liquidate only the amount of collateral needed to
   * underburrow the burrow (as approximated now). No more, no less. *)
  else
    let b_without_reward = { b with collateral_tez = b.collateral_tez -. liquidation_reward } in
    let tez_to_auction = compute_tez_to_auction p b_without_reward in
    let expected_kit = compute_expected_kit p tez_to_auction in
    let final_burrow =
      { collateral_tez = b_without_reward.collateral_tez -. tez_to_auction;
        outstanding_kit = b_without_reward.outstanding_kit -. expected_kit;
      } in
    Partial (liquidation_reward, tez_to_auction, expected_kit, final_burrow)

let burrow_experiment () =
  (* OTHER EXAMPLES *)
  (* Unwarranted liquidation for
  let initial_burrow = { outstanding_kit = 10.0; collateral_tez = 10.0; } in *)
  (* Partial liquidation for
  let initial_burrow = { outstanding_kit = 20.0; collateral_tez = 10.0; } in *)
  (* Complete liquidation (deplete the collateral, but keep the burrow) for
  let initial_burrow = { outstanding_kit = 100.0; collateral_tez = 10.0; } in *)
  (* Complete liquidation (close the burrow) for
  let initial_burrow = { outstanding_kit = 100.0; collateral_tez = 1.001; } in *)
  let initial_burrow =
    { outstanding_kit = 20.0;
      collateral_tez = 10.0;
    } in
  printf "\n=== Initial burrow state ===\n";
  print_burrow initial_burrow;
  let params =
    { q = 1.015;
      index = 0.32;
      protected_index = 0.36;
      target = 1.08;
      drift = 0.0;
      drift' = 0.0;
    } in
  printf "\n=== Checker parameters ===\n";
  print_checker_parameters params;

  printf "\n=== State of affairs ===\n";
  printf "Overburrowed          : %B\n" (is_overburrowed params initial_burrow);
  printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params initial_burrow);
  printf "\n=== Liquidation request outcome ===\n";
  let liquidation_result = request_liquidation params initial_burrow in
  print_liquidation_result liquidation_result;

  printf "\n=== State of affairs ===\n";
  match liquidation_result with
  | Partial (_,_,_,b) | Complete (_,_,_,b) | Unwarranted b ->
    printf "Overburrowed          : %B\n" (is_overburrowed params b);
    printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params b)
  | Close (_,_,_) ->
    printf "There is no burrow left to consider.\n"

let uniswap_experiment () =
  let uniswap = { tez=10.; kit=5.; total_liquidity_tokens=1 }; in
  let (tez, kit, uniswap) = sell_kit uniswap 1. in
  printf "Returned tez: %f\n" tez;
  printf "Returned kit: %f\n" kit;
  print_uniswap uniswap;
  print_newline ();
  let (liq, tez, kit, uniswap) = buy_liquidity uniswap 20. 20. in
  printf "Returned liquidity: %d\n" liq;
  printf "Returned tez: %f\n" tez;
  printf "Returned kit: %f\n" kit;
  print_uniswap uniswap

let step_experiment () =
  let initial_parameters = { q = 0.9;
                             index = 0.36;
                             target = 1.08;
                             protected_index = 0.35;
                             drift = 0.0;
                             drift' = 0.0;
                           } in
  let interblock_time = Seconds 3600 in
  let new_index = 0.34 in
  let tez_per_kit = 0.305 in
  let new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
  printf "\n=== Initial checker parameters ===\n";
  print_checker_parameters initial_parameters;
  printf "\n=== New checker parameters ===\n";
  print_checker_parameters new_parameters

let () =
  burrow_experiment ();
  (* uniswap_experiment (); *)
  (* step_experiment (); *)
  printf "\ndone.\n"

