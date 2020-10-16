
open Format;;

(* TODO: THINGS TO CONSIDER:

 * What if compute_tez_to_auction returns something positive?

   About switching to an integer representation for tez and kit
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Currently 1 tez (1 XTZ) is divisible to six decimal places, and the smallest
   unit is called a micro tez:

   1 tez = 100 cents = 1,000,000 micro tez (mutez)

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
(**                           SYSTEM PARAMETERS                              *)
(* ************************************************************************* *)
type parameters =
  { q : float; (* 1/kit, really *)
    index: tez;
    protected_index: tez;
    drift': float;
    drift: float;
  }

(* tez. To get tez/kit must multiply with q. *)
let tz_minting (p: parameters) : tez =
  max p.index p.protected_index
let tz_liquidation (p: parameters) : tez =
  min p.index p.protected_index

let clamp (v: 'a) (lower: 'a) (upper: 'a) : float =
  assert (lower <= upper);
  min upper (max v lower)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

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

(** Compute the maximum number of kit that can be outstanding from a burrow,
  * given the number of tez that have been deposited in it. The general limit
  * for burrowing is given by the following inequality:
  *
  *   fplus * kit_outstanding * (q * tz_mint) <= tez_collateral
  *
  * So we compute
  *
  *   kit_outstanding <= tez_collateral / (fplus * (q * tz_mint))
*)
let compute_burrowing_limit (p : parameters) (b : burrow) : kit =
  b.collateral_tez /. (fplus *. (p.q *. tz_minting p))

(** Check that a burrow is not overburrowed (that is, the kit outstanding does
  * not exceed the burrowing limit). *)
let is_not_overburrowed (p : parameters) (b : burrow) : bool =
  b.outstanding_kit <= compute_burrowing_limit p b

let is_overburrowed (p : parameters) (b : burrow) : bool =
  not (is_not_overburrowed p b)

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let withdraw_tez (p : parameters) (t : tez) (b : burrow) : burrow option =
  assert (t >= 0.0);
  let updated = { b with collateral_tez = b.collateral_tez -. t } in
  if is_overburrowed p updated
  then None
  else Some updated

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let mint_kits_from_burrow (p : parameters) (k : kit) (b : burrow) =
  assert (k >= 0.0);
  let updated = { b with outstanding_kit = b.outstanding_kit +. k } in
  if is_overburrowed p updated
  then None
  else Some updated

(* ************************************************************************* *)
(**                          LIQUIDATION-RELATED                             *)
(* ************************************************************************* *)

(** Compute the liquidation limit for a given burrow. The liquidation limit is
  * the maximum number of kits outstanding (given the collateral in the burrow)
  * that can be outstanding, so that the burrow cannot be marked for
  * liquidation. Essentially, if the following inequality is not satisfied,
  *
  *   tez_collateral >= fminus * (q * tz_liquidation) * kit_outstanding
  *
  * then the burrow can be marked for liquidation.
*)
let compute_liquidation_limit (p : parameters) (b : burrow) : kit =
  b.collateral_tez /. (fminus *. (p.q *. tz_liquidation p))
(* TEZ / (TEZ / KIT) = KIT *)

let should_burrow_be_liquidated (p : parameters) (b : burrow) : bool =
  b.outstanding_kit > compute_liquidation_limit p b

(** The reward for triggering a liquidation. This amounts to the burrow's
  * creation deposit, plus the liquidation reward percentage of the burrow's
  * collateral. Of course, if the burrow does not qualify for liquidation, the
  * reward is zero. In the grand scheme of things, this should be given to the
  * actor triggering liquidation. *)
let compute_liquidation_reward (p : parameters) (b : burrow) : tez =
  if should_burrow_be_liquidated p b
  then creation_deposit +. liquidation_reward_percentage *. b.collateral_tez
  else 0. (* No reward if the burrow should not be liquidated *)

(** The tez/kit price we expect to get when we liquidate is (q * tz_minting).
  * So if we auction Δcollateral tez, and we receive Δkit kit for it, the
  * following is expected to hold
  *
  *   Δcollateral = Δkit * (q * tz_minting)                          <=>
  *
  *   Δkit = Δcollateral / (q * tz_minting)                          (1)
  *
  * Furthermore, after liquidation, the burrow must not be liquidatable
  * anymore, so the following must hold
  *
  *   (C + Δcollateral) = (K + Δkit) * fplus * q * tz_liquidation    (2)
  *
  * Solving the above equations gives:
  *
  *   Δcollateral = tz_mint * (C - K*fplus*q*tz_liq) / (fplus*tz_liq - tz_mint)
  *   Δkit        = Δcollateral / (q * tz_minting)
*)

(** Compute the number of tez that needs to be auctioned off so that the burrow
  * can return to a state when it is no longer overburrowed or having a risk of
  * liquidation. George: We need some more accurate comments here. *)
let compute_tez_to_auction (p : parameters) (b : burrow) : tez =
  (-1.0) (* NOTE: What the rest computes is really DeltaTez, which is negative (tez need to be auctioned). *)
  *. tz_minting p
  *. (b.collateral_tez -. b.outstanding_kit *. fplus *. (p.q *. tz_liquidation p))
  /. (fplus *. tz_liquidation p -. tz_minting p)

(** Compute the amount of kits we expect to get from auctioning tez. *)
let compute_expected_kit_from_auction (p : parameters) (b : burrow) : kit =
  compute_tez_to_auction p b /. (p.q *. tz_minting p)

(*
I can think of the following outcomes:
  * A burrow does not need to be liquidated
  * A burrow needs to be liquidated partially
  * A burrow needs to be liquidated in its entirety
The stuff below does not even deal with the last scenario yet but it should. We
should also start looking into batching/auctioning after the basic stuff is out
of the way.
*)

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

type checker =
  { burrows : burrow Map.Make(String).t; (* TODO: Create an 'address' type *)
    uniswap : uniswap;
    parameters : parameters;
  }

(* Utku: Thresholds here are cnp / day^2, we should convert them to cnp /
 * second^2, assuming we're measuring time in seconds. My calculations might be
 * incorrect. *)
(* George: Note that we don't really need to calculate the logs here (which can
 * be lossy); we can instead exponentiate the whole equation (exp is monotonic)
 * and win some precision, like this:

    let computeDriftDerivative2 (target : float) : float =
      assert (target > 0.);
      match () with
      (* No acceleration (0) *)
      | () when exp (-. 0.5 /. 100.) < target && target < exp (0.5 /. 100.) -> 0.
      (* Low acceleration (-/+) *)
      | () when exp (-. 5.0 /. 100.) < target && target <= exp (-. 0.5 /. 100.) -> -. (cnp 0.01 /. (24. /. 60.) ** 2.)
      | () when exp    (5.0 /. 100.) > target && target >= exp    (0.5 /. 100.) ->    (cnp 0.01 /. (24. /. 60.) ** 2.)
      (* High acceleration (-/+) *)
      | () when target <= exp (-. 5.0 /. 100.) -> -. (cnp 0.05 /. (24. /. 60.) ** 2.)
      | () when target >= exp    (5.0 /. 100.) ->    (cnp 0.05 /. (24. /. 60.) ** 2.)
 *)
let compute_drift_derivative (target : float) : float =
  assert (target > 0.);
  let log_target = log target in
  let abs_log_target = Float.abs log_target in
  if abs_log_target < cnp 0.5 then
    0.
  else if abs_log_target < cnp 5.0 then
    sign log_target *. (cnp 0.01 /. (24. /. 60.) ** 2.)
  else
    sign log_target *. (cnp 0.05 /. (24. /. 60.) ** 2.)

(* TODO: Not tested, take it with a grain of salt. *)
let step (time_passed: int) (current_index: float) (checker: checker) : checker =
  (* Compute the new protected index, using the time interval, the current
   * index (given by the oracles right now), and the protected index of the
   * previous timestamp. *)
  let upper_lim = exp (protected_index_epsilon *. float_of_int time_passed) in
  let lower_lim = exp (-. protected_index_epsilon *. float_of_int time_passed) in
  let new_protected_index =
    checker.parameters.protected_index
    *. clamp
      (current_index /. checker.parameters.protected_index)
      lower_lim
      upper_lim in
  (* George: NOTE: I think that there is a small issue here:
   * checker.parameters.q and checker.parameters.index are from the previous
   * timestamp (as they should), but, if I understand correctly, kit_in_tez
   * refers to the current time. *)
  let new_drift' =
    let kit_in_tez =
      checker.uniswap.tez /. checker.uniswap.kit in
    let target =
      checker.parameters.q *. checker.parameters.index /. kit_in_tez in
    compute_drift_derivative target in
  let new_drift =
    checker.parameters.drift
    +. (1. /. 2.)
       *. (checker.parameters.drift' +. new_drift')
       *. float_of_int time_passed in
  let new_q =
    checker.parameters.q
    *. exp ( ( checker.parameters.drift
               +. (1. /. 6.)
                  *. (2. *. checker.parameters.drift' +. new_drift')
                  *. float_of_int time_passed )
             *. float_of_int time_passed ) in
  { checker with
    parameters = {
      index = current_index;
      protected_index = new_protected_index;
      drift = new_drift;
      drift' = new_drift';
      q = new_q }
  }

(* ************************************************************************* *)
(* ************************************************************************* *)

let burrow_experiment () =
  let initial_burrow =
    { outstanding_kit = 20.0;
      collateral_tez = 10.0;
    } in
  let params =
    { q = 1.015;
      index = 0.32;
      protected_index = 0.36;
      drift = 0.;
      drift' = 0.;
    } in

  print_burrow initial_burrow;

  printf "Overburrowed          : %B\n" (is_overburrowed params initial_burrow);
  printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params initial_burrow);

  let reward = compute_liquidation_reward params initial_burrow in
  printf "Reward                : %.15f\n" reward;

  (* NOTE: George: The initial state of the burrow is the collateral C, the
   * oustanding kit K, and the implicit creation deposit D (1 tez). I say
   * implicit because it does not count towards the collateral. So, in honesty,
   * the 1 tez in the reward does not come from the collateral, but it is the
   * creation deposit D. But then, in order to bring the burrow in a good
   * state, we need to stash away (implicitly, again) 1 tez as creation deposit
   * (this will be used if (a) the owner wants to close the burrow or (b) we
   * need to liquidate the burrow again). This one does come out of the
   * collateral though. From the outside it all looks the same I guess, minus
   * one plus one, but I thought that this intricacy is worth pointing out.
  *)
  let burrow_without_reward = { initial_burrow with collateral_tez = initial_burrow.collateral_tez -. reward } in
  printf "New collateral        : %.15f\n" burrow_without_reward.collateral_tez;

  let kit_to_receive = compute_expected_kit_from_auction params burrow_without_reward in
  printf "Kits to write off     : %.15f\n" kit_to_receive;

  let tez_to_auction = compute_tez_to_auction params burrow_without_reward in
  printf "Tez to auction        : %.15f\n" tez_to_auction;

  let final_burrow =
    { collateral_tez = burrow_without_reward.collateral_tez -. tez_to_auction;
      outstanding_kit = burrow_without_reward.outstanding_kit -. kit_to_receive;
    } in
  print_burrow final_burrow;

  let final_liquidation_limit = compute_liquidation_limit params final_burrow in
  printf "New liquidation limit : %.15f\n" final_liquidation_limit;
  let final_burrowing_limit = compute_burrowing_limit params final_burrow in
  printf "New burrowing   limit : %.15f\n" final_burrowing_limit;
  printf "Still overburrowed    : %B\n" (is_overburrowed params final_burrow);
  printf "Still liquidatable    : %B\n" (should_burrow_be_liquidated params final_burrow);

  printf "Hello, %s world\n%!" "cruel"

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

let () =
  burrow_experiment ();
  (* uniswap_experiment (); *)
  printf "\ndone.\n"

