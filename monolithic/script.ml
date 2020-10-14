
open Format

(* TODO: THINGS TO CONSIDER:

 * What if computeTezToAuction returns something positive?

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

(* ************************************************************************* *)
(**                           SYSTEM PARAMETERS                              *)
(* ************************************************************************* *)
type parameters =
  { q : float;            (* 1/kit, really *)
    tz_minting : tez;     (* tez. To get tez/kit must multiply with q. *)
    tz_liquidation : tez; (* tez. To get tez/kit must multiply with q. *)
  }

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(** Create a burrow without any tez collateral or outstanding kit. George: With
  * the current rules, this burrow is already undercollateralized, since 0 < 0
  * is false. *)
let createBurrow () : burrow =
  { collateral_tez = 0.0;
    outstanding_kit = 0.0;
  }

(** Add non-negative collateral to a burrow. *)
let depositTez (t : tez) (b : burrow) : burrow =
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
let computeBurrowingLimit (p : parameters) (b : burrow) : kit =
  b.collateral_tez /. (fplus *. (p.q *. p.tz_minting))

(** Check that a burrow is not overburrowed (that is, the kit outstanding does
  * not exceed the burrowing limit). *)
let isNotOverburrowed (p : parameters) (b : burrow) : bool =
  b.outstanding_kit <= computeBurrowingLimit p b

let isOverburrowed (p : parameters) (b : burrow) : bool =
  not (isNotOverburrowed p b)

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let withdrawTez (p : parameters) (t : tez) (b : burrow) : burrow option =
  assert (t >= 0.0);
  let updated = { b with collateral_tez = b.collateral_tez -. t } in
  if isOverburrowed p updated
  then None
  else Some updated

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let mintKitsFromBurrow (p : parameters) (k : kit) (b : burrow) =
  assert (k >= 0.0);
  let updated = { b with outstanding_kit = b.outstanding_kit +. k } in
  if isOverburrowed p updated
  then None
  else Some updated

(* ************************************************************************* *)
(**                          LIQUIDATION-RELATED                             *)
(* ************************************************************************* *)

(** The reward for triggering a liquidation. This amounts to the burrow's
  * creation deposit, plus the liquidation reward percentage of the burrow's
  * collateral. Of course, if the burrow does not qualify for liquidation, the
  * reward is zero. In the grand scheme of things, this should be given to the
  * actor triggering liquidation. *)
let computeLiquidationReward (p : parameters) (b : burrow) : tez =
  match isOverburrowed p b with
  | true  -> creation_deposit +. liquidation_reward_percentage *. b.collateral_tez
  | false -> 0.0 (* No reward if the burrow should not be liquidated *)

(** Compute the liquidation limit for a given burrow. The liquidation limit is
  * the maximum number of kits outstanding (given the collateral in the burrow)
  * that can be outstanding, so that the burrow cannot be marked for
  * liquidation. Essentially, if the following inequality is not satisfied,
  *
  *   tez_collateral >= fminus * (q * tz_liquidation) * kit_outstanding
  *
  * then the burrow can be marked for liquidation.
*)
let computeLiquidationLimit (p : parameters) (b : burrow) : kit =
  b.collateral_tez /. (fminus *. (p.q *. p.tz_liquidation))
(* TEZ / (TEZ / KIT) = KIT *)

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
let computeTezToAuction (p : parameters) (b : burrow) : tez =
  (-1.0) (* NOTE: What the rest computes is really DeltaTez, which is negative (tez need to be auctioned). *)
  *. p.tz_minting
  *. (b.collateral_tez -. b.outstanding_kit *. fplus *. (p.q *. p.tz_liquidation))
  /. (fplus *. p.tz_liquidation -. p.tz_minting)

(** Compute the amount of kits we expect to get from auctioning tez. *)
let computeExpectedKitFromAuction (p : parameters) (b : burrow) : kit =
  computeTezToAuction p b /. (p.q *. p.tz_minting)

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

(*
 * From Arthur:
 *
 * The general concept of uniswap is that you have quantity a of an asset A
 * and b of an asset B and you process buy and sell requests by maintaining
 * the product a * b constant. So if someone wants to sell a quantity da of
 * asset A to the contract, the balance would become (a + da) so you can
 * give that person a quantity db of asset B in exchange such that (a +
 * da)(b - db) = a b. Solving for db gives db  = da * b / (a + da). We
 * can rewrite this as db = da * (b / a) * (a / (a + da)) where (b / a)
 * represents the  "price" before the order and a / (a + da)  represents
 * the "slippage". Indeed, a property of uniswap is that with arbitrageurs
 * around, the ration a / b gives you the market price of A in terms of B.
 *
 * On top of that, we can add some fees, typically around 0.3 cNp. So the
 * equation becomes something like db = da * b / ( a + da) * (1 - 0.3/100)
 * (note that this formula is a first-order approximation in the sense that
 * two orders of size da / 2 will give you a better price than one order
 * of size da, but  the difference is far smaller than typical fees or any
 * amount we care about
 *)
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


let sell_kit (uniswap: uniswap) (kit: kit) : tez * kit * uniswap =
  (* Utku: I think, as long as the contract has non-zero tez and kit initially
   * this always succeeds and can not completely spend either after this.
   *
   * So, I think the function should fail when the contract is missing either
   * currency. It will presumably be started with some amount of tez, and
   * the first minting fee will initialize the kit amount.
  *)
  assert (uniswap.kit > 0.);
  assert (uniswap.tez > 0.);
  assert (kit > 0.);

  let price = uniswap.tez /. uniswap.kit in
  let slippage = uniswap.kit /. (uniswap.kit +. kit) in
  let return = kit *. price *. slippage in
  let updated = { uniswap with
                  kit = uniswap.kit +. kit;
                  tez = uniswap.tez -. return } in
  (return, 0.0, updated)

(* From Arthur:
 *
 * But where do the assets in uniswap come from? Liquidity providers, or
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

(* Adding liquidity always succeeds. *)
let add_liquidity (uniswap: uniswap) (tez: tez) (kit: kit)
  : tez * kit * liquidity * uniswap =
  let ratio = uniswap.tez /. uniswap.kit in
  let given = tez /. kit in
  (* There is a chance that the given tez and kit have the wrong ratio,
   * so we liquidate as much as we can and return the leftovers.
   * Invariant here is that (tez', kit') should have the correct ratio.
  *)
  let (tez', kit') =
    if given > ratio then (kit *. ratio, kit)
    else if given < ratio then (tez, tez /. ratio)
    else (tez, kit) in
  let liquidity =
    int_of_float (floor (
        float_of_int uniswap.total_liquidity_tokens
        *. tez'
        /. uniswap.tez)) in
  let updated =
    {  kit = uniswap.kit +. kit';
       tez = uniswap.tez +. tez';
       total_liquidity_tokens =
         uniswap.total_liquidity_tokens + liquidity } in
  (tez -. tez', kit -. kit', liquidity, updated)


(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

let burrow_experiment () =
  let initial_burrow =
    { outstanding_kit = 20.0;
      collateral_tez = 10.0;
    } in
  let params =
    { q = 1.015;
      tz_minting = 0.36;
      tz_liquidation = 0.32;
    } in

  print_burrow initial_burrow;

  let initial_liquidation_limit = computeLiquidationLimit params initial_burrow in
  printf "Overburrowed          : %B\n" (initial_burrow.outstanding_kit > initial_liquidation_limit);

  let reward = computeLiquidationReward params initial_burrow in
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

  let kit_to_receive = computeExpectedKitFromAuction params burrow_without_reward in
  printf "Kits to write off     : %.15f\n" kit_to_receive;

  let tez_to_auction = computeTezToAuction params burrow_without_reward in
  printf "Tez to auction        : %.15f\n" tez_to_auction;

  let final_burrow =
    { collateral_tez = burrow_without_reward.collateral_tez -. tez_to_auction;
      outstanding_kit = burrow_without_reward.outstanding_kit -. kit_to_receive;
    } in
  print_burrow final_burrow;

  let final_liquidation_limit = computeLiquidationLimit params final_burrow in
  printf "New liquidation limit : %.15f\n" final_liquidation_limit;
  printf "Still overburrowed    : %B\n" (final_burrow.outstanding_kit > final_liquidation_limit);

  printf "Hello, %s world\n%!" "cruel"

let uniswap_experiment () =
  let uniswap = { tez=1000.; kit=5000.; total_liquidity_tokens=1 }; in
  let (kit, tez, uniswap) = sell_kit uniswap 1. in
  printf "Returned tez: %f\nReturned kit: %f\n" tez kit;
  print_uniswap uniswap

let () =
  uniswap_experiment ();
  printf "done"
