
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

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

let () =
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

  printf "Hello, %s world\n%!" "cruel";

