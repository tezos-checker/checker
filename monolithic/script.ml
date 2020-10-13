
open Format

(*
collateral that will remain : (c - tez_to_auction - CREATION_DEPOSIT) :: TEZ
kits that we can considered repaid (probably what you mean by wrote off) : tez_to_auction / (tz_minting * q) :: KIT
so the position in kit will be: k - tez_to_auction / (tz_minting * q) :: KIT

and the ratio will be:  (c - tez_to_auction - CREATION_DEPOSIT) / (q * tz_liquidation * (k - tez_to_auction / (tz_minting * q))) :: DIMENSIONLESS

If (c - tez_to_auction - CREATION_DEPOSIT) / (q * tz_liquidation * (k - tez_to_auction / (tz_minting * q))) = F then

tez_to_auction = tz_minting * (c - CREATION_DEPOSIT - F * k * q * tz_liquidation) / (F * tz_liquidation - tz_minting)
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

(* ************************************************************************* *)
(**                               CONSTANTS                                  *)
(* ************************************************************************* *)
let (f : float) = 2.0;;              (* dimensionless *)
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

(* Create a burrow without any tez collateral or outstanding kit. *)
let createBurrow () : burrow =
  { collateral_tez = 0.0;
    outstanding_kit = 0.0;
  }

(* Add non-negative collateral to a burrow. *)
let depositTez (t : tez) (b : burrow) : burrow =
  if t < 0.0
    then failwith "depositTez: negative collateral given"
    else { b with collateral_tez = b.collateral_tez +. t }

(* Check that a burrow is not overburrowed *)
let isNotOverburrowed (p : parameters) (b : burrow) : bool =
  f *. b.outstanding_kit *. p.tz_minting *. p.q < b.collateral_tez

let isOverburrowed (p : parameters) (b : burrow) : bool =
  not (isNotOverburrowed p b)

(** Withdraw a non-negative amount of tez from the burrow, as long as this will
  * not overburrow it. *)
let withdrawTez (p : parameters) (t : tez) (b : burrow) : burrow option =
  let updated = { b with collateral_tez = b.collateral_tez -. t } in
  if t < 0.0
    then failwith "withdrawTez: negative amount given"
    else if isOverburrowed p updated
           then None
           else Some updated

(** Mint a non-negative amount of kits from the burrow, as long as this will
  * not overburrow it *)
let mintKitsFromBurrow (p : parameters) (k : kit) (b : burrow) =
  failwith "to be implemented"

(** The reward for triggering a liquidation. This amounts to the burrow's
  * creation deposit, plus the liquidation reward percentage of the burrow's
  * collateral. In the grand scheme of things, this should be given to the
  * actor triggering liquidation. *)
let computeLiquidationReward (p : parameters) (b : burrow) : tez =
  match isOverburrowed p b with
  | true  -> creation_deposit +. liquidation_reward_percentage *. b.collateral_tez
  | false -> 0.0 (* No reward if the burrow is OK *)

(** Compute the liquidation limit for a given burrow. The liquidation limit is
  * the maximum number of kits outstanding (given the collateral in the burrow)
  * that can be outstanding, so that the burrow cannot be marked for
  * liquidation. Essentially, if the following inequality is not satisfied,
  *
  *   tez_collateral >= F * (q * tz_liquidation) * kits_outstanding
  *
  * then the burrow can be marked for liquidation.
*)
let computeLiquidationLimit (p : parameters) (b : burrow) : kit =
  b.collateral_tez /. (f *. (p.q *. p.tz_liquidation))
  (* TEZ / (TEZ / KIT) = KIT *)

(** Compute the number of tez that needs to be auctioned off so that the burrow
  * can return to a state when it is no longer overburrowed or having a risk of
  * liquidation. George: We need some precision here. *)
let computeTezToAuction (p : parameters) (b : burrow) : tez =
  (* TODO: This calculation is actually wrong (look at the types), but leave it like this until the results are reproduced. We'll fix it immediately afterwards *)
  (-1.0) (* NOTE: What the rest computes is really DeltaTez, which is negative (tez need to be auctioned). *)
    *. p.tz_minting
    *. (b.collateral_tez -. b.outstanding_kit *. f *. (p.q *. p.tz_liquidation))
    /. (f *. (p.q *. p.tz_liquidation) -. p.tz_minting) (* TODO: wrong. No q here. *)

(** Compute the amount of kits we expect to get from auctioning the needed tez.
  * TODO: Explain and elaborate on the equations. NOTE: Previously named
  * kit_to_write_off. *)
let computeExpectedKitFromAuction (p : parameters) (b : burrow) : kit =
  (* TODO: This calculation is actually wrong (look at the types), but leave it like this until the results are reproduced. We'll fix it immediately afterwards *)
  computeTezToAuction p b /. p.tz_minting (* TODO: wrong. No q here. *)

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
  printf "New collateral        : %.15f\n" final_burrow.collateral_tez;
  printf "New outstanding kit   : %.15f\n" final_burrow.outstanding_kit;

  let final_liquidation_limit = computeLiquidationLimit params final_burrow in
  printf "New liquidation limit : %.15f\n" final_liquidation_limit;
  printf "Still overburrowed    : %B\n" (final_burrow.outstanding_kit > final_liquidation_limit);

  printf "Hello, %s world\n%!" "cruel";

