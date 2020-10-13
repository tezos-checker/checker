
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
    outstanding_kits : kit;
  }

(* ************************************************************************* *)
(**                               CONSTANTS                                  *)
(* ************************************************************************* *)
let (f : float) = 2.0;;
let (creation_deposit : tez) = 1.0;;
let (liquidation_reward_percentage : float) = 0.001;; (* TEZ% *)

(* ************************************************************************* *)
(**                           SYSTEM PARAMETERS                              *)
(* ************************************************************************* *)
type parameters =
  { q : float;            (* 1/kit, really *)
    tz_minting : tez;     (* TODO: Not tez/kit? *)
    tz_liquidation : tez; (* TODO: Not tez/kit? *)
  }

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

(* Create a burrow without any collateral or outstanding kits. *)
let createBurrow () : burrow =
  { collateral_tez = 0.0;
    outstanding_kits = 0.0;
  }

(* Add non-negative collateral to a burrow. *)
let depositTez (t : tez) (b : burrow) : burrow =
  if t < 0.0
    then failwith "depositTez: negative collateral given"
    else { b with collateral_tez = b.collateral_tez +. t }

(* Check that a burrow is not overburrowed *)
let isNotOverburrowed (p : parameters) (b : burrow) : bool =
  f *. b.outstanding_kits *. p.tz_minting *. p.q < b.collateral_tez

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

(* ************************************************************************* *)
(* ************************************************************************* *)
(* ************************************************************************* *)

let () =
  printf "Hello, %s world\n%!" "cruel";
  printf "Hello, %d times!\n%!" 42

