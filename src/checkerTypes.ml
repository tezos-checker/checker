open Kit
open Tok
open FixedPoint
open Burrow
open CfmmTypes
open Parameters
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes
open Fa2Ledger
open Fa2Interface
open Common
open Error

type burrow_map = (burrow_id, burrow) Ligo.big_map

type external_contracts = {
  oracle : Ligo.address;
  collateral_fa2 : Ligo.address;
  ctok_fa2 : Ligo.address;
  ctez_cfmm : Ligo.address;
}

type checker =
  { burrows : burrow_map;
    cfmm : cfmm;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    last_index : fixedpoint option;
    last_ctez_in_tez : ratio option;
    fa2_state : fa2_state;
    external_contracts : external_contracts;
  }

(** Make a fresh state. *)
let initial_checker (external_contracts: external_contracts) =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    cfmm = initial_cfmm ();
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    last_index = (None : fixedpoint option);
    last_ctez_in_tez = (None : ratio option);
    fa2_state = initial_fa2_state;
    external_contracts = external_contracts;
  }

type lazy_function_id = Ligo.int

type deployment_state =
  | Unsealed of Ligo.address
  | Sealed of checker

type lazy_function_map = (lazy_function_id, Ligo.bytes) Ligo.big_map
type wrapper =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  { lazy_functions : lazy_function_map
  ; metadata: (string, Ligo.bytes) Ligo.big_map
  ; deployment_state : deployment_state
  }

[@@@coverage off]

type view_current_liquidation_auction_details_result =
  { auction_id: liquidation_auction_id
  ; collateral: tok
  ; minimum_bid: kit
  ; current_bid: bid option
  ; remaining_blocks: Ligo.int option
  ; remaining_seconds: Ligo.int option
  }
[@@deriving show]

[@@@coverage on]

(* ************************************************************************* *)
(**                           EXTERNAL_CONTRACTS                             *)
(* ************************************************************************* *)

let[@inline] get_transfer_ctok_fa2_entrypoint (external_contracts: external_contracts): (fa2_transfer list) Ligo.contract =
  match (LigoOp.Tezos.get_entrypoint_opt "%transfer" external_contracts.ctok_fa2 : (fa2_transfer list) Ligo.contract option) with
  | Some c -> c
  | None -> (Ligo.failwith error_GetEntrypointOptFailureFA2Transfer : (fa2_transfer list) Ligo.contract)

let[@inline] get_ctez_cfmm_price_entrypoint (external_contracts: external_contracts): ((Ligo.nat * Ligo.nat) Ligo.contract) Ligo.contract =
  match (LigoOp.Tezos.get_entrypoint_opt "%getMarginalPrice" external_contracts.ctez_cfmm : ((Ligo.nat * Ligo.nat) Ligo.contract) Ligo.contract option) with
  | Some c -> c
  | None -> (Ligo.failwith error_GetEntrypointOptFailureCtezGetMarginalPrice : ((Ligo.nat * Ligo.nat) Ligo.contract) Ligo.contract)

let[@inline] get_transfer_collateral_fa2_entrypoint (external_contracts: external_contracts): (fa2_transfer list) Ligo.contract =
  match (LigoOp.Tezos.get_entrypoint_opt "%transfer" external_contracts.collateral_fa2 : (fa2_transfer list) Ligo.contract option) with
  | Some c -> c
  | None -> (Ligo.failwith error_GetEntrypointOptFailureFA2Transfer : (fa2_transfer list) Ligo.contract)
