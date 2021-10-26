open Kit
open Tok
open Burrow
open CfmmTypes
open Parameters
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes
open Fa2Ledger

type burrow_map = (burrow_id, burrow) Ligo.big_map

type external_contracts = {
  oracle : Ligo.address;
  collateral_fa2 : Ligo.address;
  ctez_fa12 : Ligo.address;
  ctez_cfmm : Ligo.address;
}

type checker =
  { burrows : burrow_map;
    cfmm : cfmm;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    last_index : Ligo.nat option;
    fa2_state : fa2_state;
    external_contracts : external_contracts;
  }

(** Make a fresh state. *)
let initial_checker (external_contracts: external_contracts) =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    cfmm = initial_cfmm ();
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    last_index = (None : Ligo.nat option);
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
