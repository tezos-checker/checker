open Burrow
open CfmmTypes
open Parameters
open LiquidationAuctionTypes
open Fa2Interface

type burrow_map = (burrow_id, burrow) Ligo.big_map

type external_contracts = {
  oracle : Ligo.address;
  ctez : Ligo.address;
}

type checker =
  { burrows : burrow_map;
    cfmm : cfmm;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    last_price : Ligo.nat option;
    fa2_state : fa2_state;
    external_contracts : external_contracts;
  }

(** Make a fresh state. *)
let initial_checker (external_contracts: external_contracts) =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    cfmm = initial_cfmm;
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    last_price = (None : Ligo.nat option);
    fa2_state = initial_fa2_state;
    external_contracts = external_contracts;
  }
