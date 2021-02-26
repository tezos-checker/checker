open Burrow
open UniswapTypes
open Parameters
open LiquidationAuctionTypes
open DelegationAuctionTypes

type burrow_id = Ligo.address

type burrow_map = (burrow_id, burrow) Ligo.big_map

type checker =
  { burrows : burrow_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
    delegate : Ligo.key_hash option;
  }

(** Make a fresh state. *)
let initial_checker =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    uniswap = uniswap_make_initial;
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    delegation_auction = delegation_auction_empty;
    delegate = (None : Ligo.key_hash option);
  }
