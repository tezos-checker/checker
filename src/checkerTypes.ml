open Burrow
open UniswapTypes
open Parameters
open LiquidationAuctionTypes
open DelegationAuctionTypes

type burrow_id = Ligo.address

type checker =
  { burrows : (burrow_id, burrow) Ligo.big_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
  }

(** Make a fresh state. *)
let initial_checker =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    uniswap = uniswap_make_initial;
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    delegation_auction = delegation_auction_empty;
  }
