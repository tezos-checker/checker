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
    last_price : Ligo.nat option;
  }

(** Make a fresh state. *)
let initial_checker =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    uniswap = initial_uniswap;
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    delegation_auction = delegation_auction_empty;
    delegate = (None : Ligo.key_hash option);
    last_price = (None : Ligo.nat option);
  }

(** The few components of checker's state needed when touching liquidation
  * slices. *)
type redacted_checker = liquidation_auctions * burrow_map * parameters
