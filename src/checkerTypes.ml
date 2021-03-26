open Burrow
open UniswapTypes
open Parameters
open LiquidationAuctionTypes
open DelegationAuctionTypes

type burrow_id = Ligo.address

type burrow_map = (burrow_id, burrow) Ligo.big_map

type ty_is_burrow_done_with_liquidations = liquidation_auctions -> Ligo.address -> bool
type ty_liquidation_auction_touch = liquidation_auctions -> ratio -> liquidation_auctions
type ty_liquidation_auction_send_to_auction = liquidation_auctions -> liquidation_slice_contents -> liquidation_auctions * leaf_ptr
type ty_liquidation_auction_cancel_slice = liquidation_auctions -> leaf_ptr -> liquidation_slice_contents * liquidation_auctions
type ty_liquidation_auction_pop_completed_slice = liquidation_auctions -> leaf_ptr -> liquidation_slice_contents * auction_outcome * liquidation_auctions

type lazy_function_id =
  | IsBurrowDoneWithLiquidations
  | LiquidationAuctionTouch
  | LiquidationAuctionSendToAuction
  | LiquidationAuctionCancelSlice
  | LqdtAuctionPopCompletedSlice

type checker =
  { burrows : burrow_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
    delegate : Ligo.key_hash option;
    last_price : Ligo.nat option;
    lazy_functions: (lazy_function_id, Ligo.bytes) Ligo.big_map;
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

    lazy_functions = (Ligo.Big_map.empty: (lazy_function_id, Ligo.bytes) Ligo.big_map);
  }
