open Kit
open LiquidationAuctionPrimitiveTypes
open Mem

type liquidation_auction_state =
  | Descending of (kit * Ligo.timestamp)
  | Ascending of (bid * Ligo.timestamp * Ligo.nat)
[@@deriving show]

type current_liquidation_auction = {
  contents: avl_ptr;
  state: liquidation_auction_state;
}
[@@deriving show]

type liquidation_auctions = {
  current_auction: current_liquidation_auction option;
}

let liquidation_auction_empty : liquidation_auctions =
  { current_auction = (None: current_liquidation_auction option);
  }
