open Kit
open LiquidationAuctionPrimitiveTypes
open Mem
open Avl

type liquidation_auction_state =
  | Descending of (kit * Ligo.timestamp)
  | Ascending of (bid * Ligo.timestamp * Ligo.nat)
[@@deriving show]

type current_liquidation_auction = {
  contents: avl_ptr;
  state: liquidation_auction_state;
}
[@@deriving show]

type completed_liquidation_auctions =
  { youngest: avl_ptr
  ; oldest: avl_ptr
  }
[@@deriving show]

type liquidation_auctions = {
  avl_storage: mem;

  queued_slices: avl_ptr;
  current_auction: current_liquidation_auction option;
  completed_auctions: completed_liquidation_auctions option;
}

let liquidation_auction_empty : liquidation_auctions =
  let avl_storage = mem_empty in
  let (avl_storage, queued_slices) = avl_mk_empty avl_storage (None: auction_outcome option) in
  { avl_storage = avl_storage;
    queued_slices = queued_slices;
    current_auction = (None: current_liquidation_auction option);
    completed_auctions = (None: completed_liquidation_auctions option);
  }
