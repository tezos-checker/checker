open Kit

type bid = { address: Ligo.address; kit: kit }
[@@deriving show]

let bid_eq (b1: bid) (b2: bid) =
  let {address=a1; kit=k1} = b1 in
  let {address=a2; kit=k2} = b2 in
  a1 = a2 && k1 = k2

type slice_id = Ligo.nat
[@@deriving show]

type liquidation_slice = {
  burrow: Ligo.address;
  tez: Ligo.tez;
  min_kit_for_unwarranted: kit;
}
[@@deriving show]

type lot =
  { first_slice: slice_id;
    last_slice: slice_id;
    held_tez: Ligo.tez;
  }
[@@deriving show]

type auction_id = Ligo.nat
[@@deriving show]

type slice_handle = auction_id * slice_id
[@@deriving show]

type liquidation_auction_state =
  | Descending of (kit * Ligo.timestamp)
  | Ascending of (bid * Ligo.timestamp * Ligo.nat)
[@@deriving show]

type auction_outcome = {
  sold_tez: Ligo.tez;
  winning_bid: bid;
}
[@@deriving show]

type current_liquidation_auction = {
  auction_id: auction_id;
  lot: lot;
  state: liquidation_auction_state;
}

type liquidation_auctions = {
  slices: (slice_id, liquidation_slice) Ligo.big_map;
  lots: lot list;

  current_auction: current_liquidation_auction option;
  completed_auctions: (auction_id, lot * auction_outcome) Ligo.big_map;


  next_unclaimed_slice_id: slice_id;

  next_slice_id: slice_id;
  next_auction_id: auction_id;
}

let liquidation_auction_empty : liquidation_auctions =
  { slices=Ligo.Big_map.empty;
    lots=[];

    current_auction=None;
    completed_auctions=Ligo.Big_map.empty;

    next_unclaimed_slice_id=Ligo.nat_from_literal "0n";

    next_slice_id=Ligo.nat_from_literal "0n";
    next_auction_id=Ligo.nat_from_literal "0n";
  }
