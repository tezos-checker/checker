open Kit
open Tez
open Avl
open Address

(* Stub types *)
type burrow_id = unit
type block_number = unit
type time = unit
type 'a ticket = unit

type slice_ptr = BigMap.ptr
type liquidation_slice = { burrow_id: burrow_id; expected_kit: Kit.t; tez: Tez.t }

type tree = BigMap.ptr option

type auction_start = { block_number: block_number; time: time }

type bid = { address: Address.t; kit_utxo: Kit.utxo }

type auction_id = unit
type bid_token = { auction_id: auction_id; bid: bid }

type current_auction = {
  id: auction_id;
  contents: tree;
  start: auction_start;
  leading_bid: bid
}

type auctions = {
  storage: liquidation_slice mem;

  queued_slices: tree;
  current_auction: current_auction;
  completed_auctions: tree list;

  leading_bid: bid_token option;
}

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
 *
 * Given a pointer to a leaf, we can follow the parent pointers in the tree to eventually
 * get to the tree node. Then we can decide on the state of the liquidation by checking if
 * we ended up in 'queued_slices', 'current_auction', or in one of 'completed_auctions'.
 * We might convert 'completed_auctions' to a map from a tree root for performance reasons.
 *)
val send_to_auction : auctions -> liquidation_slice -> auctions * slice_ptr option

(**
 * - Check there's an auction
 * - Only accept certain amounts
*)
val place_bid : auctions -> bid -> auctions * (bid_token ticket) option
(*
 * - Decrease / increase price gradually
 * - Close an auction if complete
 * - Called from Huxian contract's "touch" operation
 * touch   auctions -> auctions
 *
 * - Check token is not the leading bid for the current lot
 * claim_failed_bid   auctions -> bid_token -> auctions * kit.utxo option
 *
 * claim_winning_bid  auctions -> bid_token -> auctions * kit.tez option
 *
 * claim_sold   auctions -> burrow_id -> auctions * tez
 *
 * - Cancel auction
 *
 * TODO: how to see current leading bid? FA2?
 * TODO: return kit to losing bidders
 * TODO: when liquidation result was "close", what happens after the tez is sold? Might we find that we didn't need to close it after all?
 *)
