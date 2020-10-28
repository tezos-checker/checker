open Kit
open Tez
open Common
open Avl

(* Stub types *)
type burrow_id = unit
type block_number = unit
type time = unit
type 'a ticket = unit


type slice_id = unit
type liquidation_slice = { burrow_id: burrow_id; expected_kit: Kit.t; tez: Tez.t }

type tree = BigMap.ptr option

type auction_start = { block_number: block_number; time: time }

type bid  = { address: Common.address; kit_utxo: Kit.utxo }

type auction_id = unit
type bid_token = { auction_id: auction_id; bid: bid }

type current_lot = { id: auction_id; queued_stuff: tree; start: auction_start; leading_bid: bid }

type auctions = {
  storage: BigMap.ptr option;

  queued_stuff: tree;
  active_lot: tree;
  sold_stuff: tree;

  leading_bid: bid_token option;
}

val send_to_auction : auctions -> liquidation_slice -> auctions * slice_id option

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
