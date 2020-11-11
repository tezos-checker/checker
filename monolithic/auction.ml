(*
Utku: Lifecycle of liquidation slices.

1. When a 'liquidation_slice' is created from a burrow, it is
   added to the 'queued_slices' queue. This is backed by an AVL
   tree.

   Adding something to the tree returns a 'leaf_ptr'.  This needs to
   be stored in a FIFO queue inside the burrow. This queue needs to have an
   efficient prepend and append methods.

2. When checker is touched, and there is no existing auction
   going on, a prefix of the 'queued_slices' is split, and inserted
   as 'current_auction' alongside with the current timestamp. This
   process is efficient because of the AVL tree.

   When the prefix returned does not have as much tez as we
   want, this means that the next liquidation_slice in 'queued_slices'
   needs to be split. We can do this by popping the first item if exists,
   which as an invariant has more tez than we need, and splitting
   it into two slices, appending the first slice to the end of
   'current_auction', and the second one to back to the front of
   'queued_slices'. If there is no more slices, we still start the
   auction.

   While doing this, we also need to pass this information to the
   burrow. And make sure that it references the new slices rather
   than the original.

   (NOTE, an idea for efficiently handling splits on burrow side:
    We can do that by maintaining another 'split_slices' queue
    storing a FIFO queue of (original_leaf, first_half, second_half) triplets
    within the burrow. When a burrow is traversing it's liquidation slices,
    it has to check it against the head of the 'split_slices' queue, and
    replace it by the split counterparts if it matches.)

   Alternatively, we should store burrows slices within an AVL tree, then
   we can efficiently replace a slice with two new slices eagerly.

3. Then an auction starts:

   > If there are any lots waiting to be auctioned, Checker starts an open,
   > ascending bid auction. There is a reserve price set at tz_t which declines
   > exponentially over time as long as no bid as been placed. Once a bid
   > is placed, the auction continues. Every bid needs to improve over the
   > previous bid by at least 0.33 cNp and adds the longer of 20 blocks or
   > 20 minutes, to the time before the auction expires.

   TODO: Figure out how a `bid` should be represented as.

   Every bid here is either ends as a 'leading_bid' in 'current_auction', or in
   'unclaimed_bids' set when overbid.

   At any time, the owner of a bid can claim an bid if it is in 'unclaimed_bid'
   set. If it is, it is removed and the amount of kit is given back.

   When an auction expires, the current auction is both moved to 'completed_auctions'
   FIFO queue, and to another map from tree_ptr to auction_outcome called `outcomes`.

4. When a burrow is touched, it checks if the head of its slices belongs to
   a completed auction or not. This can be checked via the 'find_root'
   function of AVL trees. If we end up in 'queued_slices' or 'curent_auction',
   it is not complete, if not, it means that we're in 'completed_auctions'.

   If the slice is completed, then the burrows outstanding kit balance is
   adjusted (FIXME, clarify, quite a bit of stuff to do on burrow side,
   but AFAIK they are already implemented) and the leaf is deleted from
   the tree.

   If the deletion causes the tree to become empty, then the tree is popped from
   `outcomes` map, and the leading bid is moved to `unclaimed_bid` set.

5. When checker is touched, it fetches the oldest auction from `completed_auctions`
   queue, and processes a few of oldest liquidations via touching their burrows. This
   is to clean-up the slices that haven't been claimed for some reason.

NOTE: !!! Below code does not yet completely reflect the above process.

Notes on data structures:

* When we need a queue we can prepend and append, we can simply use a
((int, 't) bigmap, int, int) triplet, where the bigmap contains consecutive
integer keys, and we also store the bounds of the keys of the map. Prepending
and appending (and maybe updating) can be done efficiently.

* If we use an AVL tree as a queue, then we also have the ability to delete or
insert elements in the middle. However, this requires us to write our AVL tree
more generically, and also much slower than the above method. So I don't think
we should do this.
*)

open Avl
open Address

type Error.error +=
  | NoOpenAuction
  | BidTooLow
  | CannotReclaimLeadingBid
  | NotAWinningBid

(* Stub types *)
type burrow_id = unit
type 'a ticket = 'a

type liquidation_slice = {
  burrow: Address.t;
  tez: Tez.t
}

type bid = { address: Address.t; kit: Kit.t }

type auction_id = avl_ptr
type bid_details = { auction_id: auction_id; bid: bid; }
type bid_ticket = bid_details Ticket.ticket

type auction_state =
  | Descending of Kit.t * Timestamp.t
  | Ascending of bid * Timestamp.t

type current_auction = {
  contents: avl_ptr;
  state: auction_state;
}

type auction_outcome = {
  sold_tez: Tez.t;
  winning_bid: bid;
}

module PtrMap =
  Map.Make(struct
    type t = avl_ptr
    let compare (AVLPtr a) (AVLPtr b) = Int64.compare a b end)

type auctions = {
  storage: liquidation_slice mem;

  queued_slices: avl_ptr;
  current_auction: current_auction option;
  completed_auctions: auction_outcome PtrMap.t;
}

let empty : auctions =
  let storage = BigMap.BigMap.empty in
  let (storage, queued_slices) = Avl.mk_empty storage in
  { storage = storage;
    queued_slices = queued_slices;
    current_auction = None;
    completed_auctions = PtrMap.empty;
  }

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let send_to_auction
    (auctions: auctions)
    (slice: liquidation_slice)
  : auctions * leaf_ptr =
  let (new_storage, ret) =
    push_back auctions.storage auctions.queued_slices slice slice.tez in
  let new_state = { auctions with storage = new_storage; } in
  (new_state, ret)

let cancel_liquidation
    (auctions: auctions)
    (slice: leaf_ptr)
  : auctions option =
  if find_root auctions.storage slice = auctions.queued_slices
  then
    (* if the slice belongs to queued_slices tree, we can cancel it *)
    let new_storage = del auctions.storage slice in
    Some { auctions with storage = new_storage; }
  else
    (* otherwise, it means that the auction is either in progress
     * or completed, so we can not cancel it. *)
    None

(* TODO: Use precice calculations here (Z, Q) for precision, not FixedPoint. *)
let liquidation_outcome
    (auctions: auctions)
    (leaf_ptr: leaf_ptr)
  : (auctions * Kit.t) option =
  let root = find_root auctions.storage leaf_ptr in
  match PtrMap.find_opt root auctions.completed_auctions with
  | None -> None (* slice does not correspond to a completed auction *)
  | Some outcome ->
    let (slice, _) = read_leaf auctions.storage leaf_ptr in
    let kit = Kit.scale Kit.one FixedPoint.(
        (Tez.to_fp slice.tez) * (Kit.to_fp outcome.winning_bid.kit)
        / (Tez.to_fp outcome.sold_tez)) in

    (* Now we delete the slice from the lot, so it can not be
     * withdrawn twice, also to save storage. This might cause
     * the lot root to change, so we also update completed_auctions
     * to reflect that.
    *)
    let storage = del auctions.storage leaf_ptr in
    let auctions =
      { auctions with
        storage = storage;
      } in
    Some (auctions, kit)

let start_auction_if_possible
    (now: Timestamp.t) (start_price: Kit.t) (auctions: auctions): auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    (* TODO: The maximum lot size will be decided by the queue size.
     * It should be the greater of 10,000 tez or 5% of the total amount
     * in the auction queue. This is to avoid the auction queue being
     * too slow to liquidate if there’s a lot of tez to auction.
    *)
    (* TODO: When we did not get enough tez for a lot, we should
     * look at the next element and split it to fill up our lot,
     * and insert the leftover back in front of the queue.
     *
     * When this happens, we should also update the burrow of the
     * split item to make sure that the references are correct.
    *)
    let (storage, new_auction) =
      take
        auctions.storage
        auctions.queued_slices
        (Tez.of_mutez 10_000_000_000) in
    let current_auction =
      if is_empty storage new_auction
      then None
      else
        let start_value = Kit.scale start_price (Tez.to_fp (Avl.avl_tez storage new_auction))  in
        Some
          { contents = new_auction;
            state = Descending (start_value, now); } in
    { auctions with
      storage = storage;
      current_auction = current_auction;
    }

(* TODO also check if 20 blocks have passed *)
let is_auction_complete (now: Timestamp.t) (auction: current_auction) : bool =
  match auction.state with
  | Descending _ -> false
  | Ascending (_, t) ->
    Timestamp.to_seconds now > Timestamp.to_seconds t + 86400

let complete_auction_if_possible
    (now: Timestamp.t) (auctions: auctions): auctions =
  match auctions.current_auction with
  | None ->
    auctions
  | Some curr when not (is_auction_complete now curr) ->
    auctions
  | _ -> failwith "not_implemented"

(**
 * - Only accept certain amounts
*)
let place_bid (now: Timestamp.t) (auction: current_auction) (bid: bid)
  : (current_auction * bid_ticket ticket, Error.error) result =
  match auction.state with
  | Descending (start_value, start_time) ->
    let decay =
      FixedPoint.pow
        Constants.auction_decay_rate
        (Timestamp.seconds_elapsed ~start:start_time ~finish:now) in
    let min_bid = Kit.scale start_value decay in
    if Kit.compare bid.kit min_bid >= 0
    then
      Ok (
        { auction with state = Ascending (bid, now); },
        Ticket.create { auction_id = auction.contents; bid = bid; }
      )
    else Error BidTooLow
  | Ascending (winning_bid, _) ->
    if Kit.compare winning_bid.kit bid.kit < 0
    then
      Ok (
        { auction with state = Ascending (bid, now); },
        Ticket.create { auction_id = auction.contents; bid = bid; }
      )
    else Error BidTooLow

let with_current_auction
    (auctions: auctions)
    (f: current_auction -> (current_auction * 'a, Error.error) result)
  : (auctions * 'a, Error.error) result =
  match auctions.current_auction with
  | None -> Error NoOpenAuction
  | Some curr -> match f curr with
    | Error err -> Error err
    | Ok (updated, ret)
      -> Ok ({ auctions with current_auction = Some updated; }, ret)

let is_leading_current_auction
    (auctions: auctions) (bid_details: bid_details): bool =
  match auctions.current_auction with
  | Some auction when auction.contents = bid_details.auction_id ->
    (match auction.state with
     | Ascending (bid, _) -> bid = bid_details.bid
     | Descending _ -> false)
  | _ -> false

let completed_auction_won_by
    (auctions: auctions) (bid_details: bid_details): auction_outcome option =
  match PtrMap.find_opt bid_details.auction_id auctions.completed_auctions with
  | Some outcome when outcome.winning_bid = bid_details.bid -> Some outcome
  | _ -> None

let reclaim_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Kit.t, Error.error) result =
  let bid_details = Ticket.read bid_ticket in
  if is_leading_current_auction auctions bid_details
  || Option.is_some (completed_auction_won_by auctions bid_details)
  then Error CannotReclaimLeadingBid
  else
    (* TODO: punch tickets *)
    Ok bid_details.bid.kit

let reclaim_winning_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Tez.t, Error.error) result =
  let bid_details = Ticket.read bid_ticket in
  match completed_auction_won_by auctions bid_details with
  | Some outcome -> Ok outcome.sold_tez
  | _ -> Error NotAWinningBid


let touch (auctions: auctions) (now: Timestamp.t) (price: Kit.t) : auctions =
  auctions
  |> complete_auction_if_possible now
  |> start_auction_if_possible now price

let current_auction_tez (auctions: auctions) : Tez.t option =
  Option.map
    (fun auction -> avl_tez auctions.storage auction.contents)
    auctions.current_auction

(*
 * - Decrease / increase price gradually
 * - Close an auction if complete
 * - Called from Checker contract's "touch" operation
 * touch   auctions -> auctions
 *
 * - Check token is not the leading bid for the current lot
 * claim_failed_bid   auctions -> bid_ticket -> auctions * kit.utxo option
 *
 * claim_winning_bid  auctions -> bid_ticket -> auctions * kit.tez option
 *
 * claim_sold   auctions -> burrow_id -> auctions * tez
 *
 * - Cancel auction
 *
 * TODO: how to see current leading bid? FA2?
 * TODO: return kit to losing bidders
 * TODO: when liquidation result was "close", what happens after the tez is sold? Might we find that we didn't need to close it after all?
 *)
