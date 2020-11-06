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
   'queued_slices'.

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

open Kit
open Tez
open Avl
open Address

(* Stub types *)
type burrow_id = unit
type block_number = unit
type time = unit
type 'a ticket = unit

type liquidation_slice = {
  burrow: Address.t;
  tez: Tez.t
}

type auction_start = { block_number: block_number; time: time }

type bid = { address: Address.t; kit: Kit.t }

type auction_id = unit
type bid_token = { auction_id: auction_id; bid: bid }

type current_auction = {
  id: auction_id;
  contents: avl_ptr;
  start: auction_start;
  leading_bid: bid option;
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

let liquidation_outcome
  (auctions: auctions)
  (leaf_ptr: leaf_ptr)
  : (auctions * Kit.t) option =
  let root = find_root auctions.storage leaf_ptr in
  match PtrMap.find_opt root auctions.completed_auctions with
    | None -> None (* slice does not correspond to a completed auction *)
    | Some outcome ->
      let (slice, _) = read_leaf auctions.storage leaf_ptr in
      let kit = Kit.of_fp FixedPoint.FixedPoint.(
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
  (auctions: auctions): auctions =
  match auctions.current_auction with
    | Some _ -> auctions
    | None ->
      (* TODO: The lot size will be decided by the queue size.
       * It should be the greater of 1,000 tez or 5% of the total amount
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
              (Tez.of_fp (FixedPoint.FixedPoint.of_int 10_000)) in
       let current_auction =
             if is_empty storage new_auction
             then None
             else Some
                    { id = ();
                      contents = new_auction;
                      start = { time = (); block_number = (); };
                      leading_bid = None; } in
       { auctions with
          storage = storage;
          current_auction = current_auction;
       }

(*
let complete_auction_if_possible
  (auctions: auctions): auctions =
  match auctions.current_auction with
    | None -> auctions
    | Some _ ->
      (* check if the auction is finished *)
      if failwith "not_implemented"
      then auctions
      else failwith "not_implemented"

*)

(**
 * - Check there's an auction
 * - Only accept certain amounts
*)
let place_bid : auctions -> bid -> auctions * (bid_token ticket) option =
  failwith "not implemented"

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
