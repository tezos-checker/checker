(*
Utku: Lifecycle of liquidation slices.

1. When a 'liquidation_slice' is created from a burrow, it is
   added to the 'queued_slices' queue. This is backed by an AVL
   tree.

   Every 'liquidation_slice' has pointers to the older and younger
   slice for that burrow. This forms a double-linked list of slices
   overlaying the AVL tree.

   Every burrow has pointers to the first and the last element of
   that linked list, so adding/popping elements both  from the front
   and the back is efficient.

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

   While doing this, we also need to make sure that the pointers on
   liquidation slices are still correct.

3. Then an auction starts:

   > If there are any lots waiting to be auctioned, Checker starts an open,
   > ascending bid auction. There is a reserve price set at tz_t which declines
   > exponentially over time as long as no bid as been placed. Once a bid
   > is placed, the auction continues. Every bid needs to improve over the
   > previous bid by at least 0.33 cNp and adds the longer of 20 blocks or
   > 20 minutes, to the time before the auction expires.

   All bids can be claimed, unless they are the winning bid of the current
   or any of the past auctions.

   When an auction expires, the current auction is both moved to
   'completed_auctions' FIFO queue. This FIFO queue is implemented as a doubly
   linked list at tree roots.

4. When 'touch_liquidation_slice' is called the slice is checked if it belongs
   to a completed auction or not. This can be checked via the 'find_root'
   function of AVL trees. If we end up in 'queued_slices' or 'curent_auction',
   it is not complete, if not, it means that we're in 'completed_auctions'.

   If the slice is completed, then the burrows outstanding kit balance is
   adjusted and the leaf is deleted from the tree. If it was the last remaining
   slice of the relevant auction (in other words, if the auction's tree becomes
   empty after the removal), the auctions is popped from the 'completed_auctions'
   linked list.

5. When checker is touched, it fetches the oldest auction from `completed_auctions`
   queue, and processes a few of oldest liquidations. This is to clean-up the slices
   that haven't been claimed for some reason.

6. When the winner of an auction tries to claim the result, we check if its auction
   has no liquidation_slice's left. If all the slices are already touched, this
   means that the tree is already popped out of the completed_auctions list. In
   this case, we transfer the result to the callee, and remove the auction alltogether.
*)

open LiquidationAuctionPrimitiveTypes
open Mem
open Ratio
open FixedPoint
open Kit
open Constants
open Common
open Tickets
open LiquidationAuctionTypes
open Error

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let liquidation_auction_send_to_auction (auctions: liquidation_auctions) (slice: liquidation_slice) : (liquidation_auctions * leaf_ptr) =
  failwith "foo"

(** Split a liquidation slice into two. We also have to split the
  * min_kit_for_unwarranted so that we can evaluate the two auctions separately
  * (and see if the liquidation was warranted, retroactively). Perhaps a bit
  * harshly, for both slices we round up. NOTE: Alternatively, we can calculate
  * min_kit_for_unwarranted_1 and then calculate min_kit_for_unwarranted_2 =
  * min_kit_for_unwarranted - min_kit_for_unwarranted_1. *)
let split_liquidation_slice (amnt: Ligo.tez) (slice: liquidation_slice) : (liquidation_slice * liquidation_slice) =
  assert (amnt > Ligo.tez_from_literal "0mutez");
  assert (amnt < slice.tez);
  (* left slice *)
  let ltez = amnt in
  let min_kit_for_unwarranted = kit_to_ratio slice.min_kit_for_unwarranted in
  let slice_tez = tez_to_mutez slice.tez in
  let lkit =
    kit_of_ratio_ceil
      (mul_ratio
         min_kit_for_unwarranted
         (make_ratio (tez_to_mutez ltez) slice_tez)
      ) in
  (* right slice *)
  let rtez = Ligo.sub_tez_tez slice.tez amnt in
  let rkit =
    kit_of_ratio_ceil
      (mul_ratio
         min_kit_for_unwarranted
         (make_ratio (tez_to_mutez rtez) slice_tez)
      ) in
  ( { slice with tez = ltez; min_kit_for_unwarranted = lkit; },
    { slice with tez = rtez; min_kit_for_unwarranted = rkit; }
  )

let start_liquidation_auction_if_possible
    (start_price: fixedpoint) (auctions: liquidation_auctions): liquidation_auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None -> (failwith "foo": liquidation_auctions)

(** Compute the current threshold for a bid to be accepted. For a descending
  * auction this amounts to the reserve price (which is exponentially
  * dropping). For a descending auction we should improve upon the last bid
  * a fixed factor. *)
let liquidation_auction_current_auction_minimum_bid (auction: current_liquidation_auction) : kit =
  match auction.state with
  | Descending params ->
    let (start_value, start_time) = params in
    let auction_decay_rate = fixedpoint_of_ratio_ceil auction_decay_rate in
    let decay =
      match Ligo.is_nat (Ligo.sub_timestamp_timestamp !Ligo.Tezos.now start_time) with
      | None -> (failwith "TODO: is this possible?" : fixedpoint) (* TODO *)
      | Some secs -> fixedpoint_pow (fixedpoint_sub fixedpoint_one auction_decay_rate) secs in
    kit_scale start_value decay
  | Ascending params ->
    let (leading_bid, _timestamp, _level) = params in
    let bid_improvement_factor = fixedpoint_of_ratio_floor bid_improvement_factor in
    kit_scale leading_bid.kit (fixedpoint_add fixedpoint_one bid_improvement_factor)

(** Check if an auction is complete. A descending auction declines
  * exponentially over time, so it is effectively never complete (George: I
  * guess when it reaches zero it is, but I'd expect someone to buy before
  * that?). If the auction is ascending, then every bid adds the longer of 20
  * minutes or 20 blocks to the time before the auction expires. *)
let is_liquidation_auction_complete
    (auction_state: liquidation_auction_state) : bid option =
  match auction_state with
  | Descending _ ->
    (None: bid option)
  | Ascending params ->
    let (b, t, h) = params in
    if Ligo.sub_timestamp_timestamp !Ligo.Tezos.now t
       > max_bid_interval_in_seconds
    && Ligo.gt_int_int
         (Ligo.sub_nat_nat !Ligo.Tezos.level h)
         (Ligo.int max_bid_interval_in_blocks)
    then Some b
    else (None: bid option)

let complete_liquidation_auction_if_possible
    (auctions: liquidation_auctions): liquidation_auctions =
  failwith "foo"

(** Place a bid in the current auction. Fail if the bid is too low (must be at
  * least as much as the liquidation_auction_current_auction_minimum_bid. *)
let liquidation_auction_place_bid (auction: current_liquidation_auction) (bid: bid) : (current_liquidation_auction * liquidation_auction_bid) =
  if bid.kit >= liquidation_auction_current_auction_minimum_bid auction
  then
    ( { auction with state = Ascending (bid, !Ligo.Tezos.now, !Ligo.Tezos.level); },
      { auction_id = auction.contents; bid = bid; }
    )
  else (Ligo.failwith error_BidTooLow : current_liquidation_auction * liquidation_auction_bid)

let liquidation_auction_get_current_auction (auctions: liquidation_auctions) : current_liquidation_auction =
  match auctions.current_auction with
  | None -> (Ligo.failwith error_NoOpenAuction : current_liquidation_auction)
  | Some curr -> curr

let is_leading_current_liquidation_auction
    (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid): bool =
  match auctions.current_auction with
  | Some auction -> (failwith "foo": bool)
  | None -> false

let completed_liquidation_auction_won_by
    (avl_storage: mem) (bid_details: liquidation_auction_bid): auction_outcome option =
  (failwith "foo": auction_outcome option)

(* If successful, it consumes the ticket. *)
let liquidation_auction_reclaim_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid) : kit =
  if is_leading_current_liquidation_auction auctions bid_details
  then (Ligo.failwith error_CannotReclaimLeadingBid : kit)
  else (failwith "foo": kit)

(* If successful, it consumes the ticket. *)
let[@inline] liquidation_auction_reclaim_winning_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid) : (Ligo.tez * liquidation_auctions) =
  (failwith "foo": Ligo.tez * liquidation_auctions)

let liquidation_auction_touch (auctions: liquidation_auctions) (price: fixedpoint) : liquidation_auctions =
  (start_liquidation_auction_if_possible price
     (complete_liquidation_auction_if_possible
        auctions))

(*
 * - Cancel auction
 *
 * TODO: how to see current leading bid? FA2?
 * TODO: return kit to losing bidders
 * TODO: when liquidation result was "close", what happens after the tez is sold? Might we find that we didn't need to close it after all?
 *)

let liquidation_auction_oldest_completed_liquidation_slice (auctions: liquidation_auctions) : leaf_ptr option =
  failwith "foo"

(* BEGIN_OCAML *)

let liquidation_auction_current_auction_tez (auctions: liquidation_auctions) : Ligo.tez option =
  failwith "foo"

(* Checks if some invariants of auctions structure holds. *)
let assert_liquidation_auction_invariants (auctions: liquidation_auctions) : unit =
  ()
(* END_OCAML *)
