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

   TODO: Figure out how a `bid` should be represented as.

   Every bid here is either ends as a 'leading_bid' in 'current_auction', or in
   'unclaimed_bids' set when overbid.

   At any time, the owner of a bid can claim an bid if it is in 'unclaimed_bid'
   set. If it is, it is removed and the amount of kit is given back.

   When an auction expires, the current auction is both moved to
   'completed_auctions' FIFO queue, and to another map from tree_ptr to
   auction_outcome called `outcomes`.

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

type Error.error +=
  | NoOpenAuction
  | BidTooLow
  | CannotReclaimLeadingBid
  | NotAWinningBid

(* Stub types *)
type burrow_id = unit
type 'a ticket = 'a

type liquidation_slice = {
  burrow: Ptr.t;
  tez: Tez.t;
  min_kit_for_unwarranted: Kit.t;
  older: leaf_ptr option;
  younger: leaf_ptr option;
}

type bid = { address: Address.t; kit: Kit.t }
[@@deriving show]

type auction_id = avl_ptr
type bid_details = { auction_id: auction_id; bid: bid; }
type bid_ticket = bid_details Ticket.ticket

type auction_state =
  | Descending of Kit.t * Timestamp.t
  | Ascending of bid * Timestamp.t * int
[@@deriving show]

type current_auction = {
  contents: avl_ptr;
  state: auction_state;
}

type auction_outcome = {
  sold_tez: Tez.t;
  winning_bid: bid;
}

module AvlPtrMap =
  Map.Make(struct
    type t = avl_ptr
    let compare (AVLPtr a) (AVLPtr b) = Ptr.compare a b end)

type auctions = {
  storage: liquidation_slice mem;

  queued_slices: avl_ptr;
  current_auction: current_auction option;
  completed_auctions: auction_outcome AvlPtrMap.t;
}

let empty : auctions =
  let storage = BigMap.BigMap.empty in
  let (storage, queued_slices) = Avl.mk_empty storage in
  { storage = storage;
    queued_slices = queued_slices;
    current_auction = None;
    completed_auctions = AvlPtrMap.empty;
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

(** Split a liquidation slice into two. We also have to split the
  * min_kit_for_unwarranted so that we can evaluate the two auctions separately
  * (and see if the liquidation was warranted, retroactively). Perhaps a bit
  * harshly, for both slices we round up. NOTE: Alternatively, we can calculate
  * min_kit_for_unwarranted_1 and then calculate min_kit_for_unwarranted_2 =
  * min_kit_for_unwarranted - min_kit_for_unwarranted_1. *)
let split (amount: Tez.t) (slice: liquidation_slice) : (liquidation_slice * liquidation_slice) =
  assert (amount > Tez.zero);
  assert (amount < slice.tez);
  (* left slice *)
  let ltez = amount in
  let lkit = Kit.of_q_ceil Q.(
    Kit.to_q slice.min_kit_for_unwarranted * Tez.to_q ltez / Tez.to_q slice.tez
  ) in
  (* right slice *)
  let rtez = Tez.(slice.tez - amount) in
  let rkit = Kit.of_q_ceil Q.(
    Kit.to_q slice.min_kit_for_unwarranted * Tez.to_q rtez / Tez.to_q slice.tez
  ) in
  ( { slice with tez = ltez; min_kit_for_unwarranted = lkit; },
    { slice with tez = rtez; min_kit_for_unwarranted = rkit; }
  )

let take_with_splitting storage queued_slices split_threshold =
  let (storage, new_auction) = take storage queued_slices split_threshold in
  let queued_amount = Avl.avl_tez storage new_auction in
  if queued_amount < split_threshold
  then
    (* split next thing *)
    let (storage, next) = Avl.pop_front storage queued_slices in
    match next with
    | Some slice ->
      let (part1, part2) = split Tez.(split_threshold - queued_amount) slice in
      let (storage, _) = Avl.push_front storage queued_slices part2 part2.tez in
      let (storage, _) = Avl.push_back storage new_auction part1 part1.tez in
      (storage, new_auction)
    | None ->
      (storage, new_auction)
  else
    (storage, new_auction)

let start_auction_if_possible
    (now: Timestamp.t) (start_price: FixedPoint.t) (auctions: auctions): auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    let queued_amount = Avl.avl_tez auctions.storage auctions.queued_slices in
    let split_threshold =
      max
        Constants.max_lot_size
        (Tez.scale queued_amount Constants.min_lot_auction_queue_fraction) in
    let (storage, new_auction) =
      take_with_splitting
        auctions.storage
        auctions.queued_slices
        split_threshold in
    let current_auction =
      if is_empty storage new_auction
      then None
      else
        let start_value =
          Kit.scale
            Kit.one
            FixedPoint.(
              Tez.to_fp (Avl.avl_tez storage new_auction)
              * start_price)  in
        Some
          { contents = new_auction;
            state = Descending (start_value, now); } in
    { auctions with
      storage = storage;
      current_auction = current_auction;
    }

(** Compute the current threshold for a bid to be accepted. For a descending
  * auction this amounts to the reserve price (which is exponentially
  * dropping). For a descending auction we should improve upon the last bid
  * a fixed factor. *)
let current_auction_minimum_bid (now: Timestamp.t) (auction: current_auction) : Kit.t =
  match auction.state with
  | Descending (start_value, start_time) ->
    let decay =
      FixedPoint.pow
        Constants.auction_decay_rate
        (Timestamp.seconds_elapsed ~start:start_time ~finish:now) in
    Kit.scale start_value decay
  | Ascending (leading_bid, _timestamp, _height) ->
    Kit.scale leading_bid.kit FixedPoint.(one + Constants.bid_improvement_factor)

(** Check if an auction is complete. A descending auction declines
  * exponentially over time, so it is effectively never complete (George: I
  * guess when it reaches zero it is, but I'd expect someone to buy before
  * that?). If the auction is ascending, then every bid adds the longer of 20
  * minutes or 20 blocks to the time before the auction expires. *)
let is_auction_complete (now: Timestamp.t) (height: int) (auction: current_auction) : bool =
  match auction.state with
  | Descending _ -> if current_auction_minimum_bid now auction = Kit.zero then true else false
  | Ascending (_, t, h) ->
    Timestamp.seconds_elapsed ~start:t ~finish:now > Constants.max_bid_interval_in_seconds
    && height - h > Constants.max_bid_interval_in_blocks

let complete_auction_if_possible
    (now: Timestamp.t) (height: int) (auctions: auctions): auctions =
  match auctions.current_auction with
  | None ->
    auctions
  | Some curr when not (is_auction_complete now height curr) ->
    auctions
  | _ -> failwith "not_implemented"

(** Place a bid in the current auction. Fail if the bid is too low (must be at
  * least as much as the current_auction_minimum_bid. *)
let place_bid (now: Timestamp.t) (height: int) (auction: current_auction) (bid: bid)
  : (current_auction * bid_ticket ticket, Error.error) result =
  if bid.kit >= current_auction_minimum_bid now auction
  then
    Ok (
      { auction with state = Ascending (bid, now, height); },
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
     | Ascending (bid, _timestamp, _height) -> bid = bid_details.bid
     | Descending _ -> false)
  | _ -> false

let completed_auction_won_by
    (auctions: auctions) (bid_details: bid_details): auction_outcome option =
  match AvlPtrMap.find_opt bid_details.auction_id auctions.completed_auctions with
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

(* TODO Winners can only reclaim when all the liquidation slices of an
 * auction is propagated back to the burrows.
 *)
let reclaim_winning_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Tez.t, Error.error) result =
  let bid_details = Ticket.read bid_ticket in
  match completed_auction_won_by auctions bid_details with
  | Some outcome -> Ok outcome.sold_tez
  | _ -> Error NotAWinningBid


let touch (auctions: auctions) (now: Timestamp.t) (height: int) (price: FixedPoint.t) : auctions =
  auctions
  |> complete_auction_if_possible now height
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
