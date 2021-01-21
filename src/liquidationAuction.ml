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

open LiquidationAuctionTypes

type Error.error +=
  | NoOpenAuction
  | BidTooLow
  | CannotReclaimLeadingBid
  | NotAWinningBid
  | NotAllSlicesClaimed
  | LiquidationQueueTooLong
  | InvalidLiquidationAuctionTicket

type auction_id = avl_ptr
type bid_details = { auction_id: auction_id; bid: bid; }
type bid_ticket = bid_details Ligo.ticket

let issue_bid_ticket (bid_details: bid_details) =
  Ligo.Tezos.create_ticket bid_details (Ligo.nat_from_literal 1)

(** Check whether a liquidation auction bid ticket is valid. An auction bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. TODO: (c) is not
  * implemented yet. Perhaps it can be avoided, if all checker-issued tickets
  * end up having contents clearly distinguished by type. *)
let is_bid_ticket_valid
    ~(bid_ticket: bid_ticket)
  : (bid_ticket, Error.error) result =
  let (issuer, _bid_details, amount), same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid = issuer = Ligo.Tezos.self && amount = Ligo.nat_from_literal 1 in
  if is_valid then Ok same_ticket else Error InvalidLiquidationAuctionTicket

let with_valid_bid_ticket
    ~(bid_ticket: bid_ticket)
    (f: bid_ticket -> ('a, Error.error) result)
  : ('a, Error.error) result =
  match is_bid_ticket_valid ~bid_ticket with
  | Error err -> Error err
  | Ok ticket -> f ticket

type auction_state =
  | Descending of Kit.t * Ligo.timestamp
  | Ascending of bid * Ligo.timestamp * Ligo.nat
[@@deriving show]

type current_auction = {
  contents: avl_ptr;
  state: auction_state;
}
[@@deriving show]

type completed_auctions =
  { youngest: avl_ptr
  ; oldest: avl_ptr
  }
[@@deriving show]

module AvlPtrMap =
  Map.Make(struct
    type t = avl_ptr
    let compare (AVLPtr a) (AVLPtr b) = Ptr.compare a b end)

type auctions = {
  avl_storage: Mem.mem;

  queued_slices: avl_ptr;
  current_auction: current_auction option;
  completed_auctions: completed_auctions option;
}

let empty : auctions =
  let avl_storage = Mem.mem_empty in
  let (avl_storage, queued_slices) = Avl.mk_empty avl_storage None in
  { avl_storage = avl_storage;
    queued_slices = queued_slices;
    current_auction = None;
    completed_auctions = None;
  }

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let send_to_auction
    (auctions: auctions)
    (slice: liquidation_slice)
  : (auctions * leaf_ptr, Error.error) result =
  if Avl.avl_height auctions.avl_storage auctions.queued_slices
     >= Constants.max_liquidation_queue_height then
    Error LiquidationQueueTooLong
  else
    let (new_storage, ret) =
      Avl.push_back auctions.avl_storage auctions.queued_slices slice in
    let new_state = { auctions with avl_storage = new_storage; } in
    Ok (new_state, ret)

(** Split a liquidation slice into two. We also have to split the
  * min_kit_for_unwarranted so that we can evaluate the two auctions separately
  * (and see if the liquidation was warranted, retroactively). Perhaps a bit
  * harshly, for both slices we round up. NOTE: Alternatively, we can calculate
  * min_kit_for_unwarranted_1 and then calculate min_kit_for_unwarranted_2 =
  * min_kit_for_unwarranted - min_kit_for_unwarranted_1. *)
let split (amount: Ligo.tez) (slice: liquidation_slice) : (liquidation_slice * liquidation_slice) =
  assert (amount > Ligo.tez_from_mutez_literal 0);
  assert (amount < slice.tez);
  (* left slice *)
  let ltez = amount in
  let lkit =
    Kit.of_ratio_ceil
      (Ratio.mul
         (Kit.to_ratio slice.min_kit_for_unwarranted)
         (Ratio.make (Common.tez_to_mutez ltez) (Common.tez_to_mutez slice.tez))
      ) in
  (* right slice *)
  let rtez = Ligo.sub_tez_tez slice.tez amount in
  let rkit =
    Kit.of_ratio_ceil
      (Ratio.mul
         (Kit.to_ratio slice.min_kit_for_unwarranted)
         (Ratio.make (Common.tez_to_mutez rtez) (Common.tez_to_mutez slice.tez))
      ) in
  ( { slice with tez = ltez; min_kit_for_unwarranted = lkit; },
    { slice with tez = rtez; min_kit_for_unwarranted = rkit; }
  )

let take_with_splitting storage queued_slices split_threshold =
  let (storage, new_auction) = Avl.take storage queued_slices split_threshold None in
  let queued_amount = Avl.avl_tez storage new_auction in
  if queued_amount < split_threshold
  then
    (* split next thing *)
    let (storage, next) = Avl.pop_front storage queued_slices in
    match next with
    | Some slice ->
      let (part1, part2) = split (Ligo.sub_tez_tez split_threshold queued_amount) slice in
      let (storage, _) = Avl.push_front storage queued_slices part2 in
      let (storage, _) = Avl.push_back storage new_auction part1 in
      (storage, new_auction)
    | None ->
      (storage, new_auction)
  else
    (storage, new_auction)

let start_auction_if_possible
    (start_price: FixedPoint.t) (auctions: auctions): auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    let queued_amount = Avl.avl_tez auctions.avl_storage auctions.queued_slices in
    let split_threshold =
      Common.tez_max
        Constants.max_lot_size
        (Ratio.to_tez_floor
           (Ratio.mul
              (Ratio.of_tez queued_amount)
              (FixedPoint.to_ratio Constants.min_lot_auction_queue_fraction)
           )
        ) in
    let (storage, new_auction) =
      take_with_splitting
        auctions.avl_storage
        auctions.queued_slices
        split_threshold in
    let current_auction =
      if Avl.is_empty storage new_auction
      then None
      else
        let start_value =
          Kit.scale
            Kit.one
            (FixedPoint.mul
               (FixedPoint.of_ratio_floor (Ratio.of_tez (Avl.avl_tez storage new_auction)))
               start_price
            ) in
        Some
          { contents = new_auction;
            state = Descending (start_value, !Ligo.Tezos.now); } in
    { auctions with
      avl_storage = storage;
      current_auction = current_auction;
    }

(** Compute the current threshold for a bid to be accepted. For a descending
  * auction this amounts to the reserve price (which is exponentially
  * dropping). For a descending auction we should improve upon the last bid
  * a fixed factor. *)
let current_auction_minimum_bid (auction: current_auction) : Kit.t =
  match auction.state with
  | Descending (start_value, start_time) ->
    let auction_decay_rate = FixedPoint.of_ratio_ceil Constants.auction_decay_rate in
    let decay =
      match Ligo.is_nat (Ligo.sub_timestamp_timestamp !Ligo.Tezos.now start_time) with
      | None -> (failwith "TODO: is this possible?" : FixedPoint.t) (* TODO *)
      | Some secs -> FixedPoint.pow (FixedPoint.sub FixedPoint.one auction_decay_rate) secs in
    Kit.scale start_value decay
  | Ascending (leading_bid, _timestamp, _level) ->
    let bid_improvement_factor = FixedPoint.of_ratio_floor Constants.bid_improvement_factor in
    Kit.scale leading_bid.kit (FixedPoint.add FixedPoint.one bid_improvement_factor)

(** Check if an auction is complete. A descending auction declines
  * exponentially over time, so it is effectively never complete (George: I
  * guess when it reaches zero it is, but I'd expect someone to buy before
  * that?). If the auction is ascending, then every bid adds the longer of 20
  * minutes or 20 blocks to the time before the auction expires. *)
let is_auction_complete
    (auction: current_auction) : bid option =
  match auction.state with
  | Descending _ ->
    None
  | Ascending (b, t, h) ->
    if Ligo.sub_timestamp_timestamp !Ligo.Tezos.now t
       > Constants.max_bid_interval_in_seconds
    && Ligo.gt_int_int
         (Ligo.sub_nat_nat !Ligo.Tezos.level h)
         (Ligo.int Constants.max_bid_interval_in_blocks)
    then Some b
    else None

let complete_auction_if_possible
    (auctions: auctions): auctions =
  match auctions.current_auction with
  | None -> auctions
  | Some curr ->
    match is_auction_complete curr with
    | None -> auctions
    | Some winning_bid ->
      let (storage, completed_auctions) = match auctions.completed_auctions with
        | None ->
          let outcome =
            { winning_bid;
              sold_tez=Avl.avl_tez auctions.avl_storage curr.contents;
              younger_auction=None;
              older_auction=None;
            } in
          let storage =
            Avl.modify_root_data
              auctions.avl_storage
              curr.contents
              (fun prev -> assert (Option.is_none prev); Some outcome) in
          (storage, {youngest=curr.contents;oldest=curr.contents})
        | Some {youngest; oldest} ->
          let outcome =
            { winning_bid;
              sold_tez=Avl.avl_tez auctions.avl_storage curr.contents;
              younger_auction=Some youngest;
              older_auction=None;
            } in
          let storage =
            Avl.modify_root_data
              auctions.avl_storage
              curr.contents
              (fun prev -> assert (Option.is_none prev); Some outcome) in
          let storage =
            Avl.modify_root_data
              storage
              youngest
              (fun prev ->
                 match prev with
                 | None -> (failwith "completed auction without outcome" : auction_outcome option)
                 | Some xs -> Some ({xs with younger_auction=Some curr.contents})
              ) in
          (storage, {youngest=curr.contents; oldest}) in
      { auctions with
        avl_storage = storage;
        current_auction=None;
        completed_auctions=Some completed_auctions;
      }

(** Place a bid in the current auction. Fail if the bid is too low (must be at
  * least as much as the current_auction_minimum_bid. *)
let place_bid (auction: current_auction) (bid: bid)
  : (current_auction * bid_ticket, Error.error) result =
  if bid.kit >= current_auction_minimum_bid auction
  then
    Ok (
      { auction with state = Ascending (bid, !Ligo.Tezos.now, !Ligo.Tezos.level); },
      issue_bid_ticket { auction_id = auction.contents; bid = bid; }
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
     | Ascending (bid, _timestamp, _level) -> bid = bid_details.bid
     | Descending _ -> false)
  | _ -> false

let completed_auction_won_by
    (auctions: auctions) (bid_details: bid_details): auction_outcome option =
  match Avl.root_data auctions.avl_storage bid_details.auction_id with
  | Some outcome when outcome.winning_bid = bid_details.bid -> Some outcome
  | _ -> None

(* If successful, it consumes the ticket. *)
let reclaim_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Kit.t, Error.error) result =
  with_valid_bid_ticket ~bid_ticket @@ fun bid_ticket ->
  let (_, bid_details, _), _ = Ligo.Tezos.read_ticket bid_ticket in
  if is_leading_current_auction auctions bid_details
  || Option.is_some (completed_auction_won_by auctions bid_details)
  then Error CannotReclaimLeadingBid
  else
    Ok bid_details.bid.kit

(* Removes the auction from completed lots list, while preserving the auction itself. *)
let pop_completed_auction (auctions: auctions) (tree: avl_ptr) : auctions =
  let storage = auctions.avl_storage in

  let outcome = match Avl.root_data storage tree with
    | None -> (failwith "auction is not completed" : auction_outcome)
    | Some r -> r in
  let completed_auctions = match auctions.completed_auctions with
    | None -> (failwith "invariant violation" : completed_auctions)
    | Some r -> r in

  (* First, fixup the completed auctions if we're dropping the
   * youngest or the oldest lot. *)
  let completed_auctions =
    match (outcome.younger_auction, outcome.older_auction) with
    | (None, None) ->
      assert (completed_auctions.youngest = tree);
      assert (completed_auctions.oldest = tree);
      None
    | (None, Some older) ->
      assert (completed_auctions.youngest = tree);
      assert (completed_auctions.oldest <> tree);
      Some {completed_auctions with youngest = older }
    | (Some younger, None) ->
      assert (completed_auctions.youngest <> tree);
      assert (completed_auctions.oldest = tree);
      Some {completed_auctions with oldest = younger }
    | (Some _, Some _) ->
      Some completed_auctions in

  (* Then, fixup the pointers within the list.*)
  let storage =
    match outcome.younger_auction with
    | None -> storage
    | Some younger ->
      Avl.modify_root_data storage younger @@
      fun i ->
      let i = Option.get i in
      assert (i.older_auction = Some tree);
      Some {i with older_auction=outcome.older_auction} in
  let storage =
    match outcome.older_auction with
    | None -> storage
    | Some older ->
      Avl.modify_root_data storage older @@
      fun i ->
      let i = Option.get i in
      assert (i.younger_auction = Some tree);
      Some {i with younger_auction=outcome.younger_auction} in

  let storage = Avl.modify_root_data storage tree @@ fun _ ->
    Some { outcome with younger_auction = None; older_auction = None } in
  { auctions with
    completed_auctions;
    avl_storage = storage
  }

(* If successful, it consumes the ticket. *)
let reclaim_winning_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Ligo.tez * auctions, Error.error) result =
  with_valid_bid_ticket ~bid_ticket @@ fun bid_ticket ->
  let (_, bid_details, _), _ = Ligo.Tezos.read_ticket bid_ticket in
  match completed_auction_won_by auctions bid_details with
  | Some outcome ->
    (* A winning bid can only be claimed when all the liquidation slices
     * for that lot is cleaned. *)
    if not (Avl.is_empty auctions.avl_storage bid_details.auction_id)
    then Error NotAllSlicesClaimed
    else (
      (* When the winner reclaims their bid, we finally remove
         every reference to the auction. This is just to
         save storage, what's forbidding double-claiming
         is the ticket mechanism, not this.
      *)
      assert (outcome.younger_auction = None);
      assert (outcome.older_auction = None);
      let auctions =
        { auctions with
          avl_storage =
            Avl.delete_tree auctions.avl_storage bid_details.auction_id } in
      Ok (outcome.sold_tez, auctions)
    )
  | None -> Error NotAWinningBid


let touch (auctions: auctions) (price: FixedPoint.t) : auctions =
  auctions
  |> complete_auction_if_possible
  |> start_auction_if_possible price

let current_auction_tez (auctions: auctions) : Ligo.tez option =
  match auctions.current_auction with
  | None -> None
  | Some auction -> Some (Avl.avl_tez auctions.avl_storage auction.contents)

(*
 * - Cancel auction
 *
 * TODO: how to see current leading bid? FA2?
 * TODO: return kit to losing bidders
 * TODO: when liquidation result was "close", what happens after the tez is sold? Might we find that we didn't need to close it after all?
 *)

let oldest_completed_liquidation_slice (auctions: auctions) : leaf_ptr option =
  match auctions.completed_auctions with
  | None -> None
  | Some completed_auctions ->
    match Avl.peek_front auctions.avl_storage completed_auctions.youngest with
    | None -> (failwith "invariant violation: empty auction in completed_auctions" : leaf_ptr option)
    | Some (leaf_ptr, _) -> Some leaf_ptr

(* Test utilities *)

(* Checks if some invariants of auctions structure holds. *)
let assert_invariants (auctions: auctions) : unit =

  (* All AVL trees in the storage are valid. *)
  let mem = auctions.avl_storage in
  let roots = Ligo.Big_map.bindings mem.mem
              |> List.filter (fun (_, n) -> match n with | LiquidationAuctionTypes.Root _ -> true; | _ -> false)
              |> List.map (fun (p, _) -> AVLPtr p) in
  List.iter (Avl.assert_invariants mem) roots;

  (* There are no dangling pointers in the storage. *)
  Avl.assert_dangling_pointers mem roots;

  (* Completed_auctions linked list is correct. *)
  auctions.completed_auctions
  |> Option.iter (fun completed_auctions ->
      let rec go (curr: avl_ptr) (prev: avl_ptr option) =
        let curr_data = Option.get (Avl.root_data mem curr) in
        assert (curr_data.younger_auction = prev);
        match curr_data.older_auction with
        | Some next -> go next (Some curr)
        | None ->  assert (curr = completed_auctions.oldest) in
      go (completed_auctions.youngest) None
    );

  (* TODO: Check if all dangling auctions are empty. *)

  ()
