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

type Error.error +=
  | NoOpenAuction
  | BidTooLow
  | CannotReclaimLeadingBid
  | NotAWinningBid
  | NotAllSlicesClaimed

type liquidation_slice = {
  burrow: Ptr.t;
  tez: Tez.t;
  min_kit_for_unwarranted: Kit.t;
  older: Avl.leaf_ptr option;
  younger: Avl.leaf_ptr option;
}
[@@deriving show]

type auction_id = Avl.avl_ptr
type bid_details = { auction_id: auction_id; bid: Bid.t; }
type bid_ticket = bid_details Ticket.t

let create_bid_ticket (tezos: Tezos.t) (bid_details: bid_details) =
  Ticket.create ~issuer:tezos.self ~amount:1 ~content:bid_details

type auction_state =
  | Descending of Kit.t * Timestamp.t
  | Ascending of Bid.t * Timestamp.t * Level.t
[@@deriving show]

type current_auction = {
  contents: Avl.avl_ptr;
  state: auction_state;
}
[@@deriving show]

type auction_outcome = {
  sold_tez: Tez.t;
  winning_bid: Bid.t;
  younger_auction: Avl.avl_ptr option;
  older_auction: Avl.avl_ptr option;
}
[@@deriving show]

type completed_auctions =
  { youngest: Avl.avl_ptr
  ; oldest: Avl.avl_ptr
  }
[@@deriving show]

module AvlPtrMap =
  Map.Make(struct
    type t = Avl.avl_ptr
    let compare (Avl.AVLPtr a) (Avl.AVLPtr b) = Ptr.compare a b end)

type auctions = {
  avl_storage: (liquidation_slice, auction_outcome option) Avl.mem;

  queued_slices: Avl.avl_ptr;
  current_auction: current_auction option;
  completed_auctions: completed_auctions option;
}

let empty : auctions =
  let avl_storage = BigMap.BigMap.empty in
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
  : auctions * Avl.leaf_ptr =
  let (new_storage, ret) =
    Avl.push_back auctions.avl_storage auctions.queued_slices slice slice.tez in
  let new_state = { auctions with avl_storage = new_storage; } in
  (new_state, ret)

let cancel_liquidation
    (auctions: auctions)
    (slice: Avl.leaf_ptr)
  : auctions option =
  if Avl.find_root auctions.avl_storage slice = auctions.queued_slices
  then
    (* if the slice belongs to queued_slices tree, we can cancel it *)
    let (new_storage, _) = Avl.del auctions.avl_storage slice in
    Some { auctions with avl_storage = new_storage; }
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
  let (storage, new_auction) = Avl.take storage queued_slices split_threshold None in
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
    (tezos: Tezos.t) (start_price: FixedPoint.t) (auctions: auctions): auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    let queued_amount = Avl.avl_tez auctions.avl_storage auctions.queued_slices in
    let split_threshold =
      max
        Constants.max_lot_size
        (Tez.scale queued_amount Constants.min_lot_auction_queue_fraction) in
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
            FixedPoint.(
              Tez.to_fp (Avl.avl_tez storage new_auction)
              * start_price)  in
        Some
          { contents = new_auction;
            state = Descending (start_value, tezos.now); } in
    { auctions with
      avl_storage = storage;
      current_auction = current_auction;
    }

(** Compute the current threshold for a bid to be accepted. For a descending
  * auction this amounts to the reserve price (which is exponentially
  * dropping). For a descending auction we should improve upon the last bid
  * a fixed factor. *)
let current_auction_minimum_bid (tezos: Tezos.t) (auction: current_auction) : Kit.t =
  match auction.state with
  | Descending (start_value, start_time) ->
    let auction_decay_rate = FixedPoint.of_q_ceil Constants.auction_decay_rate in (* FLOOR-or-CEIL *)
    let decay =
      FixedPoint.pow
        FixedPoint.(one - auction_decay_rate)
        (Timestamp.seconds_elapsed ~start:start_time ~finish:tezos.now) in
    Kit.scale start_value decay
  | Ascending (leading_bid, _timestamp, _level) ->
    let bid_improvement_factor = FixedPoint.of_q_floor Constants.bid_improvement_factor in (* FLOOR-or-CEIL *)
    Kit.scale leading_bid.kit FixedPoint.(one + bid_improvement_factor)

(** Check if an auction is complete. A descending auction declines
  * exponentially over time, so it is effectively never complete (George: I
  * guess when it reaches zero it is, but I'd expect someone to buy before
  * that?). If the auction is ascending, then every bid adds the longer of 20
  * minutes or 20 blocks to the time before the auction expires. *)
let is_auction_complete
    (tezos: Tezos.t)
    (auction: current_auction) : Bid.t option =
  match auction.state with
  | Descending _ ->
    None
  | Ascending (b, t, h) ->
    if Timestamp.seconds_elapsed ~start:t ~finish:tezos.now
       > Constants.max_bid_interval_in_seconds
    && Level.blocks_elapsed ~start:h ~finish:tezos.level
       > Constants.max_bid_interval_in_blocks
    then Some b
    else None

let complete_auction_if_possible
    (tezos: Tezos.t) (auctions: auctions): auctions =
  match auctions.current_auction with
  | None -> auctions
  | Some curr ->
    match is_auction_complete tezos curr with
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
                  | None -> failwith "completed auction without outcome"
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
let place_bid (tezos: Tezos.t) (auction: current_auction) (bid: Bid.t)
  : (current_auction * bid_ticket, Error.error) result =
  if bid.kit >= current_auction_minimum_bid tezos auction
  then
    Ok (
      { auction with state = Ascending (bid, tezos.now, tezos.level); },
      create_bid_ticket tezos { auction_id = auction.contents; bid = bid; }
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

let reclaim_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Kit.t, Error.error) result =
  let _, _, bid_details = Ticket.read bid_ticket in
  if is_leading_current_auction auctions bid_details
  || Option.is_some (completed_auction_won_by auctions bid_details)
  then Error CannotReclaimLeadingBid
  else
    (* TODO: punch tickets *)
    Ok bid_details.bid.kit

(* Removes the auction from completed lots list, while preserving the auction itself. *)
let pop_completed_auction (auctions: auctions) (tree: Avl.avl_ptr) : auctions =
  let storage = auctions.avl_storage in

  let outcome = match Avl.root_data storage tree with
  | None -> failwith "auction is not completed"
  | Some r -> r in
  let completed_auctions = match auctions.completed_auctions with
  | None -> failwith "invariant violation"
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

let reclaim_winning_bid
    (auctions: auctions)
    (bid_ticket: bid_ticket)
  : (Tez.t * auctions, Error.error) result =
  let _, _, bid_details = Ticket.read bid_ticket in
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


let touch (auctions: auctions) (tezos: Tezos.t) (price: FixedPoint.t) : auctions =
  auctions
  |> complete_auction_if_possible tezos
  |> start_auction_if_possible tezos price

let current_auction_tez (auctions: auctions) : Tez.t option =
  Option.map
    (fun auction -> Avl.avl_tez auctions.avl_storage auction.contents)
    auctions.current_auction

(*
 * - Cancel auction
 *
 * TODO: how to see current leading bid? FA2?
 * TODO: return kit to losing bidders
 * TODO: when liquidation result was "close", what happens after the tez is sold? Might we find that we didn't need to close it after all?
 *)

let oldest_completed_liquidation_slice (auctions: auctions) : Avl.leaf_ptr option =
  match auctions.completed_auctions with
  | None -> None
  | Some completed_auctions ->
  match Avl.peek_front auctions.avl_storage completed_auctions.youngest with
  | None -> failwith "invariant violation: empty auction in completed_auctions"
  | Some (leaf_ptr, _) -> Some leaf_ptr

(* Test utilities *)

(* Checks if some invariants of auctions structure holds. *)
let assert_invariants (auctions: auctions) : unit =

  (* All AVL trees in the storage are valid. *)
  let mem = auctions.avl_storage in
  let roots = BigMap.BigMap.bindings mem
               |> List.filter (fun (_, n) -> match n with | Avl.Root _ -> true; | _ -> false)
               |> List.map (fun (p, _) -> Avl.AVLPtr p) in
  List.iter (Avl.assert_invariants mem) roots;

  (* There are no dangling pointers in the storage. *)
  Avl.assert_dangling_pointers mem roots;

  (* Completed_auctions linked list is correct. *)
  auctions.completed_auctions
    |> Option.iter (fun completed_auctions ->
      let rec go (curr: Avl.avl_ptr) (prev: Avl.avl_ptr option) =
        let curr_data = Option.get (Avl.root_data mem curr) in
        assert (curr_data.younger_auction = prev);
        match curr_data.older_auction with
        | Some next -> go next (Some curr)
        | None ->  assert (curr = completed_auctions.oldest) in
      go (completed_auctions.youngest) None
    );

  (* TODO: Check if per-burrow linked lists are correct. *)
  (* TODO: Check if all dangling auctions are empty. *)

  ()
