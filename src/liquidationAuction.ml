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
open Avl
open Constants
open Common
open TokenTypes
open LiquidationAuctionTypes

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let liquidation_auction_send_to_auction (auctions: liquidation_auctions) (slice: liquidation_slice) : (liquidation_auctions * leaf_ptr) =
  if avl_height auctions.avl_storage auctions.queued_slices
     >= max_liquidation_queue_height then
    (failwith "LiquidationQueueTooLong": liquidation_auctions * leaf_ptr)
  else
    let (new_storage, ret) =
      avl_push_back auctions.avl_storage auctions.queued_slices slice in
    let new_state = { auctions with avl_storage = new_storage; } in
    (new_state, ret)

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
  let lkit =
    kit_of_ratio_ceil
      (mul_ratio
         (kit_to_ratio slice.min_kit_for_unwarranted)
         (make_ratio (tez_to_mutez ltez) (tez_to_mutez slice.tez))
      ) in
  (* right slice *)
  let rtez = Ligo.sub_tez_tez slice.tez amnt in
  let rkit =
    kit_of_ratio_ceil
      (mul_ratio
         (kit_to_ratio slice.min_kit_for_unwarranted)
         (make_ratio (tez_to_mutez rtez) (tez_to_mutez slice.tez))
      ) in
  ( { slice with tez = ltez; min_kit_for_unwarranted = lkit; },
    { slice with tez = rtez; min_kit_for_unwarranted = rkit; }
  )

let take_with_splitting (storage: mem) (queued_slices: avl_ptr) (split_threshold: Ligo.tez) =
  let (storage, new_auction) = avl_take storage queued_slices split_threshold (None: auction_outcome option) in
  let queued_amount = avl_tez storage new_auction in
  if queued_amount < split_threshold
  then
    (* split next thing *)
    let (storage, next) = avl_pop_front storage queued_slices in
    match next with
    | Some slice ->
      let (part1, part2) = split_liquidation_slice (Ligo.sub_tez_tez split_threshold queued_amount) slice in
      let (storage, _) = avl_push_front storage queued_slices part2 in
      let (storage, _) = avl_push_back storage new_auction part1 in
      (storage, new_auction)
    | None ->
      (storage, new_auction)
  else
    (storage, new_auction)

let start_liquidation_auction_if_possible
    (start_price: fixedpoint) (auctions: liquidation_auctions): liquidation_auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    let queued_amount = avl_tez auctions.avl_storage auctions.queued_slices in
    let split_threshold =
      max_tez
        max_lot_size
        (ratio_to_tez_floor
           (mul_ratio
              (ratio_of_tez queued_amount)
              (fixedpoint_to_ratio min_lot_auction_queue_fraction)
           )
        ) in
    let (storage, new_auction) =
      take_with_splitting
        auctions.avl_storage
        auctions.queued_slices
        split_threshold in
    let current_auction =
      if avl_is_empty storage new_auction
      then (None: current_liquidation_auction option)
      else
        let start_value =
          kit_scale
            kit_one
            (fixedpoint_mul
               (fixedpoint_of_ratio_floor (ratio_of_tez (avl_tez storage new_auction)))
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
    (auction: current_liquidation_auction) : bid option =
  match auction.state with
  | Descending _ ->
    (None: bid option)
  | Ascending params ->
    let (b, t, h) = params in
    if Ligo.sub_timestamp_timestamp !Ligo.Tezos.now t
       > max_bid_interval_in_seconds
    && Ligo.gt_int_int
         (Ligo.sub_nat_nat !Ligo.tezos_level h)
         (Ligo.int max_bid_interval_in_blocks)
    then Some b
    else (None: bid option)

let complete_liquidation_auction_if_possible
    (auctions: liquidation_auctions): liquidation_auctions =
  match auctions.current_auction with
  | None -> auctions
  | Some curr -> begin
      match is_liquidation_auction_complete curr with
      | None -> auctions
      | Some winning_bid ->
        let (storage, completed_auctions) = match auctions.completed_auctions with
          | None ->
            let outcome =
              { winning_bid = winning_bid;
                sold_tez=avl_tez auctions.avl_storage curr.contents;
                younger_auction=(None: liquidation_auction_id option);
                older_auction=(None: liquidation_auction_id option);
              } in
            let storage =
              avl_modify_root_data
                auctions.avl_storage
                curr.contents
                (fun (prev: auction_outcome option) ->
                   assert (Option.is_none prev);
                   Some outcome) in
            (storage, {youngest=curr.contents;oldest=curr.contents})
          | Some params ->
            let {youngest=youngest; oldest=oldest} = params in
            let outcome =
              { winning_bid = winning_bid;
                sold_tez=avl_tez auctions.avl_storage curr.contents;
                younger_auction=Some youngest;
                older_auction=(None: liquidation_auction_id option);
              } in
            let storage =
              avl_modify_root_data
                auctions.avl_storage
                curr.contents
                (fun (prev: auction_outcome option) ->
                   assert (Option.is_none prev);
                   Some outcome) in
            let storage =
              avl_modify_root_data
                storage
                youngest
                (fun (prev: auction_outcome option) ->
                   match prev with
                   | None -> (failwith "completed auction without outcome" : auction_outcome option)
                   | Some xs -> Some ({xs with younger_auction=Some curr.contents})
                ) in
            (storage, {youngest=curr.contents; oldest=oldest; }) in
        { auctions with
          avl_storage = storage;
          current_auction=(None: current_liquidation_auction option);
          completed_auctions=Some completed_auctions;
        }
    end

(** Place a bid in the current auction. Fail if the bid is too low (must be at
  * least as much as the liquidation_auction_current_auction_minimum_bid. *)
let liquidation_auction_place_bid (auction: current_liquidation_auction) (bid: bid) : (current_liquidation_auction * liquidation_auction_bid_details) =
  if bid.kit >= liquidation_auction_current_auction_minimum_bid auction
  then
    ( { auction with state = Ascending (bid, !Ligo.Tezos.now, !Ligo.tezos_level); },
      { auction_id = auction.contents; bid = bid; }
    )
  else (failwith "BidTooLow": current_liquidation_auction * liquidation_auction_bid_details)

let liquidation_auction_get_current_auction (auctions: liquidation_auctions) : current_liquidation_auction =
  match auctions.current_auction with
  | None -> (failwith "NoOpenAuction": current_liquidation_auction)
  | Some curr -> curr

let is_leading_current_liquidation_auction
    (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid_details): bool =
  match auctions.current_auction with
  | Some auction ->
    if ptr_of_avl_ptr auction.contents = ptr_of_avl_ptr bid_details.auction_id
    then
      (match auction.state with
       | Ascending params ->
         let (bid, _timestamp, _level) = params in
         bid_eq bid bid_details.bid
       | Descending _ -> false)
    else false
  | None -> false

let completed_liquidation_auction_won_by
    (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid_details): auction_outcome option =
  match avl_root_data auctions.avl_storage bid_details.auction_id with
  | Some outcome ->
    if bid_eq outcome.winning_bid bid_details.bid
    then Some outcome
    else (None: auction_outcome option)
  | None -> (None: auction_outcome option)

(* If successful, it consumes the ticket. *)
let liquidation_auction_reclaim_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid_details) : kit =
  if is_leading_current_liquidation_auction auctions bid_details
  then (failwith "CannotReclaimLeadingBid": kit)
  else
    match completed_liquidation_auction_won_by auctions bid_details with
    | Some _ -> (failwith "CannotReclaimWinningBid": kit)
    | None -> bid_details.bid.kit

(* Removes the auction from completed lots list, while preserving the auction itself. *)
let liquidation_auction_pop_completed_auction (auctions: liquidation_auctions) (tree: avl_ptr) : liquidation_auctions =
  let storage = auctions.avl_storage in

  let outcome = match avl_root_data storage tree with
    | None -> (failwith "auction is not completed" : auction_outcome)
    | Some r -> r in
  let completed_auctions = match auctions.completed_auctions with
    | None -> (failwith "invariant violation" : completed_liquidation_auctions)
    | Some r -> r in

  (* First, fixup the completed auctions if we're dropping the
   * youngest or the oldest lot. *)
  let completed_auctions =
    match outcome.younger_auction with
    | None -> begin
        match outcome.older_auction with
        | None ->
          assert (completed_auctions.youngest = tree);
          assert (completed_auctions.oldest = tree);
          (None: completed_liquidation_auctions option)
        | Some older ->
          assert (completed_auctions.youngest = tree);
          assert (completed_auctions.oldest <> tree);
          Some {completed_auctions with youngest = older }
      end
    | Some younger -> begin
        match outcome.older_auction with
        | None ->
          assert (completed_auctions.youngest <> tree);
          assert (completed_auctions.oldest = tree);
          Some {completed_auctions with oldest = younger }
        | Some _older ->
          assert (completed_auctions.youngest <> tree);
          assert (completed_auctions.oldest <> tree);
          Some completed_auctions
      end in

  (* Then, fixup the pointers within the list.*)
  let storage =
    match outcome.younger_auction with
    | None -> storage
    | Some younger ->
      avl_modify_root_data storage younger (fun (i: auction_outcome option) ->
          let i = match i with
            | None -> (failwith "invariant violation: completed auction does not have outcome": auction_outcome)
            | Some i -> i in
          assert (i.older_auction = Some tree);
          Some {i with older_auction=outcome.older_auction}) in
  let storage =
    match outcome.older_auction with
    | None -> storage
    | Some older ->
      avl_modify_root_data storage older (fun (i: auction_outcome option) ->
          let i = match i with
            | None -> (failwith "invariant violation: completed auction does not have outcome": auction_outcome)
            | Some i -> i in
          assert (i.younger_auction = Some tree);
          Some {i with younger_auction=outcome.younger_auction}) in

  let storage = avl_modify_root_data storage tree (fun (_: auction_outcome option) ->
      Some { outcome with
             younger_auction = (None: liquidation_auction_id option);
             older_auction = (None: liquidation_auction_id option)}) in

  { auctions with
    completed_auctions = completed_auctions;
    avl_storage = storage
  }

(* If successful, it consumes the ticket. *)
let[@inline] liquidation_auction_reclaim_winning_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid_details) : (Ligo.tez * liquidation_auctions) =
  match completed_liquidation_auction_won_by auctions bid_details with
  | Some outcome ->
    (* A winning bid can only be claimed when all the liquidation slices
     * for that lot is cleaned. *)
    if not (avl_is_empty auctions.avl_storage bid_details.auction_id)
    then (failwith "NotAllSlicesClaimed": Ligo.tez * liquidation_auctions)
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
            avl_delete_empty_tree auctions.avl_storage bid_details.auction_id } in
      (outcome.sold_tez, auctions)
    )
  | None -> (failwith "NotAWinningBid": Ligo.tez * liquidation_auctions)


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
  match auctions.completed_auctions with
  | None -> (None: leaf_ptr option)
  | Some completed_auctions -> begin
      match avl_peek_front auctions.avl_storage completed_auctions.youngest with
      | None -> (failwith "invariant violation: empty auction in completed_auctions" : leaf_ptr option)
      | Some p ->
        let (leaf_ptr, _) = p in
        Some leaf_ptr
    end

(* BEGIN_OCAML *)

let liquidation_auction_current_auction_tez (auctions: liquidation_auctions) : Ligo.tez option =
  match auctions.current_auction with
  | None -> (None: Ligo.tez option)
  | Some auction -> Some (avl_tez auctions.avl_storage auction.contents)

(* Checks if some invariants of auctions structure holds. *)
let liquidation_auction_assert_invariants (auctions: liquidation_auctions) : unit =

  (* All AVL trees in the storage are valid. *)
  let mem = auctions.avl_storage in
  let roots = Ligo.Big_map.bindings mem.mem
              |> List.filter (fun (_, n) -> match n with | LiquidationAuctionPrimitiveTypes.Root _ -> true; | _ -> false)
              |> List.map (fun (p, _) -> AVLPtr p) in
  List.iter (avl_assert_invariants mem) roots;

  (* There are no dangling pointers in the storage. *)
  avl_assert_dangling_pointers mem roots;

  (* Completed_auctions linked list is correct. *)
  auctions.completed_auctions
  |> Option.iter (fun completed_auctions ->
      let rec go (curr: avl_ptr) (prev: avl_ptr option) =
        let curr_data = Option.get (avl_root_data mem curr) in
        assert (curr_data.younger_auction = prev);
        match curr_data.older_auction with
        | Some next -> go next (Some curr)
        | None ->  assert (curr = completed_auctions.oldest) in
      go (completed_auctions.youngest) None
    );

  (* TODO: Check if all dangling auctions are empty. *)

  ()
(* END_OCAML *)
