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
open Tickets
open LiquidationAuctionTypes
open Error

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let liquidation_auction_send_to_auction
    (auctions: liquidation_auctions) (contents: liquidation_slice_contents)
  : (liquidation_auctions * leaf_ptr) =
  if avl_height auctions.avl_storage auctions.queued_slices
     >= max_liquidation_queue_height then
    (Ligo.failwith error_LiquidationQueueTooLong : liquidation_auctions * leaf_ptr)
  else
    let old_burrow_slices =  Ligo.Big_map.find_opt contents.burrow auctions.burrow_slices in

    let slice = {
      contents = contents;
      older = (
        match old_burrow_slices with
        | None -> (None : leaf_ptr option)
        | Some i -> Some i.youngest_slice
      );
      younger = (None: leaf_ptr option);
    } in

    let (new_storage, ret) =
      avl_push auctions.avl_storage auctions.queued_slices slice Left in

    (* Fixup the previous youngest pointer since the newly added slice
     * is even younger.
    *)
    let new_storage, new_burrow_slices = (
      match old_burrow_slices with
      | None -> (new_storage, { oldest_slice = ret; youngest_slice = ret; })
      | Some old_slices ->
        ( mem_update
            new_storage
            (ptr_of_leaf_ptr old_slices.youngest_slice)
            (fun (older: node) ->
               let older = node_leaf older in
               Leaf { older with value = { older.value with younger = Some ret; }; }
            )
        , { old_slices with youngest_slice = ret }
        )
    ) in

    let new_state =
      { auctions with
        avl_storage = new_storage;
        burrow_slices =
          Ligo.Big_map.add
            contents.burrow
            new_burrow_slices
            auctions.burrow_slices;
      } in
    (new_state, ret)

(** Split a liquidation slice into two. The first of the two slices is the
  * "older" of the two (i.e. it is the one to be included in the auction we are
  * starting).
  *
  * We also have to split the min_kit_for_unwarranted so that we can evaluate
  * the two auctions separately (and see if the liquidation was warranted,
  * retroactively). Currently, perhaps a bit harshly, we round up for both
  * slices.  Alternatively, we can calculate min_kit_for_unwarranted_1 and then
  * calculate min_kit_for_unwarranted_2 = min_kit_for_unwarranted -
  * min_kit_for_unwarranted_1. *)
let split_liquidation_slice_contents (amnt: Ligo.tez) (contents: liquidation_slice_contents) : (liquidation_slice_contents * liquidation_slice_contents) =
  let { burrow = contents_burrow;
        tez = contents_tez;
        min_kit_for_unwarranted = contents_min_kit_for_unwarranted;
      } = contents in
  assert (amnt > Ligo.tez_from_literal "0mutez");
  assert (amnt < contents_tez);
  (* general *)
  let min_kit_for_unwarranted = kit_to_mukit_int contents_min_kit_for_unwarranted in
  let slice_tez = tez_to_mutez contents_tez in
  (* left slice *)
  let ltez = amnt in
  let lkit =
    kit_of_fraction_ceil
      (Ligo.mul_int_int min_kit_for_unwarranted (tez_to_mutez ltez))
      (Ligo.mul_int_int kit_scaling_factor_int slice_tez)
  in
  (* right slice *)
  let rtez = Ligo.sub_tez_tez contents_tez amnt in
  let rkit =
    kit_of_fraction_ceil
      (Ligo.mul_int_int min_kit_for_unwarranted (tez_to_mutez rtez))
      (Ligo.mul_int_int kit_scaling_factor_int slice_tez)
  in
  ( { burrow = contents_burrow; tez = ltez; min_kit_for_unwarranted = lkit; },
    { burrow = contents_burrow; tez = rtez; min_kit_for_unwarranted = rkit; }
  )

(** Create a slice given the contents and set older and younger to None. *)
let[@inline] make_standalone_slice (contents: liquidation_slice_contents) =
  { contents = contents;
    older = (None: leaf_ptr option);
    younger = (None: leaf_ptr option);
  }

let take_with_splitting (auctions: liquidation_auctions) (split_threshold: Ligo.tez) : liquidation_auctions * avl_ptr (* liquidation_auction_id *) =
  let queued_slices = auctions.queued_slices in

  let (storage, new_auction) = avl_take auctions.avl_storage queued_slices split_threshold (None: auction_outcome option) in
  let auctions = { auctions with avl_storage = storage } in

  let queued_amount = avl_tez storage new_auction in
  if queued_amount < split_threshold
  then
    (* split next thing *)
    let (storage, next) = avl_pop_front storage queued_slices in
    match next with
    | Some slice_ptr_and_slice ->
      let slice_ptr, slice = slice_ptr_and_slice in

      (* 1: split the contents *)
      let (part1_contents, part2_contents) =
        split_liquidation_slice_contents (Ligo.sub_tez_tez split_threshold queued_amount) slice.contents in

      (* 2: create and push the two slices to the AVL, but initially without
       * pointers to their neighbors. Before adding the slices to the AVL we
       * cannot have this information. *)
      let part1 = make_standalone_slice part1_contents in
      let (storage, part1_leaf_ptr) = avl_push storage new_auction part1 Left in

      let part2 = make_standalone_slice part2_contents in
      let (storage, part2_leaf_ptr) = avl_push storage queued_slices part2 Right in

      (* 3: fixup the pointers within slice 1 (the older of the two) as follows:
       * - part1.older = slice.older
       * - part1.younger = part2
       *)
      let storage =
        avl_update_leaf
          storage
          part1_leaf_ptr
          (fun (node: liquidation_slice) ->
             { node with
               younger = Some part2_leaf_ptr;
               older = slice.older;
             }
          ) in

      (* 4: fixup the pointers within slice 2 (the younger of the two) as follows:
       * - part2.older = part1
       * - part2.younger = slice.younger
       *)
      let storage =
        avl_update_leaf
          storage
          part2_leaf_ptr
          (fun (node: liquidation_slice) ->
             { node with
               younger = slice.younger;
               older = Some part1_leaf_ptr;
             }
          ) in

      (* 5: fixup the "younger" pointer within slice.older so that it now
       * points to part1 instead of slice. Nothing to be done if slice is the
       * oldest. *)
      let storage =
        match slice.older with
        | None -> storage (* slice was the oldest, nothing to change. *)
        | Some older_ptr ->
          avl_update_leaf
            storage
            older_ptr
            (fun (older: liquidation_slice) ->
               assert (older.younger = Some slice_ptr);
               { older with younger = Some part1_leaf_ptr }
            ) in

      (* 6: fixup the "older" pointer within slice.younger so that it now
       * points to part2 instead of slice. Nothing to be done if slice is the
       * youngest. *)
      let storage =
        match slice.younger with
        | None -> storage (* slice was the youngest, nothing to change. *)
        | Some younger_ptr ->
          avl_update_leaf
            storage
            younger_ptr
            (fun (younger: liquidation_slice) ->
               assert (younger.older = Some slice_ptr);
               { younger with older = Some part2_leaf_ptr }
            ) in

      (* 7: fixup the pointers to the oldest and youngest slices of the burrow.
       * If slice is not the oldest or the youngest itself these should remain
       * unaltered. If it's the oldest, then part1 now becomes the oldest, and
       * if it's the youngest then part2 now becomes the youngest. *)
      let new_burrow_slices =
        match Ligo.Big_map.find_opt slice.contents.burrow auctions.burrow_slices with
        | None -> (failwith "this should be impossible, right?" : burrow_liquidation_slices)
        | Some old_slices ->
          { oldest_slice =
              (match slice.older with
               | None ->
                 assert (old_slices.oldest_slice = slice_ptr);
                 part1_leaf_ptr (* slice was the oldest, now part1 is. *)
               | Some _ ->
                 assert (old_slices.oldest_slice <> slice_ptr);
                 old_slices.oldest_slice (* there was another; it remains as it was *)
              );
            youngest_slice =
              (match slice.younger with
               | None ->
                 assert (old_slices.youngest_slice = slice_ptr);
                 part2_leaf_ptr (* slice was the youngest, now part2 is. *)
               | Some _ ->
                 assert (old_slices.youngest_slice <> slice_ptr);
                 old_slices.youngest_slice (* there was another; it remains as it was *)
              );
          } in

      let auctions =
        { auctions with
          avl_storage = storage;
          burrow_slices =
            Ligo.Big_map.add
              slice.contents.burrow
              new_burrow_slices
              auctions.burrow_slices;
        } in

      (auctions, new_auction)
    | None ->
      (auctions, new_auction)
  else
    (auctions, new_auction)

let start_liquidation_auction_if_possible
    (start_price: ratio) (auctions: liquidation_auctions): liquidation_auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    let queued_amount = avl_tez auctions.avl_storage auctions.queued_slices in
    let split_threshold =
      (* split_threshold = max (max_lot_size, FLOOR(queued_amount * min_lot_auction_queue_fraction)) *)
      let { num = num_qf; den = den_qf; } = min_lot_auction_queue_fraction in
      max_tez
        max_lot_size
        (fraction_to_tez_floor
           (Ligo.mul_int_int (tez_to_mutez queued_amount) num_qf)
           (Ligo.mul_int_int (Ligo.int_from_literal "1_000_000") den_qf)
        ) in
    let (auctions, new_auction) = take_with_splitting auctions split_threshold in
    if avl_is_empty auctions.avl_storage new_auction
    then
      (* If the new auction is empty (effectively meaning that the auction
       * queue itself is empty) we garbage-collect it (it's not even referenced
       * in the output). *)
      let storage = avl_delete_empty_tree auctions.avl_storage new_auction in
      let current_auction = (None: current_liquidation_auction option) in
      { auctions with
        avl_storage = storage;
        current_auction = current_auction;
      }
    else
      let start_value =
        let { num = num_sp; den = den_sp; } = start_price in
        kit_of_fraction_ceil
          (Ligo.mul_int_int (tez_to_mutez (avl_tez auctions.avl_storage new_auction)) num_sp)
          (Ligo.mul_int_int (Ligo.int_from_literal "1_000_000") den_sp)
      in
      let current_auction =
        Some
          { contents = new_auction;
            state = Descending (start_value, !Ligo.Tezos.now); } in
      { auctions with current_auction = current_auction; }

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
  match auctions.current_auction with
  | None -> auctions
  | Some curr -> begin
      match is_liquidation_auction_complete curr.state with
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
            (storage, {youngest=curr.contents; oldest=curr.contents})
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

(* removes the slice from liquidation_auctions, fixing up the necessary pointers.
 * returns the contents of the removed slice, the tree root the slice belonged to, and the updated auctions
*)
let pop_slice (auctions: liquidation_auctions) (leaf_ptr: leaf_ptr): liquidation_slice_contents * avl_ptr * liquidation_auctions =
  let avl_storage = auctions.avl_storage in

  (* pop the leaf from the storage *)
  let leaf = avl_read_leaf avl_storage leaf_ptr in
  let avl_storage, root_ptr = avl_del avl_storage leaf_ptr in

  (* fixup burrow_slices *)
  let burrow_slices = match Ligo.Big_map.find_opt leaf.contents.burrow auctions.burrow_slices with
    | None -> (failwith "invariant violation: got a slice which is not present on burrow_slices": burrow_liquidation_slices)
    | Some s -> s in
  let burrow_slices =
    match leaf.younger with
    | None -> begin
        match leaf.older with
        | None -> (* leaf *) (None: burrow_liquidation_slices option)
        | Some older -> (* .. - older - leaf *) Some { burrow_slices with youngest_slice = older }
      end
    | Some younger -> begin
        match leaf.older with
        | None -> (* leaf - younger - ... *) Some { burrow_slices with oldest_slice = younger }
        | Some _ -> (* ... - leaf - ... *) Some burrow_slices
      end in

  (* fixup older and younger pointers *)
  let avl_storage = (
    match leaf.younger with
    | None -> avl_storage
    | Some younger_ptr ->
      avl_update_leaf
        avl_storage
        younger_ptr
        (fun (younger: liquidation_slice) ->
           assert (younger.older = Some leaf_ptr);
           { younger with older = leaf.older }
        )
  ) in
  let avl_storage = (
    match leaf.older with
    | None -> avl_storage
    | Some older_ptr ->
      avl_update_leaf
        avl_storage
        older_ptr
        (fun (older: liquidation_slice) ->
           assert (older.younger = Some leaf_ptr);
           { older with younger = leaf.younger }
        )
  ) in

  (* return *)
  ( leaf.contents
  , root_ptr
  , { auctions with
      avl_storage = avl_storage;
      burrow_slices = Ligo.Big_map.update leaf.contents.burrow burrow_slices auctions.burrow_slices;
    }
  )

let liquidation_auctions_cancel_slice (auctions: liquidation_auctions) (leaf_ptr: leaf_ptr) : liquidation_slice_contents * liquidation_auctions =
  let (contents, root, auctions) = pop_slice auctions leaf_ptr in
  if ptr_of_avl_ptr root <> ptr_of_avl_ptr auctions.queued_slices
  then (Ligo.failwith error_UnwarrantedCancellation : liquidation_slice_contents * liquidation_auctions)
  else (contents, auctions)

let completed_liquidation_auction_won_by
    (avl_storage: mem) (bid_details: liquidation_auction_bid): auction_outcome option =
  match avl_root_data avl_storage bid_details.auction_id with
  | Some outcome ->
    if bid_eq outcome.winning_bid bid_details.bid
    then Some outcome
    else (None: auction_outcome option)
  | None -> (None: auction_outcome option)

(* If successful, it consumes the ticket. *)
let liquidation_auction_reclaim_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid) : kit =
  if is_leading_current_liquidation_auction auctions bid_details
  then (Ligo.failwith error_CannotReclaimLeadingBid : kit)
  else
    match completed_liquidation_auction_won_by auctions.avl_storage bid_details with
    | Some _ -> (Ligo.failwith error_CannotReclaimWinningBid : kit)
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

let liquidation_auctions_pop_completed_slice (auctions: liquidation_auctions) (leaf_ptr: leaf_ptr) : liquidation_slice_contents * auction_outcome * liquidation_auctions =
  let (contents, root, auctions) = pop_slice auctions leaf_ptr in

  (* When the auction has no slices left, we pop it from the linked list
   * of lots. We do not delete the auction itself from the storage, since
   * we still want the winner to be able to claim its result. *)
  let auctions =
    if avl_is_empty auctions.avl_storage root
    then liquidation_auction_pop_completed_auction auctions root
    else auctions in
  let outcome =
    match avl_root_data auctions.avl_storage root with
    | None -> (Ligo.failwith error_NotACompletedSlice: auction_outcome)
    | Some outcome -> outcome in
  (contents, outcome, auctions)

(* If successful, it consumes the ticket. *)
let[@inline] liquidation_auction_reclaim_winning_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid) : (Ligo.tez * liquidation_auctions) =
  match completed_liquidation_auction_won_by auctions.avl_storage bid_details with
  | Some outcome ->
    (* A winning bid can only be claimed when all the liquidation slices
     * for that lot is cleaned. *)
    if not (avl_is_empty auctions.avl_storage bid_details.auction_id)
    then (Ligo.failwith error_NotAllSlicesClaimed : Ligo.tez * liquidation_auctions)
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
  | None -> (Ligo.failwith error_NotAWinningBid : Ligo.tez * liquidation_auctions)


let liquidation_auction_touch (auctions: liquidation_auctions) (price: ratio) : liquidation_auctions =
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

let is_burrow_done_with_liquidations (auctions: liquidation_auctions) (burrow: Ligo.address) =
  match Ligo.Big_map.find_opt burrow auctions.burrow_slices with
  | None -> true
  | Some bs ->
    let root = avl_find_root auctions.avl_storage bs.oldest_slice in
    let outcome = avl_root_data auctions.avl_storage root in
    (match outcome with
     | None -> true
     | Some _ -> false)

(* BEGIN_OCAML *)

let liquidation_auction_current_auction_tez (auctions: liquidation_auctions) : Ligo.tez option =
  match auctions.current_auction with
  | None -> (None: Ligo.tez option)
  | Some auction -> Some (avl_tez auctions.avl_storage auction.contents)

(* Checks if some invariants of auctions structure holds. *)
let assert_liquidation_auction_invariants (auctions: liquidation_auctions) : unit =

  (* All AVL trees in the storage are valid. *)
  let mem = auctions.avl_storage in
  let roots = Ligo.Big_map.bindings mem.mem
              |> List.filter (fun (_, n) -> match n with | LiquidationAuctionPrimitiveTypes.Root _ -> true; | _ -> false)
              |> List.map (fun (p, _) -> AVLPtr p) in
  List.iter (assert_avl_invariants mem) roots;

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
