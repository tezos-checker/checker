open LiquidationAuctionTypes
open Ratio
open FixedPoint
open Kit
open Constants
open Common
open Tickets
open LiquidationAuctionTypes
open Error

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

(* When burrows send a liquidation_slice, they get a pointer into a tree leaf.
 * Initially that node belongs to 'queued_slices' tree, but this can change over time
 * when we start auctions.
*)
let rec liquidation_auction_send_to_auction (auctions: liquidation_auctions) (slice: liquidation_slice): liquidation_auctions =
  assert (slice.tez <= max_lot_size);
  if Ligo.List.length auctions.lots >= max_pending_lot_count then
    (Ligo.failwith error_LiquidationQueueTooLong : liquidation_auctions)
  else
    let slice_id = auctions.next_slice_id in
    match auctions.lots with
    | [] ->
      let lot = { first_slice = slice_id; last_slice = slice_id; held_tez = slice.tez; } in
      (* TODO: Maybe start the auction here instead of waiting for the next touch *)
      { auctions with lots = [lot] }
    | newest_lot::older_lots ->
      let remaining_space = Ligo.sub_tez_tez max_lot_size newest_lot.held_tez in
      if Ligo.eq_tez_tez remaining_space (Ligo.tez_from_literal "0mutez") then
        let lot = { first_slice = slice_id; last_slice = slice_id; held_tez = slice.tez; } in
        { auctions with lots = lot :: newest_lot :: older_lots }
      else if Ligo.leq_tez_tez slice.tez remaining_space then
        { auctions with
          lots =
            { newest_lot with
              last_slice = slice_id;
              held_tez = Ligo.add_tez_tez newest_lot.held_tez slice.tez;
            } :: older_lots
        }
      else
        let (l, r) = split_liquidation_slice remaining_space slice in

        assert (Ligo.eq_tez_tez (Ligo.add_tez_tez newest_lot.held_tez l.tez) max_lot_size);

        let a1 = { auctions with
                   lots =
                     { newest_lot with
                       last_slice = slice_id;
                       held_tez = max_lot_size;
                     } :: older_lots
                 } in
        liquidation_auction_send_to_auction a1 r

let unsnoc_lots (input: lot list): (lot list * lot) option =

  let rec go (acc: lot list) (xs: lot list) =
    match xs with
    | [] -> None
    | x::xs -> begin
        match xs with
        | [] -> Some (acc, x)
        | xs -> go (x::acc) xs
      end in

  let rec reverse (acc: lot list) (xs: lot list) =
    match xs with
    | [] -> acc
    | x::xs -> reverse (x :: acc) xs in

  match go [] input with
  | None -> None
  | Some p ->
    let (reversed_init, last) = p in
    Some (reverse [] reversed_init, last)

let[@inline] lot_contains (lot: lot) (slices: (slice_id, liquidation_slice) Ligo.big_map) (slice_id: slice_id): bool =
  (lot.first_slice <= slice_id && slice_id <= lot.last_slice)
  && (match Ligo.Big_map.find_opt slice_id slices with | None -> false | Some _ -> true)

let[@inline] lookup_completed_slice
    (auctions: liquidation_auctions)
    (auction_id, slice_id: slice_handle):
  (auction_outcome * liquidation_slice) option =
  match Ligo.Big_map.find_opt auction_id auctions.completed_auctions with
  | None -> None
  | Some p ->
    let (lot, outcome) = p in
    if not (lot_contains lot auctions.slices slice_id)
    then None
    else begin
      match Ligo.Big_map.find_opt slice_id auctions.slices with
      | None -> None
      | Some slice -> Some (outcome, slice)
    end

let[@inline] delete_completed_slice
    (auctions: liquidation_auctions)
    (auction_id, slice_id: slice_handle):
  liquidation_auctions =
  match Ligo.Big_map.find_opt auction_id auctions.completed_auctions with
  | None -> auctions
  | Some p -> begin
      let (lot, outcome) = p in
      let (slice_opt, slices) = Ligo.Big_map.get_and_update slice_id None auctions.slices in
      match slice_opt with
      | None -> auctions
      | Some slice ->
        let new_lot = { lot with held_tez = Ligo.sub_tez_tez lot.held_tez slice.tez; } in
        { auctions with
          slices = slices;
          completed_auctions = Ligo.Big_map.add auction_id (new_lot, outcome) auctions.completed_auctions;
        }
    end

(*
    let split_threshold =
      max_tez
        max_lot_size
        (ratio_to_tez_floor
           (mul_ratio
              (ratio_of_tez queued_amount)
              (fixedpoint_to_ratio min_lot_auction_queue_fraction)
           )
        ) in
*)
let start_liquidation_auction_if_possible
    (start_price: fixedpoint) (auctions: liquidation_auctions): liquidation_auctions =
  match auctions.current_auction with
  | Some _ -> auctions
  | None ->
    match unsnoc_lots auctions.lots with
    | None -> auctions
    | Some (remaining_lots, lot) ->
      let start_value =
        kit_scale
          kit_one
          (fixedpoint_mul
             (fixedpoint_of_ratio_floor (ratio_of_tez lot.held_tez))
             start_price
          ) in
      let auction_id = auctions.next_auction_id in
      { auctions with
        current_auction = Some {
            auction_id = auction_id;
            lot = lot;
            state = Descending (start_value, !Ligo.Tezos.now);
          };
        next_auction_id = Ligo.add_nat_nat auction_id (Ligo.nat_from_literal "1n");
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
        let outcome = { sold_tez=curr.lot.held_tez; winning_bid=winning_bid } in
        { auctions with
          current_auction=None;
          completed_auctions=
            Ligo.Big_map.update
              curr.auction_id
              (Some (curr.lot, outcome))
              auctions.completed_auctions;
        }
    end

(** Place a bid in the current auction. Fail if the bid is too low (must be at
  * least as much as the liquidation_auction_current_auction_minimum_bid. *)
let liquidation_auction_place_bid (auction: current_liquidation_auction) (bid: bid) : (current_liquidation_auction * liquidation_auction_bid) =
  if bid.kit >= liquidation_auction_current_auction_minimum_bid auction
  then
    ( { auction with state = Ascending (bid, !Ligo.Tezos.now, !Ligo.Tezos.level); },
      { auction_id = auction.auction_id; bid = bid; }
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
    if auction.auction_id = bid_details.auction_id
    then
      (match auction.state with
       | Ascending params ->
         let (bid, _timestamp, _level) = params in
         bid_eq bid bid_details.bid
       | Descending _ -> false)
    else false
  | None -> false

let completed_liquidation_auction_won_by (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid): (lot * auction_outcome) option =
  match Ligo.Big_map.find_opt bid_details.auction_id auctions.completed_auctions with
  | Some p ->
    let (_, outcome) = p in
    if bid_eq outcome.winning_bid bid_details.bid
    then Some p
    else (None: (lot * auction_outcome) option)
  | None -> (None: (lot * auction_outcome) option)

(* If successful, it consumes the ticket. *)
let liquidation_auction_reclaim_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid) : kit =
  if is_leading_current_liquidation_auction auctions bid_details
  then (Ligo.failwith error_CannotReclaimLeadingBid : kit)
  else
    match completed_liquidation_auction_won_by auctions bid_details with
    | Some _ -> (Ligo.failwith error_CannotReclaimWinningBid : kit)
    | None -> bid_details.bid.kit

(* If successful, it consumes the ticket. *)
let[@inline] liquidation_auction_reclaim_winning_bid (auctions: liquidation_auctions) (bid_details: liquidation_auction_bid) : Ligo.tez * liquidation_auctions =
  match completed_liquidation_auction_won_by auctions bid_details with
  | Some p ->
    let (lot, outcome) = p in

    (* A winning bid can only be claimed when all the liquidation slices
     * for that lot is cleaned. *)
    if Ligo.gt_tez_tez lot.held_tez (Ligo.tez_from_literal "0mutez")
    then (Ligo.failwith error_NotAllSlicesClaimed : Ligo.tez * liquidation_auctions)
    else begin
      (* When the winner reclaims their bid, we finally remove
         every reference to the auction. This is just to
         save storage, what's forbidding double-claiming
         is the ticket mechanism, not this.
      *)
      let auctions =
        { auctions with
          completed_auctions = Ligo.Big_map.update bid_details.auction_id None auctions.completed_auctions;
        } in
      (outcome.sold_tez, auctions)
    end
  | None -> (Ligo.failwith error_NotAWinningBid : Ligo.tez * liquidation_auctions)

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

let liquidation_auction_oldest_completed_liquidation_slice
    (auctions: liquidation_auctions)
  : slice_id option =
  let upper_bound =
    match auctions.current_auction with
    | Some current_auction -> current_auction.lot.first_slice
    | None -> begin
        match last_lot auctions.lots with
        | None -> auctions.next_slice_id
        | Some lot -> lot.first_slice
      end in
  let rec go
      (auctions: liquidation_auctions)
      (max_to_touch: nat)
      (max_to_poll: nat)
    : auctions =
    failwith "FIXME Steve"


      if Ligo.ge_nat_nat auctions.next_unclaimed_slice_id upper_bound
      then None
      else failwith "FIXME Steve"



(* BEGIN_OCAML *)

let liquidation_auction_current_auction_tez (auctions: liquidation_auctions) : Ligo.tez option =
  match auctions.current_auction with
  | None -> (None: Ligo.tez option)
  | Some auction -> Some auction.lot.held_tez

(* Checks if some invariants of auctions structure holds. *)
let assert_liquidation_auction_invariants (auctions: liquidation_auctions) : unit =
  failwith "FIXME";
  ()
(* END_OCAML *)
