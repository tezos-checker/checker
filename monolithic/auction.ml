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

type slice_ptr = BigMap.ptr

type tree = BigMap.ptr option

type auction_start = { block_number: block_number; time: time }

type bid = { address: Address.t; kit_utxo: Kit.utxo }

type auction_id = unit
type bid_token = { auction_id: auction_id; bid: bid }

type current_auction = {
  id: auction_id;
  contents: tree;
  start: auction_start;
  leading_bid: bid option;
}

type auction_outcome = {
  sold_tez: Tez.t;
  got_kit: Kit.t;
}

module PtrMap =
  Map.Make(struct type t = BigMap.ptr let compare = Int64.compare end)

type auctions = {
  storage: liquidation_slice mem;

  queued_slices: tree;
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
  : auctions * slice_ptr =
  let (new_storage, new_queue, ret) =
        push_back auctions.storage auctions.queued_slices slice slice.tez in
  let new_state = { auctions with
                    storage = new_storage;
                    queued_slices = Some new_queue; } in
  (new_state, ret)

let cancel_liquidation
  (auctions: auctions)
  (slice: slice_ptr)
  : auctions option =
  if Some (find_root auctions.storage slice) = auctions.queued_slices
  then
    (* if the slice belongs to queued_slices tree, we can cancel it *)
    let (new_storage, new_queue) = del auctions.storage slice in
    Some
      { auctions with
        storage = new_storage;
        queued_slices = new_queue;
      }
  else
    (* otherwise, it means that the auction is either in progress
     * or completed, so we can not cancel it. *)
    None

let liquidation_outcome
  (auctions: auctions)
  (slice_ptr: slice_ptr)
  : (auctions * Kit.t) option =
  let root = find_root auctions.storage slice_ptr in
  match PtrMap.find_opt root auctions.completed_auctions with
    | None -> None (* slice does not correspond to a completed auction *)
    | Some outcome ->
      let slice = match BigMap.mem_get auctions.storage slice_ptr with
        | Leaf leaf -> leaf.value
        | Branch _ -> failwith "slice should point to a leaf" in
      let kit = Kit.of_fp FixedPoint.FixedPoint.(
        (Tez.to_fp slice.tez) * (Kit.to_fp outcome.got_kit)
          / (Tez.to_fp outcome.sold_tez)) in

      (* Now we delete the slice from the lot, so it can not be
       * withdrawn twice, also to save storage. This might cause
       * the lot root to change, so we also update completed_auctions
       * to reflect that.
       *)
      let (storage, popped) = del auctions.storage slice_ptr in
      let replaced =
            auctions.completed_auctions
              |> PtrMap.remove root
              |> Option.fold
                   ~some:(fun t -> PtrMap.add t outcome)
                   ~none:(fun i -> i)
                   popped in
      let auctions =
            { auctions with
              storage = storage;
              completed_auctions = replaced;
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
       let (storage, new_auction, new_queue) =
            split
              auctions.storage
              auctions.queued_slices
              (Tez.of_fp (FixedPoint.FixedPoint.of_int 10_000)) in
       let current_auction =
             if Option.is_none new_auction
             then None
             else Some
                    { id = ();
                      contents = new_auction;
                      start = { time = (); block_number = (); };
                      leading_bid = None; } in
       { auctions with
          storage = storage;
          queued_slices = new_queue;
          current_auction = current_auction;
       }

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
