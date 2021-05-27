(* Double-linked list for burrow slices which acts as a higher-level interface for data stored in the AVL queue.
 * This data structure allows for fast lookups of slices for a specific burrow, and
 * functions which are adding or removing slices from the queue should use this module instead of
 * AVL.ml directly since this module will automatically ensure that the burrow slice lists stay up
 * to date.
*)
open Avl
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes

type slice_list_element = SliceListElement of (leaf_ptr * liquidation_slice)
[@@deriving show]

let slice_list_element_contents (e: slice_list_element) : liquidation_slice_contents =
  match e with
  | SliceListElement (_, contents) -> contents.contents

type slice_list_bounds = {
  slice_list_youngest_ptr : leaf_ptr;
  slice_list_oldest_ptr : leaf_ptr;
}
[@@deriving show]

type slice_list_meta = {
  slice_list_burrow: burrow_id;
  slice_list_bounds: slice_list_bounds option
}
[@@deriving show]

(* Question: Is it worth storing one of the end elements within the SliceList?  *)
type slice_list = SliceList of slice_list_meta
[@@deriving show]

let slice_list_empty (burrow: burrow_id) : slice_list = SliceList {slice_list_burrow=burrow; slice_list_bounds=(None:slice_list_bounds option);}

let slice_list_is_empty (l: slice_list) : bool =
  let meta = match l with SliceList meta -> meta in
  match meta.slice_list_bounds with
  | Some _ -> false
  | None -> true

(* Constructs a burrow slice list for the given burrow id using the provided auction state *)
let slice_list_from_auction_state (auctions: liquidation_auctions) (burrow_id: burrow_id) : slice_list =
  match Ligo.Big_map.find_opt burrow_id auctions.burrow_slices with
  | None -> SliceList {slice_list_burrow=burrow_id; slice_list_bounds=(None:slice_list_bounds option)}
  | Some bs ->
    SliceList {
      slice_list_burrow = burrow_id;
      slice_list_bounds = Some {
          slice_list_youngest_ptr = bs.youngest_slice;
          slice_list_oldest_ptr = bs.oldest_slice;
        };
    }

(* Constructs an element from a burrow leaf in the AVL *)
let slice_list_from_leaf_ptr (auctions: liquidation_auctions) (ptr: leaf_ptr) : (slice_list_element * slice_list) =
  let slice = avl_read_leaf auctions.avl_storage ptr in
  let element = SliceListElement (ptr, slice) in
  let list = slice_list_from_auction_state auctions slice.contents.burrow in
  let _ = if slice_list_is_empty list then
      failwith "invariant violation: corresponding list for slice was empty"
    else ()
  in
  (* FIXME: Add assertion here that checks if the element exists in the list *)
  element, list

(* Constructs an element from the first item in the auction queue.
   Does NOT remove the corresponding slice from the queue. *)
let slice_list_from_queue_head (auctions: liquidation_auctions) : (slice_list_element * slice_list) option =
  match avl_peek_front auctions.avl_storage auctions.queued_slices with
  | Some (ptr, slice) ->
    (* Constructing the element directly since we already have read its contents *)
    let element = SliceListElement (ptr, slice.value) in
    let list = slice_list_from_auction_state auctions slice.value.contents.burrow in
    Some (element, list)
  | None -> (None : (slice_list_element * slice_list) option)

(* Updates the burrow slices in the provided auction state using the given burrow slice list *)
let slice_list_to_auction_state (auctions: liquidation_auctions) (l: slice_list) : liquidation_auctions =
  match l with SliceList meta ->
    let burrow_liquidation_slice = match meta.slice_list_bounds with
      | None -> (None: burrow_liquidation_slices option)
      | Some bounds -> (Some {
          youngest_slice=bounds.slice_list_youngest_ptr;
          oldest_slice=bounds.slice_list_oldest_ptr;
        })
    in
    let burrow_slices = Ligo.Big_map.update meta.slice_list_burrow burrow_liquidation_slice auctions.burrow_slices in
    {auctions with burrow_slices = burrow_slices;}


(* Appends a new element to the list. This element will be the "youngest" one in the list.
   You must specify an avl root which this new element will reside under along with the
   end of the avl queue which you would like to place the element at.
*)
let slice_list_append (l:slice_list) (auctions:liquidation_auctions) (queue_end:queue_end) (slice_contents:liquidation_slice_contents) : (liquidation_auctions * slice_list * slice_list_element) =
  let storage = auctions.avl_storage in
  let meta = match l with SliceList m -> m in
  (* FIXME: Perhaps throw specific error code here? *)
  assert (slice_contents.burrow = meta.slice_list_burrow);
  match meta.slice_list_bounds with
  (* List is empty, creating the first element *)
  | None ->
    let slice = {younger=(None: leaf_ptr option); older=(None: leaf_ptr option); contents=slice_contents;} in
    (* Write slice to AVL backend *)
    let storage, ptr = match queue_end with
      | Back -> avl_push_back storage auctions.queued_slices slice
      | Front -> avl_push_front storage auctions.queued_slices slice
    in
    let bounds = {
      slice_list_youngest_ptr=ptr;
      slice_list_oldest_ptr=ptr;
    }
    in {auctions with avl_storage=storage;}, SliceList {meta with slice_list_bounds=Some bounds;}, SliceListElement (ptr, slice)
  (* The list already has some elements. Need to do some updating.*)
  | Some bounds ->
    (* The new element is now the youngest *)
    let slice = {younger=(None: leaf_ptr option); older=Some bounds.slice_list_youngest_ptr; contents=slice_contents;} in
    (* Write slice to AVL backend *)
    let storage, ptr = match queue_end with
      | Back -> avl_push_back storage auctions.queued_slices slice
      | Front -> avl_push_front storage auctions.queued_slices slice
    in
    (* Touch up the old element in the backend *)
    let former_youngest = bounds.slice_list_youngest_ptr in
    let storage = avl_update_leaf storage former_youngest (fun (s: liquidation_slice) -> {s with younger = Some ptr}) in
    (* Update up the bounds with the new youngest element *)
    let bounds = {bounds with slice_list_youngest_ptr = ptr} in
    {auctions with avl_storage=storage;}, SliceList {meta with slice_list_bounds=Some bounds;}, SliceListElement (ptr, slice)

(* Remove the element from the list, returning its contents *)
let slice_list_remove (l:slice_list) (auctions:liquidation_auctions) (e:slice_list_element) : (liquidation_auctions * slice_list * liquidation_auction_id * liquidation_slice_contents) =
  let storage = auctions.avl_storage in
  let meta = match l with SliceList m -> m in
  let ptr, slice = match e with SliceListElement (ptr, slice) -> ptr, slice in
  assert (meta.slice_list_burrow = slice.contents.burrow);
  match meta.slice_list_bounds with
  (* FIXME: Perhaps throw specific error code here? *)
  | None -> (failwith "Attempting to remove an element from an empty list" : liquidation_auctions*slice_list*avl_ptr*liquidation_slice_contents)
  | Some bounds ->
    (* Update the list metadata: *)
    (* Case 1: We are removing the youngest slice *)
    let bounds =
      if ptr = bounds.slice_list_youngest_ptr then
        (* Case 1.1 it is the only element (i.e. also the oldest slice). The list is now empty.*)
        match slice.older with
        | None ->
          assert (ptr = bounds.slice_list_oldest_ptr);
          (None: slice_list_bounds option)
        (* Case 1.2 there is another element. This one is now the youngest *)
        | Some older_ptr -> Some {bounds with slice_list_youngest_ptr=older_ptr;}
      else
        (* Case 2: We are removing the oldest slice *)
      if ptr = bounds.slice_list_oldest_ptr then
        (* Case 2.1 it is the only element (i.e. also the youngest slice). The list is now empty *)
        match slice.younger with
        | None ->
          assert (ptr = bounds.slice_list_youngest_ptr);
          (None: slice_list_bounds option)
        (* Case 2.2 there is another element. This one is now the oldest *)
        | Some younger_ptr -> Some {bounds with slice_list_oldest_ptr=younger_ptr;}
        (* Case 3: we are removing an element assumed to be in the interior of the list.
          * For performance reasons we can't validate that this element actually resides in the list,
          * only that it belongs to the same burrow id. In this case there is nothing to update.
        *)
      else Some bounds
    in
    (* Need to update the pointers to this element in its neighbors *)
    let storage = match slice.older with
      | None -> storage
      | Some older_ptr -> avl_update_leaf storage older_ptr (fun (s: liquidation_slice) -> {s with younger=slice.younger})
    in
    let storage = match slice.younger with
      | None -> storage
      | Some younger_ptr -> avl_update_leaf storage younger_ptr (fun (s: liquidation_slice) -> {s with older=slice.older})
    in
    (* Delete the element from the AVL backend *)
    let storage, root_ptr = avl_del storage ptr in
    {auctions with avl_storage=storage;}, SliceList {meta with slice_list_bounds=bounds;}, root_ptr, slice.contents

(* BEGIN_OCAML *)
(* Extra functionality we want for testing, etc. can go here.
      e.g. folds, length, map
*)
(* Gets the youngest element of the list *)
let slice_list_youngest (l: slice_list) (auctions: liquidation_auctions) : slice_list_element option =
  let storage = auctions.avl_storage in
  let meta = match l with SliceList meta -> meta in
  match meta.slice_list_bounds with
  | Some bounds -> Some (SliceListElement (bounds.slice_list_youngest_ptr, avl_read_leaf storage bounds.slice_list_youngest_ptr))
  | None -> None

(* Gets the oldest element of the list *)
let slice_list_oldest (l: slice_list) (auctions: liquidation_auctions) : slice_list_element option =
  let storage = auctions.avl_storage in
  let meta = match l with SliceList meta -> meta in
  match meta.slice_list_bounds with
  | Some bounds -> Some (SliceListElement (bounds.slice_list_oldest_ptr, avl_read_leaf storage bounds.slice_list_oldest_ptr))
  | None -> None
(* END_OCAML *)
