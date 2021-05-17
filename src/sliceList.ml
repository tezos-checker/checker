(* Double-linked lists which act as an index for data stored in the AVL queue.
 * I would have really liked for these to be polymorphic, but can't find a way
 * to make this happen in Ligo.
*)
open Avl
open Mem
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes

(* "header" * element *)
type element = Element of (leaf_ptr * liquidation_slice)

type bounds = {
  youngest_ptr : leaf_ptr;
  oldest_ptr : leaf_ptr;
}

type meta = {
  burrow: burrow_id;
  bounds: bounds option
}

(* Question: Is it worth storing one of the end elements within the SliceList?  *)
type t = SliceList of meta

let empty (burrow: burrow_id) = SliceList {burrow=burrow; bounds=(None:bounds option);}

(* Constructs a burrow slice list for the given burrow id using the provided auction state *)
let from_auction_state (auctions: liquidation_auctions) (burrow_id: burrow_id) =
  match Ligo.Big_map.find_opt burrow_id auctions.burrow_slices with
  | None -> SliceList {burrow=burrow_id; bounds=(None:bounds option)}
  | Some bs ->
    SliceList {
      burrow = burrow_id;
      bounds = Some {
          youngest_ptr = bs.youngest_slice;
          oldest_ptr = bs.oldest_slice;
        };
    }

(* Constructs an element from a burrow leaf in the AVL *)
let from_leaf_ptr (auctions: liquidation_auctions) (ptr: leaf_ptr) =
  let slice = avl_read_leaf auctions.avl_storage ptr in
  let element = Element (ptr, slice) in
  let list = from_auction_state auctions slice.contents.burrow in
  (* TODO: What if burrow list is empty? In old implementation we throw an invariant error *)
  (* FIXME: Add assertion here that checks if the element exists in the list *)
  element, list

(* Updates the burrow slices in the provided auction state using the given burrow slice list *)
let to_auction_state (auctions: liquidation_auctions) (l: t) =
  match l with SliceList meta ->
    let burrow_liquidation_slice = match meta.bounds with
      | None -> (None: burrow_liquidation_slices option)
      | Some bounds -> (Some {
          youngest_slice=bounds.youngest_ptr;
          oldest_slice=bounds.oldest_ptr;
        })
    in
    let burrow_slices = Ligo.Big_map.update meta.burrow burrow_liquidation_slice auctions.burrow_slices in
    {auctions with burrow_slices = burrow_slices;}


(* Appends a new element to the list. This element will be the "youngest" one in the list.
   You must specify an avl root which this new element will reside under along with the
   end of the avl queue which you would like to place the element at.
*)
let append (l:t) (storage:mem) (root:liquidation_auction_id) (queue_end:queue_end) (slice_contents:liquidation_slice_contents) =
  let meta = match l with SliceList m -> m in
  (* FIXME: Throw a specific error here *)
  assert (slice_contents.burrow = meta.burrow);
  match meta.bounds with
  (* List is empty, creating the first element *)
  | None ->
    let slice = {younger=(None: leaf_ptr option); older=(None: leaf_ptr option); contents=slice_contents;} in
    (* Write slice to AVL backend *)
    let storage, ptr = match queue_end with
      | Back -> avl_push_back storage root slice
      | Front -> avl_push_front storage root slice
    in
    let bounds = {
      youngest_ptr=ptr;
      oldest_ptr=ptr;
    }
    in storage, SliceList {meta with bounds=Some bounds;}, Element (ptr, slice)
  (* The list already has some elements. Need to do some updating.*)
  | Some bounds ->
    (* The new element is now the youngest *)
    let slice = {younger=(None: leaf_ptr option); older=Some bounds.youngest_ptr; contents=slice_contents;} in
    (* Write the new slice to AVL backend *)
    let storage, ptr = avl_push_back storage root slice in
    (* Touch up the old element in the backend *)
    let former_youngest = bounds.youngest_ptr in
    let storage = avl_update_leaf storage former_youngest (fun (s: liquidation_slice) -> {s with younger = Some ptr}) in
    (* Update up the bounds with the new youngest element *)
    let bounds = {bounds with youngest_ptr = ptr} in
    storage, SliceList {meta with bounds=Some bounds;}, Element (ptr, slice)

(* Removes and returns the last element (the youngest) from the list in same time as ref_del
 * FIXME: This function might not be required in the Ligo code
*)
let pop (l:t) (storage:mem) =
  let meta = match l with SliceList m -> m in
  match meta.bounds with
  | None -> storage, l, (None: liquidation_slice_contents option)
  | Some bounds ->
    let slice_ptr = bounds.youngest_ptr in
    let slice = avl_read_leaf storage bounds.youngest_ptr in
    let new_youngest = slice.older in
    (* Remove the pointers from the popped element and remove it from the storage*)
    let slice = {slice with younger = (None: leaf_ptr option); older=(None: leaf_ptr option);} in
    let storage, _ = avl_del storage slice_ptr in
    (* Touch up the *new* youngest slice *)
    (match new_youngest with
     (* No older slices, the list is now empty *)
     | None -> storage, SliceList {meta with bounds=(None:bounds option)}, Some slice.contents
     (* 1 or more older slices, the next slice is now the youngest *)
     | Some new_youngest_ptr ->
       (* Need to update the slice itself and the list metadata *)
       let storage = avl_update_leaf storage new_youngest_ptr (fun (s: liquidation_slice) -> {s with younger = (None: leaf_ptr option)}) in
       let bounds = {bounds with youngest_ptr=new_youngest_ptr;} in
       (* Update the list metadata with the new youngest slice *)
       storage, SliceList {meta with bounds=Some bounds;}, Some slice.contents
    )

(* Remove the element from the list, returning its contents *)
let remove (l:t) (storage:mem) (e:element) =
  let meta = match l with SliceList m -> m in
  let ptr, slice = match e with Element (ptr, slice) -> ptr, slice in
  assert (meta.burrow = slice.contents.burrow);
  match meta.bounds with
  (* FIXME: Throw specific error here *)
  (* FIXME: This does not seem to be updating pointers of neighboring elements in the list *)
  | None -> (failwith "Attempting to remove an element from an empty list" : mem*t*avl_ptr*liquidation_slice_contents)
  | Some bounds ->
    (* Update the list metadata: *)
    (* Case 1: We are removing the youngest slice *)
    let bounds =
      if ptr = bounds.youngest_ptr then
        (* Case 1.1 it is the only element (i.e. also the oldest slice). The list is now empty.*)
        match slice.older with
        | None ->
          assert (ptr = bounds.oldest_ptr);
          (None: bounds option)
        (* Case 1.2 there is another element. This one is now the youngest *)
        | Some older_ptr -> Some {bounds with youngest_ptr=older_ptr;}
      else
        (* Case 2: We are removing the oldest slice *)
      if ptr = bounds.oldest_ptr then
        (* Case 2.1 it is the only element (i.e. also the youngest slice). The list is now empty *)
        match slice.younger with
        | None ->
          assert (ptr = bounds.youngest_ptr);
          (None: bounds option)
        (* Case 2.2 there is another element. This one is now the oldest *)
        | Some younger_ptr -> Some {bounds with oldest_ptr=younger_ptr;}
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
    storage, SliceList {meta with bounds=bounds;}, root_ptr, slice.contents

(* BEGIN_OCAML *)
(* Extra functionality we want for testing, etc. can go here.
      e.g. folds, length, map
*)
type direction = Older | Younger
(* END_OCAML *)
