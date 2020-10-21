(*
 * A subset of checker types. We should split those to a separate module
 * in future to avoid the module cycle.
 *)

type mutez = int
type item_id = int64

(* A liquidation item *)
type item = {
  id: item_id;
  mutez: mutez;
}

(*
 * A doubly-linked balanced tree where the leaves contain the liquidation
 * items, and the branches contain the amount of tez on their left and
 * right children.
 *)

type ptr = int64

type leaf = {
  item: item;
  parent: int64 option;
}

type branch = {
  left: ptr;
  left_height: int;
  left_mutez: mutez;
  key: item_id;
  right_mutez: mutez;
  right_height: int;
  right: ptr;
  parent: int64 option;
}

type node =
  | Leaf of leaf
  | Branch of branch

let node_mutez n =
  match n with
    | Leaf leaf -> leaf.item.mutez
    | Branch branch -> branch.left_mutez + branch.right_mutez

let node_height n =
  match n with
    | Leaf leaf -> 1
    | Branch branch -> max branch.left_height branch.right_height + 1

(* FIXME:
 * To reduce the lookups from the map, this type was used to pass the
 * pointers alongside with their value when we had access to both, but
 * it makes the code more complex, so we should get rid of this.
 *)
type ptr_pair = { ptr: ptr; value: node }

let empty: ptr option = None

(*
 * BigMap
 *
 * We use a bigmap as our memory, and an int64 as addresses.
 *
 * There is no garbage collection, so operations are responsible for
 * not leaving any dangling pointers.
 *
 * We always increase the memory addresses, even after removals. But
 * int64 is big enough that it shouldn't be an issue.
 *
 * TODO: Maybe we should use something like [int8] as a variable
 * width address.
 *)

module Mem = Map.Make(Int64)
type mem = node Mem.t

let mem_next_ptr (m: 'a Mem.t): ptr =
  match Mem.max_binding_opt m with
        | None -> Int64.zero
        | Some (t, _) -> Int64.succ t

let mem_set (m: 'a Mem.t) (k: ptr) (v: 'a) : 'a Mem.t * ptr_pair =
  (Mem.add k v m, { ptr=k; value=v; })

let mem_new (m: 'a Mem.t) (v: 'a) : 'a Mem.t * ptr_pair =
  mem_set m (mem_next_ptr m) v

let mem_get (m: 'a Mem.t) (k: ptr) : ptr_pair =
  { ptr=k; value=Mem.find k m; }

(*
 * Operations on AVL trees.
 *
 * The resulting 'ptr' and 'mem' should always be used together. They are
 * not as part of a product type, because a 'mem' can carry multiple trees,
 * in case of 'split'.
 *
 * The operations do not move leaves, so pointers to the leaves are stable
 * (unless a leaf is deleted with 'del').
 *)

(* TODO *)
let balance (mem: mem) (root: ptr_pair) : mem * ptr_pair = (mem, root)

let rec add (mem: mem) (root: ptr option) (item : item) : mem * ptr_pair =
  match root with
    (* When the tree is empty, create the initial leaf. *)
    | None ->
        let node = Leaf { item=item; parent=None; } in
        mem_new mem node
    (* When there is already an element, *)
    | Some k ->
        match Mem.find k mem with
          (* ... and if it is a leaf,*)
          | Leaf { item = existing_item; parent = parent; } ->
            (match compare existing_item.id item.id with
              (* ... we override it if the keys are the same. *)
              | cmp when cmp == 0 ->
                  (* NOTE: I can not think of a case where we'd overwrite an
                   * existing liquidation, so maybe this case should fail.
                   *)
                  let node = Leaf {item = item; parent=parent; } in
                  let mem = Mem.add k node mem in
                  (mem, { ptr=k; value=node; })
              (* ... or we create a sibling leaf and a branch.  *)
              | cmp ->
                let new_ptr = mem_next_ptr(mem) in
                let branch_ptr = Int64.succ new_ptr in
                let (left, left_ptr, right, right_ptr) =
                  if cmp < 0
                  then (existing_item, k, item, new_ptr)
                  else (item, new_ptr, existing_item, k) in
                let left_leaf = Leaf { item=left; parent=Some branch_ptr; } in
                let right_leaf = Leaf { item=right; parent=Some branch_ptr; } in
                let new_branch = Branch {
                  left = left_ptr;
                  left_height = 1;
                  left_mutez = left.mutez;
                  key = right.id;
                  right_mutez = right.mutez;
                  right_height = 1;
                  right = right_ptr;
                  parent = parent;
                } in
                let mem = Mem.add left_ptr left_leaf mem in
                let mem = Mem.add right_ptr right_leaf mem in
                let mem = Mem.add branch_ptr new_branch mem in
                (mem, { ptr=branch_ptr; value=new_branch; })
            )
          (* ... if it is a branch, we insert it to the corresponding side
           * updating the aggregates on the branch.
           *)
          | Branch existing_branch ->
            let target_left = existing_branch.key < item.id in
            let (mem, { ptr=new_subtree; value=new_node; }) =
              add
                mem
                (Some
                  (if target_left
                   then existing_branch.left
                   else existing_branch.right))
                 item in
            let new_branch =
                  if target_left
                  then Branch {
                         existing_branch with
                         left = new_subtree;
                         left_mutez = node_mutez new_node;
                         left_height = node_height new_node;
                       }
                  else Branch {
                         existing_branch with
                         right = new_subtree;
                         right_mutez = node_mutez new_node;
                         right_height = node_height new_node;
                       } in
            mem_set mem k new_branch

let rec del (mem: mem) (root: ptr option) (id : item_id) : mem * ptr_pair option =
  match root with
    | None ->
        (mem, None)
    | Some k ->
        match Mem.find k mem with
          | Leaf existing ->
            if existing.item.id == id
            then (Mem.remove k mem, None)
            else (mem, Some { ptr=k; value=Leaf existing; })
          | Branch existing ->
            let target_left = existing.key < id in
            let (mem, new_subtree') =
              del
                mem
                (Some
                  (if target_left
                   then existing.left
                   else existing.right))
                 id in
            match new_subtree' with
              | None ->
                  let (deleted, preserved) =
                    if target_left
                    then (existing.left, existing.right)
                    else (existing.right, existing.left) in
                  let mem = Mem.remove k mem in
                  (mem, Some(mem_get mem preserved))
              | Some pair ->
                  let new_branch =
                        if target_left
                        then Branch {
                               existing with
                               left = pair.ptr;
                               left_mutez = node_mutez pair.value;
                               left_height = node_height pair.value;
                             }
                        else Branch {
                               existing with
                               right = pair.ptr;
                               right_mutez = node_mutez pair.value;
                               right_height = node_height pair.value;
                             } in
                  let (mem, pair) = mem_set mem k new_branch in
                  match balance mem pair with (k, v) -> (k, Some(v))

let split (mem: mem) (root: ptr option) (mutez: int)
  : mem * ptr option * ptr option =
  failwith "not implemented"
