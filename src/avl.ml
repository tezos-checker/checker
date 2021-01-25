(* Current state
 *
 * The code is verbose, we should figure out some utilities to make it more
 * readable. (especially around setting left and right children of a branches
 * while maintaining the invariants, and we can implement more things in terms
 * of 'join')
 *
 * The invariants of the functions are sometimes not clear. Especially around
 * handling of the parent pointers; some functions overwrite the parent pointer
 * of the return value and expect the caller to set it properly, but some leave
 * it as is. This should be documented per function.
 *
 * Most of the times, when we set a value and return a pointer to it, the caller
 * immediately reads that location again. We can do this more efficiently by
 * returning the value of a pointer alongside with the pointer itself when
 * possible. We choose to skip the optimisation to keep the code clearer.
 *
 * The split function currently splits out "at most" given amount of tokens,
 * however the auction process requires us to split the next element if necessary.
 * This can be implemented by popping the smallest leftover, splitting and
 * re-inserting the pieces to both trees. And we might have some small
 * functions helping this, but it shouldn't be too hard to implement them.
 * For efficiency purposes, we might also end up writing a dedicated function.
 *
 * There are some amount of property tests on major code paths, but there
 * might be other issues.
 *
 * Also, currently all of the operations keep the tree balanced, but we might
 * not want to pay that cost for all operations. For example, when we split
 * a prefix, it might be okay if the new trees are left unbalanced.
*)

(* The AVL tree double-ended queue backed by a doubly-linked balanced
 * tree where the leaves contain the liquidation elements, and the
 * branches contain the amount of tez on their left and right children.
 *
 * It acts as a mutable data structure implemented on top of a bigmap
 * (just a Map in Ocaml prototype), acting as a "memory". So, the caller
 * should thread the memory across function calls. Returned objects from
 * AVL operations are only valid on the returned `mem` objects.
 *
 * The tree consists of three different nodes, where two of them, "root"
 * and "leaf" has dedicated pointer types called `avl_ptr` and `leaf_ptr`.
 * These pointers are "stable" references, where the operations on the
 * tree does not move them. (This is not the case with branches, where
 * any operation can create/update/delete any branches, so you can not
 * refer to them safely).
 *
 * This structure has an efficient (log(n)) `append` function (ergo,
 * `push_back` and `push_front` functions. Also, given a reference
 * to a leaf, it is possible to delete it `efficiently`.
*)

open Mem
open Ptr
open LiquidationAuctionTypes

let ptr_of_avl_ptr (ptr: avl_ptr) = match ptr with AVLPtr r -> r
let ptr_of_leaf_ptr (ptr: leaf_ptr) = match ptr with LeafPtr l -> l

let node_tez (n: node) : Ligo.tez =
  match n with
  | Leaf leaf -> leaf.value.tez
  | Branch branch -> Ligo.add_tez_tez branch.left_tez branch.right_tez
  | Root _ -> (failwith "node_tez found Root" : Ligo.tez)

let node_height (n: node) : Ligo.int =
  match n with
  | Leaf _ -> Ligo.int_from_literal "1"
  | Branch branch ->
    let max = if branch.left_height > branch.right_height
      then branch.left_height else branch.right_height in
    Ligo.add_int_int max (Ligo.int_from_literal "1")
  | Root _ -> (failwith "node_height found Root" : Ligo.int)

let node_parent (n: node) : ptr =
  match n with
  | Leaf leaf -> leaf.parent
  | Branch branch -> branch.parent
  | Root _ -> (failwith "node_parent found Root" : ptr)

let node_branch (n: node) : branch =
  match n with
  | Branch branch -> branch
  | Root _ -> (failwith "node_branch found Root" : branch)
  | Leaf _ -> (failwith "node_branch found Leaf" : branch)

let deref_avl_ptr (mem: mem) (p: avl_ptr): ptr option * auction_outcome option =
  let p = match p with AVLPtr p -> p in
  match mem_get mem p with
  | Root p -> p
  | Branch _ -> (failwith "deref_avl_ptr found Branch" : ptr option * auction_outcome option)
  | Leaf _ -> (failwith "deref_avl_ptr found Leaf" : ptr option * auction_outcome option)

let deref_leaf_ptr (mem: mem) (p: leaf_ptr): leaf =
  let p = match p with LeafPtr p -> p in
  match mem_get mem p with
  | Leaf l -> l
  | Branch _ -> (failwith "deref_leaf_ptr found Branch" : leaf)
  | Root _ -> (failwith "deref_leaf_ptr found Root" : leaf)


let node_left (n: node) : ptr =
  let b = node_branch n in b.left

let node_right (n: node) : ptr =
  let b = node_branch n in b.right

let node_set_parent (p: ptr) (n: node) : node =
  match n with
  | Leaf leaf -> Leaf { leaf with parent = p; }
  | Branch branch -> Branch { branch with parent = p; }
  | Root _ -> (failwith "node_set_parent found Root" : node)

let update_matching_child
    (mem: mem) (ptr: ptr) (from_ptr: ptr) (to_ptr: ptr) : mem =
  match mem_get mem ptr with
  | Root r ->
    (match r with
       (b, r) ->
       assert (b = Some from_ptr);
       mem_set mem ptr (Root ((Some to_ptr), r)))
  | Leaf _ ->
    (failwith "update_matching_child: got a leaf" : mem)
  | Branch old_branch ->
    let to_ = mem_get mem to_ptr in
    let new_branch =
      if old_branch.left = from_ptr
      then Branch {
          old_branch with
          left = to_ptr;
          left_tez = node_tez to_;
          left_height = node_height to_;
        }
      else (
        assert (old_branch.right = from_ptr);
        Branch {
          old_branch with
          right = to_ptr;
          right_tez = node_tez to_;
          right_height = node_height to_;
        }) in
    mem_set mem ptr new_branch

(*
 * Operations on AVL trees.
 *
 * The resulting 'ptr' and 'mem' should always be used together. They are
 * not as part of a product type, because a 'mem' can carry multiple trees,
 * in case of 'take'.
 *
 * The operations do not move leaves or the root, so pointers to them are stable
 * (unless a leaf is deleted with 'del').
 *
 * Implementation detail: The functions prefixed by `ref_` work on untyped
 * `ptr` pointers, and usually are only passed branches (not root). There
 * are usually wrappers around them which makes them work on `avl_ptr`'s
 * via doing the necessary unwrapping. The reason of this distinction is
 * that tree operations are usually called recursively and they don't have
 * a concept of a `Root` node.
 *)

let avl_mk_empty (mem: mem) (r: auction_outcome option): mem * avl_ptr =
  let (mem, ptr) = mem_new mem (Root ((None: ptr option), r)) in
  (mem, AVLPtr ptr)

(* Before:
 *
 *            curr
 *            /  \
 *           /    \
 *         left  right
 *               /   \
 *              /     \
 *         right_left  a
 *
 * After:
 *
 *            right
 *            /   \
 *           /     \
 *        curr      a
 *        /   \
 *       /     \
 *     left  right_left
*)
let ref_rotate_left (mem: mem) (curr_ptr: ptr) : mem * ptr =
  let curr =
    match mem_get mem curr_ptr with
    | Root _ -> (failwith "rotate_left: curr_ptr is Root" : branch)
    | Leaf _ -> (failwith "rotate_left: curr_ptr is Leaf" : branch)
    | Branch curr -> curr in

  let right_ptr = curr.right in
  let right =
    match mem_get mem right_ptr with
    | Root _ -> (failwith "rotate_left: right_ptr is Root" : branch)
    | Leaf _ -> (failwith "rotate_left: right_ptr is Leaf" : branch)
    | Branch right -> right in

  let right_left_ptr = right.left in

  (* move right_left under curr *)
  let mem = mem_update mem right_left_ptr (node_set_parent curr_ptr) in
  let mem = update_matching_child mem curr_ptr right_ptr right_left_ptr in

  (* move curr under right *)
  let mem = mem_update mem right_ptr (node_set_parent curr.parent) in
  let mem = mem_update mem curr_ptr (node_set_parent right_ptr) in
  let mem = update_matching_child mem right_ptr right_left_ptr curr_ptr in

  (mem, right_ptr)

(* Before:
 *
 *            curr
 *            /  \
 *           /    \
 *         left  right
 *         / \
 *        /   \
 *       a  left_right
 *
 * After:
 *
 *             left
 *             /  \
 *            /    \
 *           a    curr
 *               /    \
 *              /      \
 *        left_right   right
*)
let ref_rotate_right (mem: mem) (curr_ptr: ptr) : mem * ptr =
  let curr =
    match mem_get mem curr_ptr with
    | Root _ -> (failwith "rotate_right: curr_ptr is Root" : branch)
    | Leaf _ -> (failwith "rotate_right: curr_ptr is Leaf" : branch)
    | Branch curr -> curr in

  let left_ptr = curr.left in
  let left =
    match mem_get mem left_ptr with
    | Root _ -> (failwith "rotate_right: left_ptr is Root" : branch)
    | Leaf _ -> (failwith "rotate_right: curr_ptr is Leaf" : branch)
    | Branch left -> left in

  let left_right_ptr = left.right in

  (* move left_right under curr *)
  let mem = mem_update mem left_right_ptr (node_set_parent curr_ptr) in
  let mem = update_matching_child mem curr_ptr left_ptr left_right_ptr in

  (* move curr under left *)
  let mem = mem_update mem left_ptr (node_set_parent curr.parent) in
  let mem = mem_update mem curr_ptr (node_set_parent left_ptr) in
  let mem = update_matching_child mem left_ptr left_right_ptr curr_ptr in

  (mem, left_ptr)

(* From: https://en.wikipedia.org/wiki/Avl_tree#Rebalancing
 *
 * Dir1  | Dir2
 * ------+------
 * Left  | Left  => Z is a left  child of its parent X and Z is not right-heavy
 * Left  | Right => Z is a left  child of its parent X and Z is right-heavy
 * Right | Right => Z is a right child of its parent X and Z is not left-heavy
 * Right | Left  => Z is a right child of its parent X and Z is left-heavy
 *
 * The balance violation of case Dir1 = Dir2 is repaired by
 *   a simple rotation: rotate_(−Dir1)
 * The case Dir1 <> Dir2 is repaired by
 *   a double rotation: rotate_Dir1Dir2
*)
let rebalance (mem: mem) (curr_ptr: ptr) : mem * ptr =
  match mem_get mem curr_ptr with
  | Branch branch ->
    if Ligo.int (Ligo.abs (Ligo.sub_int_int branch.left_height branch.right_height)) > Ligo.int_from_literal "1" then (
      let diff = Ligo.sub_int_int branch.right_height branch.left_height in
      assert (Ligo.int (Ligo.abs diff) = Ligo.int_from_literal "2");

      let heavy_child_ptr =
        if Ligo.lt_int_int diff (Ligo.int_from_literal "0") then branch.left else branch.right in
      let heavy_child = match mem_get mem heavy_child_ptr with
        | Branch b -> b
        | Leaf _ -> (failwith "invariant violation: heavy_child should be a branch" : branch)
        | Root _ -> (failwith "invariant violation: heavy_child should be a branch" : branch) in
      let heavy_child_balance =
        Ligo.sub_int_int heavy_child.right_height heavy_child.left_height in

      let zero = Ligo.int_from_literal "0" in
      let (mem, ptr) = if Ligo.lt_int_int diff zero && Ligo.leq_int_int heavy_child_balance zero then
          (* Left, Left *)
          ref_rotate_right mem curr_ptr
        else if Ligo.lt_int_int diff zero && heavy_child_balance > zero then
          (* Left, Right *)
          let (mem, new_) = ref_rotate_left mem heavy_child_ptr in
          let mem = update_matching_child mem curr_ptr heavy_child_ptr new_ in
          ref_rotate_right mem curr_ptr
        else if Ligo.geq_int_int diff zero && heavy_child_balance >= zero then
          (* Right, Right*)
          ref_rotate_left mem curr_ptr
        else if Ligo.geq_int_int diff zero && heavy_child_balance < zero then
          (* Right, Left *)
          let (mem, new_) = ref_rotate_right mem heavy_child_ptr in
          let mem = update_matching_child mem curr_ptr heavy_child_ptr new_ in
          ref_rotate_left mem curr_ptr
        else
          (failwith "invariant violation: balance predicates partial" : mem * ptr) in
      assert (branch.parent = node_parent (mem_get mem ptr));
      (mem, ptr)
    ) else (mem, curr_ptr)
  | Leaf _ -> (mem, curr_ptr)
  | Root _ -> (mem, curr_ptr)

(* (match mem_get mem ptr with
   | Branch b -> assert (abs (b.left_height - b.right_height) <= 1); ()
   | Root _ -> ()
   | Leaf _ -> failwith "impossible"); *)

type direction =
  | Left
  | Right

(* ************************** *)

type ref_join_data = {
  join_direction: direction;
  ptr: ptr;
  to_fix: ptr;
  parent_ptr: ptr;
}

let ref_join_post_processing
    (data: ref_join_data)
    ((mem, new_child) : mem * ptr)
  : mem * ptr =
  let mem = update_matching_child mem data.ptr data.to_fix new_child in
  let (mem, new_tree) = rebalance mem data.ptr in
  let mem = mem_update mem new_tree (node_set_parent data.parent_ptr) in
  assert (node_parent (mem_get mem new_tree) = data.parent_ptr);
  (mem, new_tree)

(* Nice and tail-recursive left fold we can write in ligo more or less as-is. *)
let rec left_fold_ref_join_data
    ((mem_and_child_ptr, stack): (mem * ptr) * ref_join_data list)
  : mem * ptr =
  match stack with
  | [] -> mem_and_child_ptr
  | d :: ds ->
    let new_mem_and_child_ptr = ref_join_post_processing d mem_and_child_ptr in
    left_fold_ref_join_data (new_mem_and_child_ptr, ds)

(* Appends left_ptr and right_ptr to form a new tree, and returns a pointer to
 * the newly created tree. The resulting node will inherit the parent of the
 * "${join_direction}_ptr". *)
let rec ref_join_rec
    (mem, join_direction, left_ptr, right_ptr, stack: mem * direction * ptr * ptr * ref_join_data list)
  : mem * ptr =
  let left = mem_get mem left_ptr in
  let right = mem_get mem right_ptr in

  (* The given direction determines whose parent will be the parent of the
   * resulting tree. *)
  let parent_ptr = match join_direction with
    | Left -> node_parent left
    | Right -> node_parent right
  in

  (* If the left and right is of similar height, simply combining them
   * as a branch gives a valid AVL. *)
  if Ligo.lt_int_int
      (Ligo.int (Ligo.abs (Ligo.sub_int_int (node_height left) (node_height right))))
      (Ligo.int_from_literal "2") then
    let new_branch = Branch {
        left = left_ptr;
        left_height = node_height left;
        left_tez = node_tez left;
        right_tez = node_tez right;
        right_height = node_height right;
        right = right_ptr;
        parent = parent_ptr;
      } in

    let (mem, ptr) = mem_new mem new_branch in
    let mem = mem_update mem left_ptr (node_set_parent ptr) in
    let mem = mem_update mem right_ptr (node_set_parent ptr) in

    (* Do all the patching up here *)
    left_fold_ref_join_data ((mem, ptr), stack)
  else
    let new_join_direction, left_p, right_p, (ptr, to_fix) =
      (* If the left is heavier, we can make left the parent and append the
       * original right to left.right . *)
      if node_height left > node_height right then
        let left_p = node_right left in
        (Left, left_p, right_ptr, (left_ptr, left_p))
        (* Or vice versa. *)
      else (* node_height left < node_height right *)
        let right_p = node_left right in
        (Right, left_ptr, right_p, (right_ptr, right_p))
    in
    ref_join_rec
      ( mem
      , new_join_direction
      , left_p
      , right_p
      , ({ join_direction=join_direction; ptr=ptr; to_fix=to_fix; parent_ptr=parent_ptr; } :: stack)
      )

let ref_join
    (mem: mem)
    (join_direction: direction)
    (left_ptr: ptr)
    (right_ptr: ptr)
  : mem * ptr =
  ref_join_rec
    ( mem
    , join_direction
    , left_ptr
    , right_ptr
    , ([]: ref_join_data list)
    )

(* ************************** *)

let avl_push_back
    (mem: mem) (root_ptr: avl_ptr) (value: liquidation_slice)
  : mem * leaf_ptr =
  let root_ptr = match root_ptr with AVLPtr root_ptr -> root_ptr in
  let node = Leaf { value=value; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new mem node in
  match mem_get mem root_ptr with
  | Root root ->
    let (r, data) = root in
    (match r with
     (* When the tree is empty, create the initial leaf. *)
     | None ->
       let mem = mem_set mem root_ptr (Root (Some leaf_ptr, data)) in
       (mem, LeafPtr leaf_ptr)
     (* When there is already an element, join with the new leaf. *)
     | Some ptr ->
       let (mem, ret) = ref_join mem (Left) ptr leaf_ptr in
       let mem = mem_set mem root_ptr (Root (Some ret, data)) in
       (mem, LeafPtr leaf_ptr))
  | Branch _ ->
    (failwith "push_back is passed a non-root pointer." : mem * leaf_ptr)
  | Leaf _ ->
    (failwith "push_back is passed a non-root pointer." : mem * leaf_ptr)

(* The only implementation difference between this and push_back
 * is the order of parameters on 'join'. We should probably combine
 * these.
*)
let avl_push_front
    (mem: mem) (root_ptr: avl_ptr) (value: liquidation_slice)
  : mem * leaf_ptr =
  let (root, root_data) = deref_avl_ptr mem root_ptr in
  let root_ptr = match root_ptr with AVLPtr r -> r in

  let node = Leaf { value=value; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new mem node in

  begin match root with
    (* When the tree is empty, create the initial leaf. *)
    | None ->
      let mem = mem_set mem root_ptr (Root (Some leaf_ptr, root_data)) in
      (mem, LeafPtr leaf_ptr)
    (* When there is already an element, join with the new leaf. *)
    | Some r ->
      let (mem, ret) = ref_join mem (Right) leaf_ptr r in
      let mem = mem_set mem root_ptr (Root (Some ret, root_data)) in
      (mem, LeafPtr leaf_ptr)
  end

let rec balance_bottom_up ((mem, curr_ptr): mem * ptr): mem * avl_ptr =
  let curr = mem_get mem curr_ptr in
  match curr with
  | Root _ -> (mem, AVLPtr curr_ptr)
  | Leaf _ -> (failwith "impossible" : mem * avl_ptr)
  | Branch b ->
    (* TODO we can stop recursing up when node height does not change. *)
    let (mem, new_curr) = rebalance mem curr_ptr in
    assert (node_parent (mem_get mem new_curr) = b.parent);
    let mem = update_matching_child mem b.parent curr_ptr new_curr in
    balance_bottom_up (mem, b.parent)

(* Deletes a leaf pointer. Note that this does not require the tree root
 * to be passed. Returns the root of the tree as an extra information. *)
let ref_del (mem: mem) (ptr: ptr): mem * avl_ptr =
  let self = mem_get mem ptr in
  let parent_ptr = node_parent self in
  let mem = mem_del mem ptr in
  match mem_get mem parent_ptr with
  | Leaf _ -> (failwith "del: parent is a leaf" : mem * avl_ptr)
  (* when deleting the sole element, we return an empty tree *)
  | Root r ->
    let (_, m) = r in
    let mem = mem_set mem parent_ptr (Root ((None: ptr option), m)) in
    (mem, AVLPtr parent_ptr)
  (* otherwise, the parent of the deleted element is redundant since it
   * only has a single child, so we delete the parent and the orphan sibling
   * is adopted by the grandparent who have lost its child. *)
  | Branch parent ->
    let sibling_ptr = if parent.left = ptr
      then parent.right
      else parent.left in
    assert (sibling_ptr <> ptr);
    let mem = mem_del mem parent_ptr in
    let grandparent_ptr = parent.parent in
    let mem = mem_update mem sibling_ptr (node_set_parent grandparent_ptr) in
    let mem = update_matching_child
        mem
        grandparent_ptr
        parent_ptr
        sibling_ptr in
    balance_bottom_up (mem, grandparent_ptr)

let avl_del (mem: mem) (ptr: leaf_ptr): mem * avl_ptr =
  match ptr with LeafPtr ptr -> ref_del mem ptr

let avl_read_leaf (mem: mem) (ptr: leaf_ptr): liquidation_slice =
  let l = deref_leaf_ptr mem ptr in
  l.value

let avl_update_leaf (mem: mem) (ptr: leaf_ptr) (f: liquidation_slice -> liquidation_slice): mem =
  let l = deref_leaf_ptr mem ptr in
  let ptr = match ptr with LeafPtr p -> p in
  mem_set mem ptr (Leaf { l with value = f l.value })

let avl_is_empty (mem: mem) (ptr: avl_ptr) : bool =
  let (r, _) = deref_avl_ptr mem ptr in
  (match r with | None -> true | Some _ -> false)

(* BEGIN_OCAML *)
let rec ref_delete_tree (mem: mem) (ptrs: ptr list): mem =
  match ptrs with
  | [] -> mem
  | ptr :: ptrs ->
    let root = mem_get mem ptr in
    let mem = mem_del mem ptr in
    (match root with
     | Root (None, _) -> ref_delete_tree mem ptrs
     | Leaf _ -> ref_delete_tree mem ptrs
     | Root (Some p, _) -> ref_delete_tree mem (p :: ptrs)
     | Branch branch -> ref_delete_tree mem (branch.left :: branch.right :: ptrs))

let avl_delete_tree (mem: mem) (AVLPtr ptr): mem =
  ref_delete_tree mem [ptr]
(* END_OCAML *)

let avl_delete_empty_tree (mem: mem) (ptr: avl_ptr): mem =
  let (r, _) = deref_avl_ptr mem ptr in
  match r with
  | Some _ -> (failwith "tree not empty": mem)
  | None -> mem_del mem (ptr_of_avl_ptr ptr)

let avl_find_root (mem: mem) (leaf: leaf_ptr) : avl_ptr =
  let leaf = match leaf with LeafPtr p -> p in
  let rec go (ptr: ptr) : avl_ptr =
    match mem_get mem ptr with
    | Root _ -> AVLPtr ptr
    | Branch b -> go b.parent
    | Leaf l -> go l.parent in
  go leaf

let rec ref_peek_front (mem, ptr: mem * ptr) : leaf_ptr * leaf =
  let self = mem_get mem ptr in
  match self with
  | Leaf l -> (LeafPtr ptr, l)
  | Branch b -> ref_peek_front (mem, b.left)
  | Root _ -> (failwith "node is not leaf or branch" : leaf_ptr * leaf)

let avl_peek_front (mem: mem) (ptr: avl_ptr) : (leaf_ptr * leaf) option =
  let (p, _) = deref_avl_ptr mem ptr in
  match p with
  | None -> (None: (leaf_ptr * leaf) option)
  | Some r -> Some (ref_peek_front (mem, r))

(* FIXME: needs an efficient reimplementation *)
let avl_pop_front (mem: mem) (root_ptr: avl_ptr) : mem * liquidation_slice option =
  let (r, _) = deref_avl_ptr mem root_ptr in
  match r with
  | None -> (mem, (None: liquidation_slice option))
  | Some r ->
    let (leafptr, leaf) = ref_peek_front (mem, r) in
    let (mem, _) = avl_del mem leafptr in
    (mem, Some leaf.value)

(* ************************** *)

type ref_split_data = {
  rec_direction: direction;
  branch: branch;
}

let ref_split_post_processing
    (data : ref_split_data)
    ((mem, maybe_left, maybe_right) : mem * ptr option * ptr option)
  : mem * ptr option * ptr option =
  match data.rec_direction with
  | Left -> (
      match maybe_right with
      | None -> (failwith "impossible" : mem * ptr option * ptr option)
      | Some right ->
        let (mem, joined) = ref_join mem (Right) right data.branch.right in
        (mem, maybe_left, Some joined)
    )
  | Right -> (
      match maybe_left with
      | Some left ->
        let (mem, joined) = ref_join mem (Left) data.branch.left left in
        (mem, Some joined, maybe_right)
      | None ->
        (mem, Some data.branch.left, maybe_right)
    )

(* Nice and tail-recursive left fold we can write in ligo more or less as-is. *)
let rec left_fold_ref_split_data
    (mem_and_left_ptr_and_right_ptr, stack : (mem * ptr option * ptr option) * ref_split_data list)
  : mem * ptr option * ptr option =
  match stack with
  | [] -> mem_and_left_ptr_and_right_ptr
  | d :: ds ->
    let new_mem_and_left_ptr_and_right_ptr = ref_split_post_processing d mem_and_left_ptr_and_right_ptr in
    left_fold_ref_split_data (new_mem_and_left_ptr_and_right_ptr, ds)

(* George: This does not split leaves; if the tez on the leaf exceeds the limit
 * then it is not included in the result (it's part of the second tree
 * returned). Essentially this means that the union of the resulting trees has
 * the same contents as the input tree. *)
let rec ref_split_rec
    (mem, curr_ptr, limit, stack: mem * ptr * Ligo.tez * ref_split_data list)
  : mem * ptr option * ptr option =
  match mem_get mem curr_ptr with
  | Root _ -> (failwith "ref_split found Root" : mem * ptr option * ptr option)
  | Leaf leaf ->
    if leaf.value.tez <= limit
    then
      (* Case 1a. Single leaf with not too much tez in it. Include it. *)
      let mem = mem_update mem curr_ptr (node_set_parent ptr_null) in
      left_fold_ref_split_data ((mem, Some curr_ptr, (None: ptr option)), stack)
    else
      (* Case 1b. Single leaf with too much tez in it. Exclude it. *)
      left_fold_ref_split_data ((mem, (None: ptr option), Some curr_ptr), stack)
  | Branch branch ->
    if Ligo.add_tez_tez branch.left_tez branch.right_tez <= limit
    then (* total_tez <= limit *)
      (* Case 2. The whole tree has not too much tez in it. Include it. *)
      let mem = mem_update mem curr_ptr (node_set_parent ptr_null) in
      left_fold_ref_split_data ((mem, Some curr_ptr, (None: ptr option)), stack)
    else (* limit < total_tez *)
      let mem = mem_del mem curr_ptr in
      let mem = mem_update mem branch.right (node_set_parent branch.parent) in
      (* Semantically it would be better to detach branch.left as well here
       *
       *   let mem = mem_update mem branch.left (node_set_parent ptr_null) in
       *
       * instead of changing the parent of branch.left in function "take" below.
       * Unfortunately, this bumps reads and writes significantly (reads+=16%
       * and writes+=20%), so we don't do it. *)

      if branch.left_tez = limit
      then (* Case 3a. left_tez = limit (no need for recursion, split the tree right here) *)
        left_fold_ref_split_data ((mem, Some branch.left, Some branch.right), stack)
      else
        let rec_direction, tree_to_recurse_into, limit_to_use =
          if limit < branch.left_tez
          then (* Case 3b. limit < left_tez < total_tez (we have to recurse into and split the left tree) *)
            (Left, branch.left, limit)
          else (* Case 3c. left_tez < limit < total_tez (we have to recurse into and split the right tree) *)
            let left_branch = mem_get mem branch.left in
            (Right, branch.right, Ligo.sub_tez_tez limit (node_tez left_branch))
        in
        ref_split_rec
          ( mem
          , tree_to_recurse_into
          , limit_to_use
          , ({ rec_direction=rec_direction; branch=branch } :: stack)
          )

let ref_split
    (mem: mem)
    (curr_ptr: ptr)
    (limit: Ligo.tez)
  : mem * ptr option * ptr option =
  ref_split_rec
    ( mem
    , curr_ptr
    , limit
    , ([]: ref_split_data list)
    )

(* ************************** *)

(* Split the longest prefix of the tree with less than
 * given amount of tez.
*)
let avl_take (mem: mem) (root_ptr: avl_ptr) (limit: Ligo.tez) (root_data: auction_outcome option)
  : mem * avl_ptr =
  let (r, old_root_data) = deref_avl_ptr mem root_ptr  in
  let root_ptr = match root_ptr with AVLPtr r -> r in
  match r with
  | Some r ->
    let (mem, l, r) = ref_split mem r limit in
    let (mem, new_root) = mem_new mem (Root (l, root_data)) in
    let mem = match l with
      | Some l -> mem_update mem l (node_set_parent new_root)
      | None -> mem in
    let mem = mem_set mem root_ptr (Root (r, old_root_data)) in
    (mem, AVLPtr new_root)
  | None ->
    let (mem, new_root) = mem_new mem (Root ((None: ptr option), root_data)) in
    (mem, AVLPtr new_root)

let avl_tez (mem: mem) (ptr: avl_ptr) : Ligo.tez =
  let (r, _) = deref_avl_ptr mem ptr in
  match r with
  | Some ptr -> node_tez (mem_get mem ptr)
  | None -> Ligo.tez_from_literal "0mutez"

let avl_height (mem: mem) (ptr: avl_ptr): Ligo.int =
  let (r, _) = deref_avl_ptr mem ptr in
  match r with
  | Some ptr -> node_height (mem_get mem ptr)
  | None -> Ligo.int_from_literal "0"

let avl_root_data (mem: mem) (ptr: avl_ptr) : auction_outcome option =
  let (_, d) = deref_avl_ptr mem ptr in d

let avl_modify_root_data (mem: mem) (ptr: avl_ptr) (f: auction_outcome option -> auction_outcome option) =
  let (r, d) = deref_avl_ptr mem ptr in
  let ptr = match ptr with AVLPtr p -> p in
  mem_set mem ptr (Root (r, f d))


(* BEGIN_OCAML *)

(* This is not going to be used in the final implementation, but it allows
 * testing some useful properties (mainly about `join` function).
*)
let avl_append (mem: mem) (AVLPtr left_ptr) (AVLPtr right_ptr): mem =
  let mem = match (mem_get mem left_ptr, mem_get mem right_ptr) with
    | (Root _, Root (None, _)) ->
      mem
    | (Root (None, d), Root (Some r, _)) ->
      let mem = mem_set mem left_ptr (Root (Some r, d)) in
      mem_update mem r (node_set_parent left_ptr)
    | (Root (Some l, d), Root (Some r, _)) ->
      let (mem, l) = ref_join mem (Left) l r in
      mem_set mem left_ptr (Root (Some l, d))
    | _ ->
      (failwith "avlptr is not root" : mem) in
  mem_del mem right_ptr

let debug_mem (mem: mem) : unit =
  List.iter
    (fun (k, v) ->
       Format.printf
         "%s -> %s\n"
         (Ptr.show k)
         (show_node v);
    )
    (Ligo.Big_map.bindings mem.mem)

let avl_assert_invariants (mem: mem) (AVLPtr root) : unit =
  let rec go (parent: ptr) (curr: ptr) =
    match mem_get mem curr with
    | Root _ ->
      (failwith "assert_invariants: tree root in unexpected location." : unit)
    | Leaf leaf ->
      assert (leaf.parent = parent)
    | Branch branch ->
      let left = mem_get mem branch.left in
      let right = mem_get mem branch.right in
      assert (branch.parent = parent);
      assert (branch.left_height = node_height left);
      assert (branch.left_tez = node_tez left);
      assert (branch.right_height = node_height right);
      assert (branch.right_tez = node_tez right);
      assert (Ligo.lt_int_int (Ligo.int (Ligo.abs (Ligo.sub_int_int branch.left_height branch.right_height))) (Ligo.int_from_literal "2"));
      go curr branch.left;
      go curr branch.right
  in match mem_get mem root with
  | Root (None, _) -> ()
  | Root (Some r, _) -> go root r
  | _ -> (failwith "assert_invariants needs a root." : unit)

let avl_assert_dangling_pointers (mem: mem) (roots: avl_ptr list) : unit =
  let mem = List.fold_left avl_delete_tree mem roots in
  assert (List.length (Ligo.Big_map.bindings mem.mem) = 0)

(* END_OCAML *)

