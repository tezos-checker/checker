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

open BigMap

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

type avl_ptr = AVLPtr of ptr
[@@deriving show]

let ptr_of_avl_ptr (AVLPtr ptr) = ptr

type leaf_ptr = LeafPtr of ptr
[@@deriving show]

let ptr_of_leaf_ptr (LeafPtr ptr) = ptr

type 't leaf = {
  value: 't;
  tez: Tez.t;
  parent: ptr;
}
[@@deriving show]

type branch = {
  left: ptr;
  left_height: int;
  left_tez: Tez.t;
  right_tez: Tez.t;
  right_height: int;
  right: ptr;
  parent: ptr;
}
[@@deriving show]

type 't node =
  | Leaf of 't leaf
  | Branch of branch
  | Root of ptr option
[@@deriving show]

type 't mem = ('t node) BigMap.t

let node_tez n =
  match n with
  | Leaf leaf -> leaf.tez
  | Branch branch -> Tez.(branch.left_tez + branch.right_tez)
  | Root _ -> failwith "node_tez found Root"

let node_height n =
  match n with
  | Leaf _ -> 1
  | Branch branch -> max branch.left_height branch.right_height + 1
  | Root _ -> failwith "node_height found Root"

let node_parent n =
  match n with
  | Leaf leaf -> leaf.parent
  | Branch branch -> branch.parent
  | Root _ -> failwith "node_parent found Root"

let node_set_parent (p: ptr) (n: 't node) =
  match n with
  | Leaf leaf -> Leaf { leaf with parent = p; }
  | Branch branch -> Branch { branch with parent = p; }
  | Root _ -> failwith "node_set_parent found Root"

let update_matching_child
    (mem: 't mem) (ptr: ptr) (from_ptr: ptr) (to_ptr: ptr) : 't mem =
  match mem_get mem ptr with
  | Root b ->
    assert (b = Some from_ptr);
    mem_set mem ptr (Root (Some to_ptr))
  | Leaf _ ->
    failwith "update_matching_child: got a leaf"
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

let mk_empty (mem: 't mem): 't mem * avl_ptr =
  let (mem, ptr) = mem_new mem (Root None) in
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
let ref_rotate_left (mem: 't mem) (curr_ptr: ptr) : 't mem * ptr =
  let curr =
    match mem_get mem curr_ptr with
    | Root _ -> failwith "rotate_left: curr_ptr is Root"
    | Leaf _ -> failwith "rotate_left: curr_ptr is Leaf"
    | Branch curr -> curr in

  let right_ptr = curr.right in
  let right =
    match mem_get mem right_ptr with
    | Root _ -> failwith "rotate_left: right_ptr is Root"
    | Leaf _ -> failwith "rotate_left: right_ptr is Leaf"
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
let ref_rotate_right (mem: 't mem) (curr_ptr: ptr) : 't mem * ptr =
  let curr =
    match mem_get mem curr_ptr with
    | Root _ -> failwith "rotate_right: curr_ptr is Root"
    | Leaf _ -> failwith "rotate_right: curr_ptr is Leaf"
    | Branch curr -> curr in

  let left_ptr = curr.left in
  let left =
    match mem_get mem left_ptr with
    | Root _ -> failwith "rotate_right: left_ptr is Root"
    | Leaf _ -> failwith "rotate_right: curr_ptr is Leaf"
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
let balance (mem: 't mem) (curr_ptr: ptr) : 't mem * ptr =
  match mem_get mem curr_ptr with
  | Branch branch
    when abs (branch.left_height - branch.right_height) > 1 ->

    let balance = branch.right_height - branch.left_height in
    assert (abs balance = 2);

    let heavy_child_ptr =
      if balance < 0 then branch.left else branch.right in
    let heavy_child = match mem_get mem heavy_child_ptr with
      | Branch b -> b
      | _ -> failwith "invariant violation: heavy_child should be a branch" in
    let heavy_child_balance =
      heavy_child.right_height - heavy_child.left_height in

    if balance < 0 && heavy_child_balance <= 0 then
      (* Left, Left *)
      ref_rotate_right mem curr_ptr
    else if balance < 0 && heavy_child_balance > 0 then
      (* Left, Right *)
      let (mem, new_) = ref_rotate_left mem heavy_child_ptr in
      let mem = update_matching_child mem curr_ptr heavy_child_ptr new_ in
      ref_rotate_right mem curr_ptr
    else if balance > 0 && heavy_child_balance >= 0 then
      (* Right, Right*)
      ref_rotate_left mem curr_ptr
    else if balance > 0 && heavy_child_balance < 0 then
      (* Right, Left *)
      let (mem, new_) = ref_rotate_right mem heavy_child_ptr in
      let mem = update_matching_child mem curr_ptr heavy_child_ptr new_ in
      ref_rotate_left mem curr_ptr
    else
      failwith "invariant violation: balance predicates partial"
  | _ -> (mem, curr_ptr)
(* (match mem_get mem ptr with
   | Branch b -> assert (abs (b.left_height - b.right_height) <= 1); ()
   | Root _ -> ()
   | Leaf _ -> failwith "impossible"); *)

type join_direction =
  | Left
  | Right

let rec ref_join
    (mem: 't mem) (direction: join_direction)
    (left_ptr: ptr) (right_ptr: ptr) : 't mem * ptr =
  let left = mem_get mem left_ptr in
  let right = mem_get mem right_ptr in

  let focused = match direction with | Left -> left | Right -> right in
  let parent_ptr = node_parent focused in

  if abs (node_height left - node_height right) < 2 then
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
    (mem, ptr)
  else if node_height left > node_height right then
    let left = match left with Branch b -> b | _ -> failwith "impossible" in
    let (mem, new_) = ref_join mem Left left.right right_ptr in
    let mem = update_matching_child mem left_ptr left.right new_ in
    let (mem, new_) = balance mem left_ptr in
    (mem, new_)
  else (* node_height left < node_height right *)
    let right = match right with Branch b -> b | _ -> failwith "impossible" in
    let (mem, new_) = ref_join mem Right left_ptr right.left in
    let mem = update_matching_child mem right_ptr right.left new_ in
    let (mem, new_) = balance mem right_ptr in
    let mem = mem_update mem new_ (node_set_parent parent_ptr) in
    (mem, new_)

let push_back
    (mem: 't mem) (AVLPtr root_ptr) (value: 't) (tez: 'tez)
  : 't mem * leaf_ptr =
  let node = Leaf { value=value; tez=tez; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new mem node in
  match mem_get mem root_ptr with
  (* When the tree is empty, create the initial leaf. *)
  | Root None ->
    let mem = mem_set mem root_ptr (Root (Some leaf_ptr)) in
    (mem, LeafPtr leaf_ptr)
  (* When there is already an element, join with the new leaf. *)
  | Root (Some r) ->
    let (mem, ret) = ref_join mem Left r leaf_ptr in
    let mem = mem_set mem root_ptr (Root (Some ret)) in
    (mem, LeafPtr leaf_ptr)
  | _ ->
    failwith "push_back is passed a non-root pointer."

let append (mem: 't mem) (AVLPtr left_ptr) (AVLPtr right_ptr): 't mem =
  let mem = match (mem_get mem left_ptr, mem_get mem right_ptr) with
    | (Root _, Root None) ->
      mem
    | (Root None, Root (Some r)) ->
      let mem = mem_set mem left_ptr (Root (Some r)) in
      mem_update mem r (node_set_parent left_ptr)
    | (Root (Some l), Root (Some r)) ->
      let (mem, l) = ref_join mem Left l r in
      mem_set mem left_ptr (Root (Some l))
    | _ ->
      failwith "avlptr is not root" in
  mem_del mem right_ptr

(* The only implementation difference between this and push_back
 * is the order of parameters on 'join'. We should probably combine
 * these.
*)
let push_front
    (mem: 't mem) (AVLPtr root_ptr) (value: 't) (tez: 'tez)
  : 't mem * leaf_ptr =
  let node = Leaf { value=value; tez=tez; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new mem node in
  match mem_get mem root_ptr with
  (* When the tree is empty, create the initial leaf. *)
  | Root None ->
    let mem = mem_set mem root_ptr (Root (Some leaf_ptr)) in
    (mem, LeafPtr leaf_ptr)
  (* When there is already an element, join with the new leaf. *)
  | Root (Some r) ->
    let (mem, ret) = ref_join mem Right leaf_ptr r in
    let mem = mem_set mem root_ptr (Root (Some ret)) in
    (mem, LeafPtr leaf_ptr)
  | _ ->
    failwith "push_front is passed a non-root pointer."

let ref_del (mem: 't mem) (ptr: ptr): 't mem =
  let self = mem_get mem ptr in
  let parent_ptr = node_parent self in
  let mem = mem_del mem ptr in
  match mem_get mem parent_ptr with
  | Leaf _ -> failwith "del: parent is a leaf"
  (* when deleting the sole element, we return an empty tree *)
  | Root _ ->
    mem_set mem parent_ptr (Root None)
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
    let rec balance_bottom_up mem curr_ptr =
      let curr = mem_get mem curr_ptr in
      match curr with
      | Root _ -> mem
      | Leaf _ -> failwith "impossible"
      | Branch b ->
        let (mem, new_curr) = balance mem curr_ptr in
        assert (node_parent (mem_get mem new_curr) = b.parent);
        let mem = update_matching_child mem b.parent curr_ptr new_curr in
        balance_bottom_up mem b.parent in
    balance_bottom_up mem grandparent_ptr

let del (mem: 't mem) (LeafPtr ptr): 't mem = ref_del mem ptr

let read_leaf (mem: 't mem) (LeafPtr ptr): 't * Tez.t =
  match mem_get mem ptr with
  | Leaf l -> (l.value, l.tez)
  | _ -> failwith "read_leaf: leaf_ptr does not point to a leaf"

let update_leaf (mem: 't mem) (LeafPtr ptr) (f: 't -> 't): 't mem =
  match mem_get mem ptr with
  | Leaf l ->
    mem_set mem ptr (Leaf { l with value = f l.value })
  | _ -> failwith "read_leaf: leaf_ptr does not point to a leaf"

let is_empty (mem: 't mem) (AVLPtr ptr) : bool =
  match mem_get mem ptr with
  | Root None -> true
  | Root (Some _) -> false
  | _ -> failwith "is_empty: avl_ptr does not point to a Root"

let rec ref_delete_tree (mem: 't mem) (ptr: ptr): 't mem =
  let root = mem_get mem ptr in
  let mem = mem_del mem ptr in
  match root with
  | Root None -> mem
  | Leaf _ -> mem
  | Root (Some p) -> ref_delete_tree mem p
  | Branch branch ->
    let mem = ref_delete_tree mem branch.left in
    let mem = ref_delete_tree mem branch.right in
    mem

let delete_tree (mem: 't mem) (AVLPtr ptr): 't mem =
  ref_delete_tree mem ptr

let find_root (mem: 't mem) (LeafPtr leaf) : avl_ptr =
  let rec go (ptr: ptr) : avl_ptr =
    match mem_get mem ptr with
    | Root _ -> AVLPtr ptr
    | Branch b -> go b.parent
    | Leaf l -> go l.parent in
  go leaf

let rec ref_peek_front (mem: 't mem) (ptr: ptr) : leaf_ptr =
  let self = mem_get mem ptr in
  match self with
  | Leaf _ -> LeafPtr ptr
  | Branch b -> ref_peek_front mem b.left
  | _ -> failwith "node is not leaf or branch"

(* FIXME: needs an efficient reimplementation *)
let pop_front (mem: 't mem) (AVLPtr root_ptr) : 't mem * 't option =
  match mem_get mem root_ptr with
  | Root None -> (mem, None)
  | Root (Some r) ->
    let leafptr = ref_peek_front mem r in
    let (x, _) = read_leaf mem leafptr in
    let mem = del mem leafptr in
    (mem, Some x)
  | _ -> failwith "pop_front: avl_ptr does not point to a Root"

let rec ref_split (mem: 't mem) (curr_ptr: ptr) (limit: Tez.t)
  : 't mem * ptr option * ptr option =
  match mem_get mem curr_ptr with
  | Root _ -> failwith "ref_split found Root"
  | Leaf leaf ->
    if leaf.tez <= limit
    then
      let mem = mem_update mem curr_ptr (node_set_parent Ptr.null) in
      (mem, Some curr_ptr, None)
    else
      (mem, None, Some curr_ptr)
  | Branch branch ->
    if Tez.(branch.left_tez + branch.right_tez) <= limit
    then (* total_tez <= limit *)
      let mem = mem_update mem curr_ptr (node_set_parent Ptr.null) in
      (mem, Some curr_ptr, None)
    else
      let mem = mem_del mem curr_ptr in
      let mem = mem_update mem branch.right (node_set_parent branch.parent) in

      if branch.left_tez = limit
      then (* left_tez = limit *)
        (mem, Some branch.left, Some branch.right)

      else if limit < branch.left_tez
      then (* limit < left_tez < total_tez *)
        match ref_split mem branch.left limit with
        | (_, _, None) -> failwith "impossible"
        | (mem, left, Some right) ->
          let (mem, joined) = ref_join mem Right right branch.right in
          (mem, left, Some joined)

      else (* left_tez < limit < total_tez *)
        let left = mem_get mem branch.left in
        match ref_split mem branch.right Tez.(limit - (node_tez left)) with
        | (mem, Some left, right) ->
          let (mem, joined) = ref_join mem Left branch.left left in
          let mem =
            match right with
            | None -> mem
            | Some r -> mem_update mem r (node_set_parent branch.parent) in
          (mem, Some joined, right)
        | (mem, None, right) ->
          let mem = mem_update mem branch.left (node_set_parent Ptr.null) in
          (mem, Some branch.left, right)

(* Split the longest prefix of the tree with less than
 * given amount of tez.
*)
let take (mem: 't mem) (AVLPtr root_ptr) (limit: Tez.t)
  : 't mem * avl_ptr =
  match mem_get mem root_ptr with
  | Root (Some r) ->
    let (mem, l, r) = ref_split mem r limit in
    let (mem, new_root) = mem_new mem (Root l) in
    let mem = match l with
      | Some l -> mem_update mem l (node_set_parent new_root)
      | None -> mem in
    let mem = mem_set mem root_ptr (Root r) in
    (mem, AVLPtr new_root)
  | Root None ->
    let (mem, new_root) = mem_new mem (Root None) in
    (mem, AVLPtr new_root)
  | _ -> failwith "invariant violation: avl_ptr does not point to a Root"

let avl_tez (mem: 't mem) (AVLPtr ptr) =
  match mem_get mem ptr with
  | Root (Some ptr) -> node_tez (mem_get mem ptr)
  | Root None -> Tez.zero
  | _ -> failwith "impossible"
