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

type ptr = Ptr of int
let null_ptr = Ptr 0

type leaf_ptr = LeafPtr of ptr
type avl_ptr = AVLPtr of ptr

let ptr_of_avl_ptr (ptr: avl_ptr) = match ptr with AVLPtr r -> r
let ptr_of_leaf_ptr (ptr: leaf_ptr) = match ptr with LeafPtr l -> l

[@inline]
let int_of_ptr (p: ptr) =
  match p with Ptr i -> i
[@inline]
let ptr_eq (p1, p2 : ptr * ptr) =
  int_of_ptr p1 = int_of_ptr p2

type leaf = {
  value: int;
  tez: tez;
  parent: ptr;
}


(* TODO Instead of storing left_height and right_height, we could just
 * store the sum type LeftHeavy | Balanced | RightHeavy. However, I think
 * we might want to leave some trees unbalanced, and I think this approach
 * might work better in that case. *)
type branch = {
  left: ptr;
  left_height: int;
  left_tez: tez;
  right_tez: tez;
  right_height: int;
  right: ptr;
  parent: ptr;
}


type node =
  | Leaf of leaf
  | Branch of branch
  | Root of (ptr option * int)

type mem = {
  mem: (ptr, node) big_map;
  max_id: int;
}

[@inline]
let mem_new (mem, value : mem * node) : mem * ptr =
  let new_id = mem.max_id + 1 in
  let new_mem = {
        mem = Big_map.update (Ptr new_id) (Some value) mem.mem;
        max_id = new_id;
      } in
  (new_mem, Ptr new_id)

[@inline]
let mem_get (mem , ptr: mem * ptr) : node =
  Big_map.find ptr mem.mem

[@inline]
let mem_del (mem , ptr : mem * ptr) : mem =
  { mem = Big_map.update ptr (None: node option) mem.mem;
    max_id = mem.max_id
  }

let mem_set (mem, ptr, value : mem * ptr * node) : mem =
  let ign = assert (int_of_ptr ptr <= mem.max_id) in
  { mem = Big_map.update ptr (Some value) mem.mem;
    max_id = mem.max_id
  }

let mem_update (m, k, f : mem * ptr * (node -> node)) : mem =
  mem_set (m, k, (f (mem_get (m, k))))

[@inline]
let max (i: int) (j: int) =
  if i >= j then i else j

let node_tez (n: node) : tez =
  match n with
  | Leaf leaf -> leaf.tez
  | Branch branch -> branch.left_tez + branch.right_tez
  | Root ignored -> (failwith "node_tez found Root" : tez)

let node_height (n: node) : int =
  match n with
  | Leaf ignored -> 1
  | Branch branch -> max branch.left_height branch.right_height + 1
  | Root ignored -> (failwith "node_height found Root" : int)

let node_parent (n: node) : ptr =
  match n with
  | Leaf leaf -> leaf.parent
  | Branch branch -> branch.parent
  | Root ignored -> (failwith "node_parent found Root" : ptr)

let node_branch (n: node) : branch =
  match n with
  | Branch branch -> branch
  | Root ignored -> (failwith "node_branch found Root" : branch)
  | Leaf ignored -> (failwith "node_branch found Leaf" : branch)

let node_left (n: node) : ptr =
  let b = node_branch n in b.left

let node_right (n: node) : ptr =
  let b = node_branch n in b.right

let node_set_parent (p: ptr) (n: node) : node =
  match n with
  | Leaf leaf -> Leaf { leaf with parent = p; }
  | Branch branch -> Branch { branch with parent = p; }
  | Root ignored -> (failwith "node_set_parent found Root" : node)

let update_matching_child
    (mem, ptr, from_ptr, to_ptr: mem * ptr * ptr * ptr) : mem =
  match mem_get (mem, ptr) with
  | Root r ->
    (match r with
       (b, r) ->
       mem_set (mem, ptr, (Root ((Some to_ptr), r))))
  | Leaf ignored ->
    (failwith "update_matching_child: got a leaf" : mem)
  | Branch old_branch ->
    let to_ = mem_get (mem, to_ptr) in
    let new_branch =
      if ptr_eq (old_branch.left, from_ptr)
      then Branch {
          old_branch with
          left = to_ptr;
          left_tez = node_tez to_;
          left_height = node_height to_;
        }
      else (
        Branch {
          old_branch with
          right = to_ptr;
          right_tez = node_tez to_;
          right_height = node_height to_;
        }) in
    mem_set (mem, ptr, new_branch)

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
let ref_rotate_left (mem, curr_ptr: mem * ptr) : mem * ptr =
  let curr =
    match mem_get (mem, curr_ptr) with
    | Root ignored -> (failwith "rotate_left: curr_ptr is Root" : branch)
    | Leaf ignored -> (failwith "rotate_left: curr_ptr is Leaf" : branch)
    | Branch curr -> curr in

  let right_ptr = curr.right in
  let right =
    match mem_get (mem, right_ptr) with
    | Root ignored -> (failwith "rotate_left: right_ptr is Root" : branch)
    | Leaf ignored -> (failwith "rotate_left: right_ptr is Leaf" : branch)
    | Branch right -> right in

  let right_left_ptr = right.left in

  (* move right_left under curr *)
  let mem = mem_update (mem, right_left_ptr, (node_set_parent curr_ptr)) in
  let mem = update_matching_child (mem, curr_ptr, right_ptr, right_left_ptr) in

  (* move curr under right *)
  let mem = mem_update (mem, right_ptr, (node_set_parent curr.parent)) in
  let mem = mem_update (mem, curr_ptr, (node_set_parent right_ptr)) in
  let mem = update_matching_child (mem, right_ptr, right_left_ptr, curr_ptr) in

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
let ref_rotate_right (mem, curr_ptr: mem * ptr) : mem * ptr =
  let curr =
    match mem_get (mem, curr_ptr) with
    | Root ignored -> (failwith "rotate_right: curr_ptr is Root" : branch)
    | Leaf ignored -> (failwith "rotate_right: curr_ptr is Leaf" : branch)
    | Branch curr -> curr in

  let left_ptr = curr.left in
  let left =
    match mem_get (mem, left_ptr) with
    | Root ignored -> (failwith "rotate_right: left_ptr is Root" : branch)
    | Leaf ignored -> (failwith "rotate_right: curr_ptr is Leaf" : branch)
    | Branch left -> left in

  let left_right_ptr = left.right in

  (* move left_right under curr *)
  let mem = mem_update (mem, left_right_ptr, (node_set_parent curr_ptr)) in
  let mem = update_matching_child (mem, curr_ptr, left_ptr, left_right_ptr) in

  (* move curr under left *)
  let mem = mem_update (mem, left_ptr, (node_set_parent curr.parent)) in
  let mem = mem_update (mem, curr_ptr, (node_set_parent left_ptr)) in
  let mem = update_matching_child (mem, left_ptr, left_right_ptr, curr_ptr) in

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
let rebalance (mem, curr_ptr: mem * ptr) : mem * ptr =
  match mem_get (mem, curr_ptr) with
  | Branch branch ->
    if abs (branch.left_height - branch.right_height) > 1n then (
      let diff = branch.right_height - branch.left_height in

      let heavy_child_ptr =
        if diff < 0 then branch.left else branch.right in
      let heavy_child = match mem_get (mem, heavy_child_ptr) with
        | Branch b -> b
        | Leaf ignored -> (failwith "invariant violation: heavy_child should be a branch" : branch)
        | Root ignored -> (failwith "invariant violation: heavy_child should be a branch" : branch) in
      let heavy_child_balance =
        heavy_child.right_height - heavy_child.left_height in

      let (mem, ptr) = if diff < 0 && heavy_child_balance <= 0 then
          (* Left, Left *)
          ref_rotate_right (mem, curr_ptr)
        else if diff < 0 && heavy_child_balance > 0 then
          (* Left, Right *)
          let (mem, new_) = ref_rotate_left (mem, heavy_child_ptr) in
          let mem = update_matching_child (mem, curr_ptr, heavy_child_ptr, new_) in
          ref_rotate_right (mem, curr_ptr)
        else if diff > 0 && heavy_child_balance >= 0 then
          (* Right, Right*)
          ref_rotate_left (mem, curr_ptr)
        else if diff > 0 && heavy_child_balance < 0 then
          (* Right, Left *)
          let (mem, new_) = ref_rotate_right (mem, heavy_child_ptr) in
          let mem = update_matching_child (mem, curr_ptr, heavy_child_ptr, new_) in
          ref_rotate_left (mem, curr_ptr)
        else
          (failwith "invariant violation: balance predicates partial" : mem * ptr) in
      (mem, ptr)
    ) else (mem, curr_ptr)
  | Leaf ignored -> (mem, curr_ptr)
  | Root ignored -> (mem, curr_ptr)

(* (match mem_get mem ptr with
   | Branch b -> assert (abs (b.left_height - b.right_height) <= 1); ()
   | Root ignored -> ()
   | Leaf ignored -> failwith "impossible"); *)

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
  let mem = update_matching_child (mem, data.ptr, data.to_fix, new_child) in
  let (mem, new_tree) = rebalance (mem, data.ptr) in
  let mem = mem_update (mem, new_tree, node_set_parent data.parent_ptr) in
  (mem, new_tree)

(* Nice and tail-recursive left fold we can write in ligo more or less as-is. *)
let rec left_fold_ref_join_data
    (mem_and_child_ptr, stack : (mem * ptr) * ref_join_data list)
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
  let left = mem_get (mem, left_ptr) in
  let right = mem_get (mem, right_ptr) in

  (* The given direction determines whose parent will be the parent of the
   * resulting tree. *)
  let parent_ptr = match join_direction with
    | Left -> node_parent left
    | Right -> node_parent right
  in

  (* If the left and right is of similar height, simply combining them
   * as a branch gives a valid AVL. *)
  if abs (node_height left - node_height right) < 2n then
    let new_branch = Branch {
        left = left_ptr;
        left_height = node_height left;
        left_tez = node_tez left;
        right_tez = node_tez right;
        right_height = node_height right;
        right = right_ptr;
        parent = parent_ptr;
      } in

    let (mem, ptr) = mem_new (mem, new_branch) in
    let mem = mem_update (mem, left_ptr, node_set_parent ptr) in
    let mem = mem_update (mem, right_ptr, node_set_parent ptr) in

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
    (mem, join_direction, left_ptr, right_ptr: mem * direction * ptr * ptr)
  : mem * ptr =
  ref_join_rec
    ( mem
    , join_direction
    , left_ptr
    , right_ptr
    , ([]: ref_join_data list)
    )

(* ************************** *)

let push_back
    (mem, root_ptr, value, tez: mem * avl_ptr * int * tez)
  : mem * leaf_ptr =
  let root_ptr = match root_ptr with AVLPtr root_ptr -> root_ptr in
  let node = Leaf { value=value; tez=tez; parent=root_ptr; } in
  let (mem, leaf_ptr) = mem_new (mem, node) in
  match mem_get (mem, root_ptr) with
  | Root root ->
    let (r, data) = root in
    (match r with
     (* When the tree is empty, create the initial leaf. *)
     | None ->
       let mem = mem_set (mem, root_ptr, (Root (Some leaf_ptr, data))) in
       (mem, LeafPtr leaf_ptr)
     (* When there is already an element, join with the new leaf. *)
     | Some ptr ->
       let (mem, ret) = ref_join (mem, (Left), ptr, leaf_ptr) in
       let mem = mem_set (mem, root_ptr, (Root (Some ret, data))) in
       (mem, LeafPtr leaf_ptr))
  | Branch ignored ->
    (failwith "push_back is passed a non-root pointer." : mem * leaf_ptr)
  | Leaf ignored ->
    (failwith "push_back is passed a non-root pointer." : mem * leaf_ptr)

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
        let (mem, joined) = ref_join (mem, (Right), right, data.branch.right) in
        (mem, maybe_left, Some joined)
    )
  | Right -> (
      match maybe_left with
      | Some left ->
        let (mem, joined) = ref_join (mem, (Left), data.branch.left, left) in
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
    (mem, curr_ptr, limit, stack: mem * ptr * tez * ref_split_data list)
  : mem * ptr option * ptr option =
  match mem_get (mem, curr_ptr) with
  | Root ignored -> (failwith "ref_split found Root" : mem * ptr option * ptr option)
  | Leaf leaf ->
    if leaf.tez <= limit
    then
      (* Case 1a. Single leaf with not too much tez in it. Include it. *)
      let mem = mem_update (mem, curr_ptr, node_set_parent null_ptr) in
      left_fold_ref_split_data ((mem, Some curr_ptr, (None: ptr option)), stack)
    else
      (* Case 1b. Single leaf with too much tez in it. Exclude it. *)
      left_fold_ref_split_data ((mem, (None: ptr option), Some curr_ptr), stack)
  | Branch branch ->
    if branch.left_tez + branch.right_tez <= limit
    then (* total_tez <= limit *)
      (* Case 2. The whole tree has not too much tez in it. Include it. *)
      let mem = mem_update (mem, curr_ptr, node_set_parent null_ptr) in
      left_fold_ref_split_data ((mem, Some curr_ptr, (None: ptr option)), stack)
    else (* limit < total_tez *)
      let mem = mem_del (mem, curr_ptr) in
      let mem = mem_update (mem, branch.right, node_set_parent branch.parent) in
      (* Semantically it would be better to detach branch.left as well here
       *
       *   let mem = BigMap.mem_update mem branch.left (node_set_parent Ptr.null) in
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
            let left_branch = mem_get (mem, branch.left) in
            (Right, branch.right, limit - (node_tez left_branch))
        in
        ref_split_rec
          ( mem
          , tree_to_recurse_into
          , limit_to_use
          , ({ rec_direction=rec_direction; branch=branch } :: stack)
          )

let ref_split
    (mem, curr_ptr, limit: mem * ptr * tez)
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
let take (mem, root_ptr, limit, root_data: mem * avl_ptr * tez * int)
  : mem * avl_ptr =
  let root_ptr = match root_ptr with AVLPtr root_ptr -> root_ptr in
  match mem_get (mem, root_ptr) with
  | Root deep ->
    let (deep, r2) = deep in
    (match deep with
    | Some r ->
      let (mem, l, r) = ref_split (mem, r, limit) in
      let (mem, new_root) = mem_new (mem, Root (l, root_data)) in
      let mem = match l with
        | Some l -> mem_update (mem, l, node_set_parent new_root)
        | None -> mem in
      let mem = mem_set (mem, root_ptr, Root (r, r2)) in
      (mem, AVLPtr new_root)
    | None ->
      let (mem, new_root) = mem_new (mem, Root ((None: ptr option), root_data)) in
      (mem, AVLPtr new_root))
  | Branch ignored -> (failwith "invariant violation: avl_ptr does not point to a Root" : mem * avl_ptr)
  | Leaf ignored -> (failwith "invariant violation: avl_ptr does not point to a Root" : mem * avl_ptr)

let avl_mk_empty (root_data, mem : int * mem) : mem * avl_ptr =
  let mem = {
    mem = (Big_map.empty: (ptr, node) big_map);
    max_id = 0;
  } in
  let (mem, ptr) = mem_new (mem, Root ((None: ptr option), root_data)) in
  (mem, AVLPtr ptr)
