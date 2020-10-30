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
 *
 * There are some amount of property tests on major code paths, but there
 * might be other issues.
*)

open BigMap
open Tez
open Format

(*
 * A double-ended queue backed by a doubly-linked balanced tree where
 * the leaves contain the liquidation elements, and the branches contain
 * the amount of tez on their left and
 * right children.
 *)

type 't leaf = {
  value: 't;
  tez: Tez.t;
  parent: int64 option;
}
[@@deriving show]

type branch = {
  left: ptr;
  left_height: int;
  left_tez: Tez.t;
  right_tez: Tez.t;
  right_height: int;
  right: ptr;
  parent: int64 option;
}
[@@deriving show]

type 't node =
  | Leaf of 't leaf
  | Branch of branch
[@@deriving show]

type 't mem = ('t node) BigMap.t

let node_tez n =
  match n with
  | Leaf leaf -> leaf.tez
  | Branch branch -> Tez.add branch.left_tez branch.right_tez

let node_height n =
  match n with
  | Leaf _ -> 1
  | Branch branch -> max branch.left_height branch.right_height + 1

let node_parent n =
  match n with
  | Leaf leaf -> leaf.parent
  | Branch branch -> branch.parent

let node_set_parent (p: ptr option) (n: 't node) =
  match n with
  | Leaf leaf -> Leaf { leaf with parent = p; }
  | Branch branch -> Branch { branch with parent = p; }

let empty: ptr option = None


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


(* Before:
 *
 *            parent
 *            /    \
 *           /      \
 *         left    right
 *                 /   \
 *                /     \
 *           right_left  a
 *
 * After:
 *
 *            right
 *            /   \
 *           /     \
 *        parent    a
 *        /   \
 *       /     \
 *     left  right_left
*)
let rotate_left (mem: 't mem) (parent_ptr: ptr) : 't mem * ptr =
  let parent =
    match mem_get mem parent_ptr with
    | Leaf _ -> failwith "rotate_left: can't rotate a leaf"
    | Branch parent -> parent in
  let right_ptr = parent.right in
  let right =
    match mem_get mem right_ptr with
    | Leaf _ -> failwith "rotate_left: can't rotate a leaf"
    | Branch right -> right in
  let right_left_ptr = right.left in
  let right_left = mem_get mem right_left_ptr in
  let updated_parent = Branch
      { parent with
        right = right_left_ptr;
        right_tez = node_tez right_left;
        right_height = node_height right_left;
        parent = Some right_ptr;
      } in
  let updated_right = Branch
      { right with
        parent = parent.parent;
        left = parent_ptr;
        left_tez = node_tez updated_parent;
        left_height = node_height updated_parent;
      } in
  let updated_right_left =
    node_set_parent (Some parent_ptr) right_left in
  let mem = mem_set mem parent_ptr updated_parent in
  let mem = mem_set mem right_ptr updated_right in
  let mem = mem_set mem right_left_ptr updated_right_left in
  (mem, right_ptr)

(* Before:
 *
 *            parent
 *            /    \
 *           /      \
 *         left    right
 *         / \
 *        /   \
 *       a  left_right
 *
 * After:
 *
 *             left
 *             /  \
 *            /    \
 *           a    parent
 *                /   \
 *               /     \
 *         left_right  right
*)
let rotate_right (mem: 't mem) (parent_ptr: ptr) : 't mem * ptr =
  let parent =
    match mem_get mem parent_ptr with
    | Leaf _ -> failwith "rotate_left: can't rotate a leaf"
    | Branch parent -> parent in
  let left_ptr = parent.left in
  let left =
    match mem_get mem left_ptr with
    | Leaf _ -> failwith "rotate_left: can't rotate a leaf"
    | Branch left -> left in
  let left_right_ptr = left.right in
  let left_right = mem_get mem left_right_ptr in
  let updated_parent = Branch
      { parent with
        left = left_right_ptr;
        left_tez = node_tez left_right;
        left_height = node_height left_right;
        parent = Some left_ptr;
      } in
  let updated_left = Branch
      { left with
        parent = parent.parent;
        right = parent_ptr;
        right_tez = node_tez updated_parent;
        right_height = node_height updated_parent;
      } in
  let updated_left_right =
    node_set_parent (Some parent_ptr) left_right in

  let mem = mem_set mem parent_ptr updated_parent in
  let mem = mem_set mem left_ptr updated_left in
  let mem = mem_set mem left_right_ptr updated_left_right in
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
let balance (mem: 't mem) (parent_ptr: ptr) : 't mem * ptr =
  let (mem, ptr) = match mem_get mem parent_ptr with
    | Branch branch
      when abs (branch.left_height - branch.right_height) > 1 ->
      assert (abs (branch.left_height - branch.right_height) == 2);

      let parent_balance = branch.right_height - branch.left_height in
      let heavy_child_ptr = if parent_balance < 0 then branch.left else branch.right in
      let heavy_child = match mem_get mem heavy_child_ptr with
        | Leaf _ -> failwith "invariant violation: heavy_child should be a branch"
        | Branch b -> b in
      let heavy_child_balance =
        heavy_child.right_height - heavy_child.left_height in
      if parent_balance < 0 && heavy_child_balance <= 0 then
        (* Left, Left *)
        rotate_right mem parent_ptr
      else if parent_balance < 0 && heavy_child_balance > 0 then
        (* Left, Right *)
        let (mem, child) = rotate_left mem heavy_child_ptr in
        let mem =
          mem_set mem parent_ptr (Branch  {
              branch with
              left = child;
              left_height = branch.left_height - 1; }) in
        rotate_right mem parent_ptr
      else if parent_balance > 0 && heavy_child_balance >= 0 then
        (* Right, Right*)
        rotate_left mem parent_ptr
      else if parent_balance > 0 && heavy_child_balance < 0 then
        (* Right, Left *)
        let (mem, child) = rotate_right mem heavy_child_ptr in
        let mem =
          mem_set mem parent_ptr (Branch {
              branch with
              right = child;
              right_height = branch.right_height - 1;}) in
        rotate_left mem parent_ptr
      else
        failwith "invariant violation: balance predicates partial"
    | _ -> (mem, parent_ptr) in
  assert (
    let b = match mem_get mem ptr with
      | Branch b -> b | _ -> failwith "impossible" in
    abs (b.left_height - b.right_height) <= 1);
  (mem, ptr)

let rec join (mem: 't mem) (left_ptr: ptr) (right_ptr: ptr) : 't mem * ptr =
  let left = mem_get mem left_ptr in
  let right = mem_get mem right_ptr in

  if abs (node_height left - node_height right) < 2 then
    let new_branch = Branch {
        left = left_ptr;
        left_height = node_height left;
        left_tez = node_tez left;
        right_tez = node_tez right;
        right_height = node_height right;
        right = right_ptr;
        parent = None;
      } in

    let (mem, ptr) = mem_new mem new_branch in
    let mem = mem_update mem left_ptr (node_set_parent (Some ptr)) in
    let mem = mem_update mem right_ptr (node_set_parent (Some ptr)) in

    (mem, ptr)
  else if node_height left > node_height right then
    let left = match left with Branch b -> b | Leaf _ -> failwith "impossible" in
    let (mem, new_left_right_ptr) = join mem left.right right_ptr in
    let new_left_right = mem_get mem new_left_right_ptr in
    let mem = mem_set mem left_ptr @@ Branch
        { left with
          right = new_left_right_ptr;
          right_height = node_height new_left_right;
          right_tez = node_tez new_left_right;
          parent = None;
        } in
    let mem = mem_update mem new_left_right_ptr
        (node_set_parent (Some left_ptr)) in
    let (mem, left_ptr) = balance mem left_ptr in
    (mem, left_ptr)
  else (* node_height left < node_height right *)
    let right = match right with Branch b -> b | Leaf _ -> failwith "impossible" in
    let (mem, new_right_left_ptr) = join mem left_ptr right.left in
    let new_right_left = mem_get mem new_right_left_ptr in
    let mem = mem_set mem right_ptr @@ Branch
        { right with
          left = new_right_left_ptr;
          left_height = node_height new_right_left;
          left_tez = node_tez new_right_left;
          parent = None;
        } in
    let mem = mem_update mem new_right_left_ptr
        (node_set_parent (Some right_ptr)) in
    let (mem, right_ptr) = balance mem right_ptr in
    (mem, right_ptr)

let push_back
  (mem: 't mem) (root: ptr option) (value: 't) (tez: 'tez)
  : 't mem * ptr * ptr =
  match root with
  (* When the tree is empty, create the initial leaf. *)
  | None ->
    let node = Leaf { value=value; tez=tez; parent=None; } in
    (match mem_new mem node with (mem, ptr) -> (mem, ptr, ptr))
  (* When there is already an element, append it. *)
  | Some root_ptr ->
    let mem, new_leaf =
          mem_new mem @@ Leaf { value=value; tez=tez; parent=None; } in
    (match join mem root_ptr new_leaf with (mem, avl) -> (mem, avl, new_leaf))

let push_front
  (mem: 't mem) (root: ptr option) (value: 't) (tez: 'tez)
  : 't mem * ptr * ptr =
  match root with
  (* When the tree is empty, create the initial leaf. *)
  | None ->
    let node = Leaf { value=value; tez=tez; parent=None; } in
    (match mem_new mem node with (mem, ptr) -> (mem, ptr, ptr))
  (* When there is already an element, append it. *)
  | Some root_ptr ->
    let mem, new_leaf =
          mem_new mem @@ Leaf { value=value; tez=tez; parent=None; } in
    (match join mem new_leaf root_ptr with (mem, avl) -> (mem, avl, new_leaf))

let del (mem: 't mem) (ptr: ptr): 't mem * ptr option =
  let self = mem_get mem ptr in
  let mem = mem_del mem ptr in
  match node_parent self with
    (* when deleting the sole element, we return an empty tree *)
    | None -> (mem, None)
    (* otherwise, the parent of the deleted element is redundant since it
     * only has a single child, so we delete the parent and the orphan sibling
     * is adopted by the grandparent who have lost its child. *)
    | Some parent_ptr -> match mem_get mem parent_ptr with
      | Leaf _ -> failwith "del: parent is a leaf"
      | Branch parent ->
        let sibling_ptr = if parent.left = ptr
                            then parent.right
                            else parent.left in
        assert (sibling_ptr <> ptr);
        let mem = mem_del mem parent_ptr in
        let mem = mem_update mem sibling_ptr (node_set_parent parent.parent) in
        match parent.parent with
          | None -> (mem, Some sibling_ptr)
          | Some grandparent_ptr ->
            let update_matching_child mem ptr from_ptr to_ptr =
              mem_update mem ptr @@ fun v -> match v with
                | Leaf _ -> failwith "update_matching_child: got a leaf"
                | Branch b ->
                   let to_ = mem_get mem to_ptr in
                   if b.left = from_ptr
                   then Branch {
                           b with
                           left = to_ptr;
                           left_tez = node_tez to_;
                           left_height = node_height to_;
                         }
                   else (
                     assert (b.right = from_ptr);
                     Branch {
                       b with
                       right = to_ptr;
                       right_tez = node_tez to_;
                       right_height = node_height to_;
                     }) in

            let mem = update_matching_child mem grandparent_ptr parent_ptr sibling_ptr in

            let rec balance_parents mem ptr =
                let parent_ptr = node_parent (mem_get mem ptr) in
                let (mem, new_ptr) = balance mem ptr in
                match parent_ptr with
                  | None -> (mem, new_ptr)
                  | Some parent_ptr ->
                    let mem = update_matching_child mem parent_ptr ptr new_ptr in
                    balance_parents mem parent_ptr in

             match balance_parents mem grandparent_ptr with
               (mem, root) -> (mem, Some root)

let rec debug_string (mem: 't mem) (show: 't -> string) (root: ptr option) : string =
  let indent str = "  " ^ String.concat "\n  " (String.split_on_char '\n' str) in
  match root with
  | None -> "Empty"
  | Some root_ptr -> match mem_get mem root_ptr with
    | Leaf leaf ->
      Int64.to_string root_ptr
        ^ sprintf ": Leaf { value: %s; tez: %s; parent: %s }"
            (show leaf.value) (Tez.show_tez leaf.tez)
            (match leaf.parent with | Some i -> Int64.to_string i | None -> "None")
    | Branch branch ->
      Int64.to_string root_ptr ^ ": Branch " ^ show_branch branch ^ "\n"
      ^ indent ("Left:\n"
                ^ indent (debug_string mem show (Some branch.left))) ^ "\n"
      ^ indent ("Right:\n"
                ^ indent (debug_string mem show (Some branch.right)))

let add_all (mem: 't mem) (root: ptr option) (xs: ('t * Tez.t) list)
  : 't mem * ptr option =
  List.fold_left
    (fun (mem, root) (value, tez) ->
       let (mem, root, _) = push_back mem root value tez in
       (mem, Some root))
    (mem, root)
    xs

let rec find_root (mem: 't mem) (node: ptr) : ptr =
  match node_parent (mem_get mem node) with
  | None -> node
  | Some parent_ptr -> find_root mem parent_ptr


let rec max (mem: 't mem) (root: ptr) : ptr * 't leaf =
  match mem_get mem root with
  | Leaf leaf -> (root, leaf)
  | Branch branch -> max mem branch.right

let rec min (mem: 't mem) (root: ptr) : ptr * 't leaf =
  match mem_get mem root with
  | Leaf leaf -> (root, leaf)
  | Branch branch -> min mem branch.left

(*
let add_all_debug (mem: 't mem) (root: ptr option) (elements: element list)
  : 't mem * ptr option =
  List.fold_left
    (fun (mem, root) element ->
       print_string "--------------------------------\n";
       print_string ("Inserting: " ^ show_element element ^ "\n");
       let (mem, root) = add mem root element in
       print_string (debug_string mem (Some root));
       print_newline ();
       (mem, Some root))
    (mem, root)
    elements
*)

(* Split the longest prefix of the tree with less than
 * given amount of tez.
*)
let rec split (mem: 't mem) (root: ptr option) (limit: Tez.t)
  : 't mem * ptr option * ptr option =
  match root with
  | None -> (mem, None, None)
  | Some root_ptr ->
    let mem = mem_update mem root_ptr (node_set_parent None) in
    match mem_get mem root_ptr with
    | Leaf leaf ->
      if Tez.compare leaf.tez limit <= 0
      then (mem, Some root_ptr, None)
      else (mem, None, Some root_ptr)
    | Branch branch ->
      if Tez.compare (Tez.add branch.left_tez branch.right_tez) limit <= 0
      then (* total_tez <= limit *)
        (mem, Some root_ptr, None)
      else if Tez.compare branch.left_tez limit = 0
      then (* left_tez == limit *)
        let mem = mem_update mem branch.left (node_set_parent None) in
        let mem = mem_update mem branch.right (node_set_parent None) in
        (mem_del mem root_ptr,
         Some branch.left,
         Some branch.right)
      else if Tez.compare limit branch.left_tez < 0
      then (* limit < left_tez < total_tez *)
        match split mem (Some branch.left) limit with
        | (mem, left, Some right) ->
          let (mem, joined) = join mem right branch.right in
          (mem_del mem root_ptr, left, Some joined)
        | _ -> failwith "impossible"
      else (* left_tez < limit < total_tez *)
        let left = mem_get mem branch.left in
        match split mem (Some branch.right) (Tez.sub limit (node_tez left)) with
        | (mem, Some left, right) ->
          let (mem, joined) = join mem branch.left left in
          (mem_del mem root_ptr, Some joined, right)
        | (mem, None, right) ->
          let mem = mem_update mem branch.left (node_set_parent None) in
          (mem_del mem root_ptr,
           Some branch.left,
           right)
