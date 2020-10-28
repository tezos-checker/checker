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
 * however the auction process requires us to split the next item if necessary.
 * This can be implemented by popping the smallest leftover, splitting and
 * re-inserting the pieces to both trees. And we might have some small
 * functions helping this, but it shouldn't be too hard to implement them.
 *
 * However, if the item_ids are sequential, we can not split them with each
 * piece having a different id, while still being smaller than the following
 * items. We might have to change their representation to allow this.
 *
 * There are some amount of property tests on major code paths, but there
 * might be other issues.
 *
 * On property tests, we use generic generators and skip most of them. This
 * is very inefficient, instead we should have custom generators.
 *
 *)

(*
 * A subset of checker types. We should split those to a separate module
 * in future to avoid the module cycle.
 *)

type mutez = int [@@deriving show]
type item_id = int [@@deriving show]


(* A liquidation item *)
type item = {
  id: item_id;
  mutez: mutez;
  }
[@@deriving show]

type item_list = item list [@@deriving show]

(*
 * A doubly-linked balanced tree where the leaves contain the liquidation
 * items, and the branches contain the amount of tez on their left and
 * right children.
 *)

type ptr = int64 [@@deriving show]

type leaf = {
  item: item;
  parent: int64 option;
  }
[@@deriving show]

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
[@@deriving show]

type node =
  | Leaf of leaf
  | Branch of branch
[@@deriving show]

let node_mutez n =
  match n with
    | Leaf leaf -> leaf.item.mutez
    | Branch branch -> branch.left_mutez + branch.right_mutez

let node_height n =
  match n with
    | Leaf _ -> 1
    | Branch branch -> max branch.left_height branch.right_height + 1

let node_key n =
  match n with
    | Leaf leaf -> leaf.item.id
    | Branch branch -> branch.key

let node_set_parent (p: ptr option) (n: node) =
  match n with
    | Leaf leaf -> Leaf { leaf with parent = p; }
    | Branch branch -> Branch { branch with parent = p; }

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

let mem_set (m: 'a Mem.t) (k: ptr) (v: 'a) : 'a Mem.t =
  Mem.add k v m

let mem_new (m: 'a Mem.t) (v: 'a) : 'a Mem.t * ptr =
  let ptr = mem_next_ptr m in
  (mem_set m ptr v, ptr)

let mem_get (m: 'a Mem.t) (k: ptr) : 'a =
  Mem.find k m

let mem_update (m: 'a Mem.t) (k: ptr) (f: 'a -> 'a) : 'a Mem.t =
  mem_set m k @@ f (mem_get m k)

let mem_del (m: 'a Mem.t) (k: ptr) : 'a Mem.t =
  Mem.remove k m

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
let rotate_left (mem: mem) (parent_ptr: ptr) : mem * ptr =
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
            right_mutez = node_mutez right_left;
            right_height = node_height right_left;
            parent = Some right_ptr;
        } in
  let updated_right = Branch
        { right with
          parent = parent.parent;
          left = parent_ptr;
          left_mutez = node_mutez updated_parent;
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
let rotate_right (mem: mem) (parent_ptr: ptr) : mem * ptr =
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
            left_mutez = node_mutez left_right;
            left_height = node_height left_right;
            parent = Some left_ptr;
        } in
  let updated_left = Branch
        { left with
          parent = parent.parent;
          right = parent_ptr;
          right_mutez = node_mutez updated_parent;
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
let balance (mem: mem) (parent_ptr: ptr) : mem * ptr =
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

let rec add (mem: mem) (root: ptr option) (new_item : item) : mem * ptr =
  match root with
    (* When the tree is empty, create the initial leaf. *)
    | None ->
        let node = Leaf { item=new_item; parent=None; } in
        mem_new mem node
    (* When there is already an element, *)
    | Some root_ptr ->
        match Mem.find root_ptr mem with
          (* ... and if it is a leaf,*)
          | Leaf { item = existing_item; parent = parent; } ->
            (match compare existing_item.id new_item.id with
              (* ... we override it if the keys are the same. *)
              | cmp when cmp = 0 ->
                  (* NOTE: I can not think of a case where we'd overwrite an
                   * existing liquidation, so maybe this case should fail.
                   *)
                  let node = Leaf {item=new_item; parent=parent; } in
                  let mem = mem_set mem root_ptr node in
                  (mem, root_ptr)
              (* ... or we create a sibling leaf and a parent branch.  *)
              | cmp ->
                let new_ptr = mem_next_ptr(mem) in
                let branch_ptr = Int64.succ new_ptr in
                let (left, left_ptr, right, right_ptr) =
                  if cmp < 0
                  then (existing_item, root_ptr, new_item, new_ptr)
                  else (new_item, new_ptr, existing_item, root_ptr) in
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
                let mem = mem_set mem left_ptr left_leaf in
                let mem = mem_set mem right_ptr right_leaf in
                let mem = mem_set mem branch_ptr new_branch in
                (mem, branch_ptr)
            )
          (* ... if it is a branch, we insert it to the corresponding side
           * updating the aggregates on the branch.
           *)
          | Branch existing_branch ->
            let target_left = new_item.id < existing_branch.key in
            let (mem, new_subtree) =
              add
                mem
                (Some
                  (if target_left
                   then existing_branch.left
                   else existing_branch.right))
                 new_item in
            let new_node = mem_get mem new_subtree in
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
            let mem = mem_set mem root_ptr new_branch in
            balance mem root_ptr

let rec del (mem: mem) (root: ptr option) (id : item_id) : mem * ptr option =
  match root with
    (* Deleting something from an empty tree returns an empty tree. *)
    | None ->
        (mem, None)
    | Some root_ptr ->
        match Mem.find root_ptr mem with
          (* Deleting something from a singleton tree might be an empty tree. *)
          | Leaf existing ->
            if existing.item.id = id
            then (Mem.remove root_ptr mem, None)
            else (mem, Some root_ptr)
          (* Deleting something from a branch recurses to the relevant side. *)
          | Branch existing ->
            let target_left = id < existing.key in
            let (mem, new_subtree') =
              del
                mem
                (Some
                  (if target_left
                   then existing.left
                   else existing.right))
                 id in
            match new_subtree' with
              | Some ptr ->
                  let value = mem_get mem ptr in
                  let new_branch =
                        if target_left
                        then Branch {
                               existing with
                               left = ptr;
                               left_mutez = node_mutez value;
                               left_height = node_height value;
                             }
                        else Branch {
                               existing with
                               right = ptr;
                               right_mutez = node_mutez value;
                               right_height = node_height value;
                             } in
                  let mem = mem_set mem root_ptr new_branch in
                  (match balance mem root_ptr with
                    (a, b) -> (a, Some b))
              (* If one side of the branch ends up being empty, we replace the
               * branch itself with the other side. *)
              | None ->
                  let (_deleted, preserved) =
                    if target_left
                    then (existing.left, existing.right)
                    else (existing.right, existing.left) in
                  let mem = Mem.remove root_ptr mem in
                  let mem = mem_update mem preserved
                              (node_set_parent existing.parent) in
                  (mem, Some(preserved))

let rec debug_string (mem: mem) (root: ptr option) : string =
  let indent str = "  " ^ String.concat "\n  " (String.split_on_char '\n' str) in
  match root with
    | None -> "Empty"
    | Some root_ptr -> match mem_get mem root_ptr with
      | Leaf leaf -> Int64.to_string root_ptr ^ ": Leaf " ^ show_leaf leaf
      | Branch branch ->
        Int64.to_string root_ptr ^ ": Branch " ^ show_branch branch ^ "\n"
          ^ indent ("Left:\n"
            ^ indent (debug_string mem (Some branch.left))) ^ "\n"
          ^ indent ("Right:\n"
            ^ indent (debug_string mem (Some branch.right)))

let add_all (mem: mem) (root: ptr option) (items: item list)
  : mem * ptr option =
  List.fold_left
    (fun (mem, root) item ->
       let (mem, root) = add mem root item in
       (mem, Some root))
    (mem, root)
    items

let rec max (mem: mem) (root: ptr) : item =
  match mem_get mem root with
    | Leaf leaf -> leaf.item
    | Branch branch -> max mem branch.right

let rec min (mem: mem) (root: ptr) : item =
  match mem_get mem root with
    | Leaf leaf -> leaf.item
    | Branch branch -> min mem branch.left

let add_all_debug (mem: mem) (root: ptr option) (items: item list)
  : mem * ptr option =
  List.fold_left
    (fun (mem, root) item ->
       print_string "--------------------------------\n";
       print_string ("Inserting: " ^ show_item item ^ "\n");
       let (mem, root) = add mem root item in
       print_string (debug_string mem (Some root));
       print_newline ();
       (mem, Some root))
    (mem, root)
    items

let rec join (mem: mem) (left_ptr: ptr) (right_ptr: ptr) : mem * ptr =
  assert ((max mem left_ptr).id < (min mem right_ptr).id);

  let left = mem_get mem left_ptr in
  let right = mem_get mem right_ptr in

  if abs (node_height left - node_height right) < 2 then
    let new_branch = Branch {
      left = left_ptr;
      left_height = node_height left;
      left_mutez = node_mutez left;
      key = node_key right;
      right_mutez = node_mutez right;
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
                  right_mutez = node_mutez new_left_right;
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
                  left_mutez = node_mutez new_right_left;
                  parent = None;
                } in
    let mem = mem_update mem new_right_left_ptr
                (node_set_parent (Some right_ptr)) in
    let (mem, right_ptr) = balance mem right_ptr in
    (mem, right_ptr)

(* Split the longest prefix of the tree with less than
 * given amount of tez.
 *)
let rec split (mem: mem) (root: ptr option) (limit: mutez)
  : mem * ptr option * ptr option =
  match root with
    | None -> (mem, None, None)
    | Some root_ptr ->
      let mem = mem_update mem root_ptr (node_set_parent None) in
      match mem_get mem root_ptr with
      | Leaf leaf ->
        if leaf.item.mutez <= limit
        then (mem, Some root_ptr, None)
        else (mem, None, Some root_ptr)
      | Branch branch ->
        if branch.left_mutez + branch.right_mutez <= limit
          then (* total_mutez <= limit *)
            (mem, Some root_ptr, None)
        else if branch.left_mutez = limit
          then (* left_mutez == limit *)
            let mem = mem_update mem branch.left (node_set_parent None) in
            let mem = mem_update mem branch.right (node_set_parent None) in
            (mem_del mem root_ptr,
              Some branch.left,
              Some branch.right)
        else if limit < branch.left_mutez
          then (* limit < left_mutez < total_mutez *)
            match split mem (Some branch.left) limit with
              | (mem, left, Some right) ->
                  let (mem, joined) = join mem right branch.right in
                  (mem_del mem root_ptr, left, Some joined)
              | _ -> failwith "impossible"
        else (* left_mutez < limit < total_mutez *)
            let left = mem_get mem branch.left in
            match split mem (Some branch.right) (limit - node_mutez left) with
              | (mem, Some left, right) ->
                  let (mem, joined) = join mem branch.left left in
                  (mem_del mem root_ptr, Some joined, right)
              | (mem, None, right) ->
                  let mem = mem_update mem branch.left (node_set_parent None) in
                  (mem_del mem root_ptr,
                    Some branch.left,
                    right)
