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

let node_set_parent (n: node) (p: ptr option) =
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
        node_set_parent right_left (Some parent_ptr) in
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
        node_set_parent left_right (Some parent_ptr) in

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

let join
      (mem: mem) (left_ptr: ptr) (right_ptr: ptr)
      : mem * ptr =

  assert ((max mem left_ptr).id < (min mem right_ptr).id);

  let left = mem_get mem left_ptr in
  let right = mem_get mem right_ptr in

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
  balance mem ptr

(* Split the longest prefix of the tree with less than
 * given amount of tez.
 *)
let rec split (mem: mem) (root: ptr option) (limit: mutez)
  : mem * ptr option * ptr option =
  match root with
    | None -> (mem, None, None)
    | Some root_ptr -> match mem_get mem root_ptr with
      | Leaf leaf ->
        if leaf.item.mutez <= limit
        then (mem, Some root_ptr, None)
        else (mem, None, Some root_ptr)
      | Branch branch ->
        if branch.left_mutez + branch.right_mutez <= limit
          then (* total_mutez <= limit *)
            (mem, Some root_ptr, None)
        else if branch.left_mutez == limit
          then (* left_mutez == limit *)
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
                  (mem_del mem root_ptr,
                    Some branch.left,
                    right)

let rec to_list (mem: mem) (root: ptr option) : item list =
  match root with
    | None -> []
    | Some k -> match mem_get mem k with
      | Leaf leaf -> [leaf.item]
      | Branch branch ->
        List.append
          (to_list mem (Some branch.left))
          (to_list mem (Some branch.right))

let from_list (mem: mem) (items: item list) : mem * ptr option =
  add_all mem None items

open OUnit2
module Q = QCheck

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module IntSet = Set.Make(Int)

let suite =
  "AVLTests" >::: [
    "test_singleton" >::
    (fun _ ->
      let item = { id = 0; mutez = 5; } in
      let (mem, root) = add Mem.empty empty item in
      let actual = to_list mem (Some root) in
      let expected = [item] in
      assert_equal expected actual);

    "test_multiple" >::
    (fun _ ->
      let items =
            (List.map (fun i -> { id = i; mutez = 5; })
             [ 1; 2; 8; 4; 3; 5; 6; 7; ]) in
      let (mem, root) = add_all Mem.empty None items in
      let actual = to_list mem root in
      let expected = List.sort (fun a b -> compare a.id b.id) items in
      assert_equal expected actual ~printer:show_item_list);

    "test_del_singleton" >::
    (fun _ ->
      let (mem, root) = add Mem.empty None { id = 1; mutez = 5} in
      let (mem, root) = del mem (Some root) 1 in
      assert_equal None root;
      assert_bool "mem wasn't empty" (Mem.is_empty mem));

    "test_del" >::
    (fun _ ->
      let items =
            (List.map (fun i -> { id = i; mutez = 5; })
             [ 1; 2; 8; 4; 3; 5; 6; 7; ]) in
      let (mem, root) = from_list Mem.empty items in
      let (mem, root) = del mem root 5 in
      let actual = to_list mem root in
      let expected =
        List.sort
          (fun a b -> compare a.id b.id)
          (List.filter (fun i -> i.id <> 5) items) in
      assert_equal expected actual ~printer:show_item_list);

    "test_empty_from_list_to_list" >::
    (fun _ ->
      let items = [] in
      let (mem, root) = from_list Mem.empty items in
      let actual = to_list mem root in
      let expected = [] in
      assert_equal expected actual);

    (qcheck_to_ounit
       @@ Q.Test.make ~name:"test_from_list_to_list" ~count:10_000 Q.(list small_int)
       @@ fun xs ->
         let mkitem i = { id = i; mutez = 100 + i; } in

         let (mem, root) = add_all Mem.empty None (List.map mkitem xs) in
         let actual = to_list mem root in

         let expected = List.map mkitem (IntSet.elements @@ IntSet.of_list xs) in
         assert_equal expected actual ~printer:show_item_list;
         true
    );

    (qcheck_to_ounit
       @@ Q.Test.make ~name:"test_del" ~count:10_000 Q.(list small_int)
       @@ fun xs ->
         Q.assume (List.length xs > 0);
         let (to_del, xs) = (List.hd xs, List.tl xs) in

         let mkitem i = { id = i; mutez = 100 + i; } in

         let (mem, root) = add_all Mem.empty None (List.map mkitem xs) in
         let (mem, root) = del mem root to_del in
         let actual = to_list mem root in

         let expected =
                xs
                  |> IntSet.of_list
                  |> IntSet.remove to_del
                  |> IntSet.elements
                  |> List.map mkitem in
         assert_equal expected actual ~printer:show_item_list;
         true
    );

    (qcheck_to_ounit
       @@ Q.Test.make ~name:"test_split" ~count:10_000 Q.(list small_int)
       @@ fun xs ->
         Q.assume (List.length xs > 0);
         Q.assume (List.for_all (fun i -> i > 0) xs);
         let (limit, xs) = (List.hd xs, List.tl xs) in

         let mkitem i = { id = i; mutez = i; } in

         let (mem, root) = add_all Mem.empty None (List.map mkitem xs) in

         let (mem, left, right) = split mem root limit in
         let actual_left = to_list mem left in
         let actual_right = to_list mem right in

         let rec split_list lim xs =
           match xs with
             | [] -> ([], [])
             | x :: xs ->
               if x <= lim
               then
                 match split_list (lim - x) xs with
                   (l, r) -> (x::l, r)
               else
                 ([], x::xs)
               in

         let (expected_left, expected_right) =
                xs
                  |> IntSet.of_list
                  |> IntSet.elements
                  |> split_list limit in

         assert_equal
           (List.map mkitem expected_left)
           actual_left
           ~printer:show_item_list;

         assert_equal
           (List.map mkitem expected_right)
           actual_right
           ~printer:show_item_list;

         true
    )
  ]
