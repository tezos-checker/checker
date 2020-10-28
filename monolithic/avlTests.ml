open Avl
open OUnit2
module Q = QCheck

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

let assert_invariants (mem: mem) (root: ptr option) : unit =
  let rec go (parent: ptr option) (curr: ptr) =
    match mem_get mem curr with
    | Leaf leaf ->
      assert (leaf.parent = parent)
    | Branch branch ->
      let left = mem_get mem branch.left in
      let right = mem_get mem branch.right in
      assert (branch.parent = parent);
      assert (branch.left_height = node_height left);
      assert (branch.left_mutez = node_mutez left);
      assert (branch.right_height = node_height right);
      assert (branch.right_mutez = node_mutez right);
      assert (abs (branch.left_height - branch.right_height) < 2);
      go (Some curr) branch.left;
      go (Some curr) branch.right
  in match root with
  | None -> ()
  | Some root_ptr -> go None root_ptr

let assert_dangling_pointers (mem: mem) (roots: ptr option list) : unit =
  let rec delete_tree (mem: mem) (root_ptr: ptr) : mem =
    let root = mem_get mem root_ptr in
    let mem = mem_del mem root_ptr in
    match root with
    | Leaf _ -> mem
    | Branch branch ->
      let mem = delete_tree mem branch.left in
      let mem = delete_tree mem branch.right in
      mem in
  let mem = List.fold_left
      (fun mem x -> Option.fold ~none:mem ~some:(delete_tree mem) x)
      mem
      roots in
  assert (Mem.is_empty mem)

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module IntSet = Set.Make(Int)

let property_test_count = 1000

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
       assert_invariants mem root;
       assert_dangling_pointers mem [root];
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
     @@ Q.Test.make ~name:"prop_from_list_to_list" ~count:property_test_count Q.(list small_int)
     @@ fun xs ->
     let mkitem i = { id = i; mutez = 100 + i; } in

     let (mem, root) = add_all Mem.empty None (List.map mkitem xs) in
     assert_invariants mem root;
     assert_dangling_pointers mem [root];

     let actual = to_list mem root in

     let expected = List.map mkitem (IntSet.elements @@ IntSet.of_list xs) in
     assert_equal expected actual ~printer:show_item_list;
     true
    );

    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_del" ~count:property_test_count Q.(list small_int)
     @@ fun xs ->
     Q.assume (List.length xs > 0);
     let (to_del, xs) = (List.hd xs, List.tl xs) in

     let mkitem i = { id = i; mutez = 100 + i; } in

     let (mem, root) = add_all Mem.empty None (List.map mkitem xs) in
     assert_invariants mem root;

     let (mem, root) = del mem root to_del in
     assert_invariants mem root;

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
     @@ Q.Test.make ~name:"prop_join" ~count:property_test_count Q.(list small_int)
     @@ fun xs ->
     Q.assume (List.length xs > 2);
     let (pos, xs) = (List.hd xs, List.tl xs) in

     let xs = List.sort_uniq (fun i j -> Int.compare i j) xs in

     Q.assume (pos > 0);
     Q.assume (pos < List.length xs - 1);
     Q.assume (List.for_all (fun i -> i > 0) xs);

     let splitAt n xs =
       let rec go n xs acc =
         if n <= 0
         then (List.rev acc, xs)
         else match xs with
           | [] -> failwith "index out of bounds"
           | (x::xs) -> go (n-1) xs (x::acc)
       in go n xs [] in

     let mkitem i = { id = i; mutez = 100 + i; } in
     let (left, right) = splitAt pos xs in
     let (left, right) = (List.map mkitem left, List.map mkitem right) in

     let mem = Mem.empty in
     let (mem, left_tree) = add_all mem None left in
     let (mem, right_tree) = add_all mem None right in

         (*
         print_string "=Left==================================\n";
         print_string (debug_string mem left_tree);
         print_newline ();
         print_string "-Right---------------------------------\n";
         print_string (debug_string mem right_tree);
         print_newline ();
         *)

     let (mem, joined_tree) =
       join mem (Option.get left_tree) (Option.get right_tree) in

         (*
         print_string "-Joined--------------------------------\n";
         print_string (debug_string mem (Some joined_tree));
         print_newline ();
         *)

     let joined_tree = Some joined_tree in
     assert_invariants mem joined_tree;
     assert_dangling_pointers mem [joined_tree];

     let actual = to_list mem joined_tree in
     let expected = left @ right in

     assert_equal expected actual ~printer:show_item_list;
     true
    );

    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_split" ~count:property_test_count Q.(list small_int)
     @@ fun xs ->
     Q.assume (List.length xs > 0);
     Q.assume (List.for_all (fun i -> i > 0) xs);
     let (limit, xs) = (List.hd xs, List.tl xs) in

     let mkitem i = { id = i; mutez = i; } in

     let (mem, root) = add_all Mem.empty None (List.map mkitem xs) in

     let (mem, left, right) = split mem root limit in
     assert_invariants mem left;
     assert_invariants mem right;
     assert_dangling_pointers mem [left; right];

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
