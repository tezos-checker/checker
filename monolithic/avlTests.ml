open Avl
open OUnit2
module Q = QCheck
open BigMap
open Tez

type element_list = (int * Tez.t) list [@@deriving show]

let nTez (i: int) : Tez.t =
  Tez.of_float (float_of_int i)

let rec to_list (mem: int mem) (root: ptr option) : element_list =
  match root with
  | None -> []
  | Some k -> match mem_get mem k with
    | Leaf leaf -> [(leaf.value, leaf.tez)]
    | Branch branch ->
      List.append
        (to_list mem (Some branch.left))
        (to_list mem (Some branch.right))

let from_list (mem: int mem) (elements: element_list)
  : int mem * ptr option =
  add_all mem None elements

let assert_invariants (mem: int mem) (root: ptr option) : unit =
  let rec go (parent: ptr option) (curr: ptr) =
    match mem_get mem curr with
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
      assert (abs (branch.left_height - branch.right_height) < 2);
      go (Some curr) branch.left;
      go (Some curr) branch.right
  in match root with
  | None -> ()
  | Some root_ptr -> go None root_ptr

let assert_dangling_pointers (mem: int mem) (roots: ptr option list) : unit =
  let rec delete_tree (mem: int mem) (root_ptr: ptr) : int mem =
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
  assert (BigMap.is_empty mem)

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module IntSet = Set.Make(Int)

let arb_tez = Q.(
  map
    ~rev:(fun i -> int_of_float (Tez.to_float i))
    (fun i -> Tez.of_float (float_of_int i))
    small_int
)

let arb_item = Q.(pair small_int arb_tez)

let property_test_count = 1000

let suite =
  "AVLTests" >::: [
    "test_push_back_singleton" >::
    (fun _ ->
       let (mem, root, _) = push_back BigMap.empty empty 0 (nTez 5) in
       let actual = to_list mem (Some root) in
       let expected = [(0, nTez 5)] in
       assert_equal expected actual);

    "test_push_front_singleton" >::
    (fun _ ->
       let (mem, root, _) = push_front BigMap.empty empty 0 (nTez 5) in
       let actual = to_list mem (Some root) in
       let expected = [(0, nTez 5)] in
       assert_equal expected actual);

    "test_from_list" >::
    (fun _ ->
       let elements =
         (List.map (fun i -> (i, nTez 5))
            [ 1; 2; 8; 4; 3; 5; 6; 7; ]) in
       let (mem, root) = from_list BigMap.empty elements in
       let actual = to_list mem root in
       assert_equal elements actual ~printer:show_element_list);

    "test_del_singleton" >::
    (fun _ ->
       let (mem, _, elem) = push_back BigMap.empty None 1 (nTez 5) in
       let (mem, root) = del mem elem in
       assert_equal None root;
       assert_bool "mem wasn't empty" (BigMap.is_empty mem));

    "test_del" >::
    (fun _ ->
       let fst_elements =
         (List.map (fun i -> (i, nTez 5))
            [ 1; 2; 3; 4; 5 ]) in
       let mid = (6, nTez 5) in
       let snd_elements =
         (List.map (fun i -> (i, nTez 5))
            [ 7; 8; 9 ]) in

       let (mem, root) = from_list BigMap.empty fst_elements in
       let (mem, root, elem) = push_back mem root (fst mid) (snd mid) in
       let (mem, _) = add_all mem (Some root) snd_elements in

       let (mem, root) = del mem elem in

       assert_invariants mem root;
       assert_dangling_pointers mem [root];

       let actual = to_list mem root in
       let expected = fst_elements @ snd_elements in
       assert_equal expected actual ~printer:show_element_list);
    "test_empty_from_list_to_list" >::
    (fun _ ->
       let elements = [] in
       let (mem, root) = from_list BigMap.empty elements in
       let actual = to_list mem root in
       let expected = [] in
       assert_equal expected actual);

    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_from_list_to_list" ~count:property_test_count (Q.list arb_item)
     @@ fun xs ->
     let (mem, root) = from_list BigMap.empty xs in
     assert_invariants mem root;
     assert_dangling_pointers mem [root];

     let actual = to_list mem root in

     assert_equal xs actual ~printer:show_element_list;
     true
    );

    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_del" ~count:property_test_count
          Q.(triple (list arb_item) arb_item (list arb_item))
     @@ fun (left_items, mid_item, right_items) ->

     let (mem, root) = from_list BigMap.empty left_items in
     assert_invariants mem root;

     let (mem, root, to_del) = push_back mem root (fst mid_item) (snd mid_item)in
     assert_invariants mem (Some root);

     let (mem, root) = add_all mem (Some root) right_items in
     assert_invariants mem root;

     let (mem, root) = del mem to_del in
     assert_invariants mem root;

     let actual = to_list mem root in
     let expected = left_items @ right_items in

     assert_equal expected actual ~printer:show_element_list;
     true
    );
    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_join" ~count:property_test_count
          Q.(pair (list arb_item) (list arb_item))
     @@ fun (left, right) ->
     Q.assume (List.length left > 0);
     Q.assume (List.length right > 0);

     let mem = BigMap.empty in
     let (mem, left_tree) = from_list mem left in
     let (mem, right_tree) = from_list mem right in

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

     assert_equal expected actual ~printer:show_element_list;
     true
    );

    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_split" ~count:property_test_count Q.(pair arb_tez (list arb_item))
     @@ fun (limit, xs) ->

     Q.assume (List.for_all (fun (_, t) -> Tez.compare t Tez.zero > 0) xs);
     Q.assume (Tez.compare limit Tez.zero > 0);

     let (mem, root) = from_list BigMap.empty xs in

     let (mem, left, right) = split mem root limit in
     assert_invariants mem left;
     assert_invariants mem right;
     assert_dangling_pointers mem [left; right];

     let actual_left = to_list mem left in
     let actual_right = to_list mem right in

     let rec split_list (lim: Tez.t) (xs: element_list) =
       match xs with
       | [] -> ([], [])
       | x :: xs ->
         if Tez.compare (snd x) lim <= 0
         then
           match split_list (Tez.sub lim (snd x)) xs with
             (l, r) -> (x::l, r)
         else
           ([], x::xs)
     in

     let (expected_left, expected_right) = split_list limit xs in

     assert_equal
       expected_left
       actual_left
       ~printer:show_element_list;

     assert_equal
       expected_right
       actual_right
       ~printer:show_element_list;

     true
    )
  ]
