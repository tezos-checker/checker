open Avl
open OUnit2
module Q = QCheck
open BigMap
open Format

type element_list = (int * Tez.t) list [@@deriving show]

let nTez (i: int) : Tez.t = Tez.of_mutez (1_000_000 * i)

let add_all (mem: 't mem) (root: avl_ptr) (xs: element_list)
  : 't mem =
  List.fold_left
    (fun mem (value, tez) ->
       let (mem, _) = push_back mem root value tez in
       mem)
    mem
    xs

let debug_mem (mem: int mem) : unit =
  BigMap.iter
    (fun k v ->
       printf
         "%s -> %s\n"
         (Ptr.to_string k)
         (show_node pp_print_int v);
    )
    mem

let debug_avl (mem: int mem) (AVLPtr root) : unit =
  let rec go curr =
    let indent str = "  " ^ String.concat "\n  " (String.split_on_char '\n' str) in
    sprintf "%s: " (Ptr.to_string curr) ^
    match mem_get mem curr with
    | Root None -> "Root Empty"
    | Root (Some r) -> "Root\n" ^ indent (go r)
    | Leaf leaf ->
      sprintf "Leaf { value: %s; tez: %s; parent: %s }"
        (Int.to_string leaf.value) (Tez.show leaf.tez) (Ptr.to_string leaf.parent)
    | Branch branch ->
      "Branch " ^ show_branch branch ^ "\n"
      ^ indent ("Left:\n" ^ indent (go branch.left)) ^ "\n"
      ^ indent ("Right:\n" ^ indent (go branch.right));
  in printf "%s\n" (go root)

let assert_invariants (mem: int mem) (AVLPtr root) : unit =
  let rec go (parent: ptr) (curr: ptr) =
    match mem_get mem curr with
    | Root _ ->
      failwith "assert_invariants: tree root in unexpected location."
    | Leaf leaf ->
      assert (leaf.parent = parent)
    | Branch branch ->
      let left = mem_get mem branch.left in
      let right = mem_get mem branch.right in

      (*
      if (branch.right_tez <> node_tez right)
            || (branch.left_tez <> node_tez left)
        then printf "Failed branch: %s\n" (Ptr.to_string curr);
      *)
      assert (branch.parent = parent);
      assert (branch.left_height = node_height left);
      assert (branch.left_tez = node_tez left);
      assert (branch.right_height = node_height right);
      assert (branch.right_tez = node_tez right);
      assert (abs (branch.left_height - branch.right_height) < 2);
      go curr branch.left;
      go curr branch.right
  in match mem_get mem root with
  | Root None -> ()
  | Root (Some r) -> go root r
  | _ -> failwith "assert_invariants needs a root."

let assert_dangling_pointers (mem: int mem) (roots: avl_ptr list) : unit =
  let rec delete_tree (mem: int mem) (root_ptr: ptr) : int mem =
    let root = mem_get mem root_ptr in
    let mem = mem_del mem root_ptr in
    match root with
    | Root None -> mem
    | Leaf _ -> mem
    | Root (Some p) -> delete_tree mem p
    | Branch branch ->
      let mem = delete_tree mem branch.left in
      let mem = delete_tree mem branch.right in
      mem in
  let mem = List.fold_left
      (fun mem (AVLPtr t) -> delete_tree mem t)
      mem
      roots in
  assert (BigMap.is_empty mem)

let add_all_debug (mem: 't mem) (root: avl_ptr) (xs: element_list)
  : 't mem =
  List.fold_left
    (fun mem (value, tez) ->
       print_string "--------------------------------\n";
       print_string ("Inserting: " ^ Int.to_string value ^ "\n");
       let (mem, _) = push_back mem root value tez in
       debug_avl mem root;
       assert_invariants mem root;
       print_newline ();
       mem)
    mem
    xs

let to_list (mem: int mem) (AVLPtr ptr) : element_list =
  let rec go ptr: element_list =
    match mem_get mem ptr with
    | Root None -> []
    | Root (Some ptr) -> go ptr
    | Leaf leaf ->
      [(leaf.value, leaf.tez)]
    | Branch branch ->
      List.append (go branch.left) (go branch.right) in
  go ptr

let from_list (mem: int mem) (elements: element_list)
  : int mem * avl_ptr =
  let (mem, root) = mk_empty mem in
  (add_all mem root elements, root)

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module IntSet = Set.Make(Int)

let arb_tez = Q.(
    map
      ~rev:(fun i -> FixedPoint.to_int (Tez.to_fp i))
      (fun i -> nTez i)
      small_int
  )

let arb_item = Q.(pair small_int arb_tez)

let property_test_count = 1000

let suite =
  "AVLTests" >::: [
    "test_push_back_singleton" >::
    (fun _ ->
       let (mem, root) = mk_empty BigMap.empty in
       let (mem, _) = push_back mem root 0 (nTez 5) in
       let actual = to_list mem root in
       let expected = [(0, nTez 5)] in
       assert_equal expected actual);

    "test_push_front_singleton" >::
    (fun _ ->
       let (mem, root) = mk_empty BigMap.empty in
       let (mem, _) = push_front mem root 0 (nTez 5) in
       let actual = to_list mem root in
       let expected = [(0, nTez 5)] in
       assert_equal expected actual);

    "test_from_list" >::
    (fun _ ->
       let elements =
         (List.map (fun i -> (i, nTez 5))
            [ 1; 2; 3; 4; 5; 6; 7; 8; ]) in
       let (mem, root) = from_list BigMap.empty elements in
       assert_invariants mem root;
       assert_dangling_pointers mem [root];

       let actual = to_list mem root in
       assert_equal elements actual ~printer:show_element_list);

    "test_pop_front_empty" >::
    (fun _ ->
       let (mem, root) = from_list BigMap.empty [] in
       let (mem, x) = pop_front mem root in
       assert_equal [] (to_list mem root) ~printer:show_element_list;
       assert_equal x None;
    );

    "test_pop_front" >::
    (fun _ ->
       let elements = [ (1, nTez 5); (2, nTez 5) ] in
       let (mem, root) = from_list BigMap.empty elements in
       let (mem, x) = pop_front mem root in
       assert_equal [ (2, nTez 5) ] (to_list mem root) ~printer:show_element_list;
       assert_equal x (Some 1);
       let (mem, x) = pop_front mem root in
       assert_equal [] (to_list mem root) ~printer:show_element_list;
       assert_equal x (Some 2);
    );

    "test_del_singleton" >::
    (fun _ ->
       let (mem, root) = mk_empty BigMap.empty in
       let (mem, elem) = push_back mem root 1 (nTez 5) in
       let mem = del mem elem in
       assert_equal [] (to_list mem root);
       assert_equal 1 (BigMap.cardinal mem));

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
       let (mem, elem) = push_back mem root (fst mid) (snd mid) in
       let mem = add_all mem root snd_elements in

       assert_invariants mem root;
       assert_dangling_pointers mem [root];

       let mem = del mem elem in

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

     let (mem, to_del) = push_back mem root (fst mid_item) (snd mid_item)in
     assert_invariants mem root;

     let mem = add_all mem root right_items in
     assert_invariants mem root;

     let mem = del mem to_del in
     (*
     printf "- %s %s %s ----------\n"
       (show_element_list left_items)
       (show_element_list [mid_item])
       (show_element_list right_items);
     debug_avl mem root;
     *)
     assert_invariants mem root;

     let actual = to_list mem root in
     let expected = left_items @ right_items in

     assert_equal expected actual ~printer:show_element_list;
     true
    );

    (qcheck_to_ounit

     @@ Q.Test.make ~name:"prop_append" ~count:property_test_count Q.(pair (list arb_item) (list arb_item))
     @@ fun (ls, rs) ->

     let (mem, left_tree) = from_list BigMap.empty ls in
     let (mem, right_tree) = from_list mem rs in

     let mem = append mem left_tree right_tree in

     assert_invariants mem left_tree;
     assert_dangling_pointers mem [left_tree];

     let actual = to_list mem left_tree in
     let expected = ls @ rs in
     assert_equal expected actual ~printer:show_element_list;
     true
    );

    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_take" ~count:property_test_count Q.(pair arb_tez (list arb_item))
     @@ fun (limit, xs) ->

     Q.assume (List.for_all (fun (_, t) -> t > Tez.zero) xs);
     Q.assume (limit > Tez.zero);

     let (mem, right) = from_list BigMap.empty xs in
     let (mem, left) = take mem right limit in

     assert_invariants mem left;
     assert_invariants mem right;
     assert_dangling_pointers mem [left; right];

     let actual_left = to_list mem left in
     let actual_right = to_list mem right in

     let rec split_list (lim: Tez.t) (xs: element_list) =
       match xs with
       | [] -> ([], [])
       | x :: xs ->
         if snd x <= lim
         then
           match split_list Tez.(lim - snd x) xs with
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
    );
    (qcheck_to_ounit
     @@ Q.Test.make ~name:"prop_take_append" ~count:property_test_count Q.(pair arb_tez (list arb_item))
     @@ fun (limit, xs) ->

     Q.assume (List.for_all (fun (_, t) -> t > Tez.zero) xs);
     Q.assume (limit > Tez.zero);

     let (mem, right) = from_list BigMap.empty xs in
     let (mem, left) = take mem right limit in

     let mem = append mem left right in
     assert_invariants mem left;
     assert_dangling_pointers mem [left];

     let actual = to_list mem left in
     let expected = xs in
     assert_equal expected actual ~printer:show_element_list;
     true
    )
  ]
