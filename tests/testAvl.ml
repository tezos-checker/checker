open Mem
open LiquidationAuctionPrimitiveTypes
open Avl
open Kit
open OUnit2
open Format
open Ptr

type auction_outcome_option = auction_outcome option [@@deriving show]
type liquidation_slice_list = liquidation_slice list [@@deriving show]
type slice_option = liquidation_slice option [@@deriving show]

let nTez (i: int) : Ligo.tez = Ligo.tez_from_literal (string_of_int (1_000_000 * i) ^ "mutez")

let add_all (mem: mem) (root: avl_ptr) (xs: liquidation_slice list)
  : mem =
  List.fold_left
    (fun mem value ->
       let (mem, _) = avl_push mem root value Left in
       mem)
    mem
    xs

let debug_avl (mem: mem) (AVLPtr root) : unit =
  let rec go curr =
    let indent str = "  " ^ String.concat "\n  " (String.split_on_char '\n' str) in
    sprintf "%s: " (show_ptr curr) ^
    match Mem.mem_get mem curr with
    | Root (None, r) -> "Root(" ^ show_auction_outcome_option r ^ ") Empty"
    | Root (Some r, r') -> "Root(" ^ show_auction_outcome_option r' ^ ")\n" ^ indent (go r)
    | Leaf leaf ->
      sprintf "Leaf { value: %s; parent: %s }"
        (show_liquidation_slice leaf.value) (show_ptr leaf.parent)
    | Branch branch ->
      "Branch " ^ show_branch branch ^ "\n"
      ^ indent ("Left:\n" ^ indent (go branch.left)) ^ "\n"
      ^ indent ("Right:\n" ^ indent (go branch.right));
  in printf "%s\n" (go root)

let add_all_debug (mem: mem) (root: avl_ptr) (xs: liquidation_slice list)
  : mem =
  List.fold_left
    (fun mem value ->
       print_string "--------------------------------\n";
       print_string ("Inserting: " ^ show_liquidation_slice value ^ "\n");
       let (mem, _) = avl_push mem root value Left in
       debug_avl mem root;
       assert_avl_invariants mem root;
       print_newline ();
       mem)
    mem
    xs

let avl_to_list (mem: mem) (AVLPtr ptr) : liquidation_slice list =
  let rec go ptr: liquidation_slice list =
    match Mem.mem_get mem ptr with
    | Root (None, _) -> []
    | Root (Some ptr, _) -> go ptr
    | Leaf leaf ->
      [leaf.value]
    | Branch branch ->
      List.append (go branch.left) (go branch.right) in
  go ptr

let avl_from_list (mem: mem) (root_data: auction_outcome option) (elements: liquidation_slice list)
  : mem * avl_ptr =
  let (mem, root) = avl_mk_empty mem root_data in
  (add_all mem root elements, root)

let mk_liquidation_slice (n: int): liquidation_slice =
  { contents =
      { tez = Ligo.tez_from_literal (string_of_int n ^ "mutez")
      ; min_kit_for_unwarranted = Some kit_zero
      ; burrow = Ligo.address_of_string "someburrow"
      }
  ; older = None
  ; younger = None
  }

let rec range (f: int) (t: int) =
  if f >= t
  then []
  else f :: range (f+1) t

(* More straightforward recursive implementations tend to overflow stack on large inputs.
 * So here's an iterative implementation which returns a lazy stream. I don't know how
 * this works, I just translated below link to Ocaml.
 * https://en.wikipedia.org/wiki/Heap%27s_algorithm#Details_of_the_algorithm *)
let permutations (xs: 't list): ('t list) Stream.t =
  let a = Array.of_list xs in
  let n = Array.length a in
  let c = Array.make n 0 in
  let i = ref (-1) in

  let swap i1 i2 =
    let tmp = a.(i1) in
    a.(i1) <- a.(i2);
    a.(i2) <- tmp in

  let rec loop () =
    if !i < n
    then (
      if c.(!i) < !i
      then (
        (if !i mod 2 = 0
         then swap 0 !i
         else swap c.(!i) !i);
        c.(!i) <- c.(!i) + 1;
        i := 0;
        Some (Array.to_list a)
      ) else (
        c.(!i) <- 0;
        i := !i + 1;
        loop ()
      )
    ) else None in

  Stream.from (fun _ ->
      if !i = -1
      then (i := 0; Some (Array.to_list a))
      else loop ())

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module IntSet = Set.Make(Int)

let property_test_count = 100

let suite =
  "AVLTests" >::: [
    "test_push_back_singleton" >::
    (fun _ ->
       let (mem, root) = avl_mk_empty mem_empty None in
       let (mem, _) = avl_push mem root (mk_liquidation_slice 0) Left in
       let actual = avl_to_list mem root in
       let expected = [mk_liquidation_slice 0] in
       assert_equal expected actual ~printer:show_liquidation_slice_list);

    "test_push_front_singleton" >::
    (fun _ ->
       let (mem, root) = avl_mk_empty mem_empty None in
       let (mem, _) = avl_push mem root (mk_liquidation_slice 0) Right in
       let actual = avl_to_list mem root in
       let expected = [mk_liquidation_slice 0] in
       assert_equal expected actual);

    "test_from_list" >::
    (fun _ ->
       let elements =
         (List.map mk_liquidation_slice
            [ 1; 2; 3; 4; 5; 6; 7; 8; ]) in
       let (mem, root) = avl_from_list mem_empty None elements in
       assert_avl_invariants mem root;
       avl_assert_dangling_pointers mem [root];

       let actual = avl_to_list mem root in
       assert_equal elements actual ~printer:show_liquidation_slice_list);

    "test_pop_front_empty" >::
    (fun _ ->
       let (mem, root) = avl_from_list mem_empty None [] in
       let (mem, x) = avl_pop_front mem root in
       assert_equal [] (avl_to_list mem root) ~printer:show_liquidation_slice_list;
       assert_equal None (Option.map snd x) ~printer:show_slice_option;
    );

    "test_pop_front" >::
    (fun _ ->
       let elements = [ mk_liquidation_slice 1; mk_liquidation_slice 2 ] in
       let (mem, root) = avl_from_list mem_empty None elements in
       let (mem, x) = avl_pop_front mem root in
       assert_equal [ mk_liquidation_slice 2 ] (avl_to_list mem root) ~printer:show_liquidation_slice_list;
       assert_equal (Some (mk_liquidation_slice 1)) (Option.map snd x) ~printer:show_slice_option;
       let (mem, x) = avl_pop_front mem root in
       assert_equal [] (avl_to_list mem root) ~printer:show_liquidation_slice_list;
       assert_equal (Some (mk_liquidation_slice 2)) (Option.map snd x) ~printer:show_slice_option;
    );

    "test_del_singleton" >::
    (fun _ ->
       Mem.reset_ops ();
       let (mem, root) = avl_mk_empty mem_empty None in
       let (mem, elem) = avl_push mem root (mk_liquidation_slice 1) Left in
       let (mem, root_) = avl_del mem elem in
       assert_equal root root_;
       assert_equal [] (avl_to_list mem root);
       assert_equal 1 (List.length (Mem.mem_bindings mem))
    );

    "test_del" >::
    (fun _ ->
       let fst_elements =
         (List.map mk_liquidation_slice [ 1; 2; 3; 4; 5 ]) in
       let mid = mk_liquidation_slice 6 in
       let snd_elements =
         (List.map mk_liquidation_slice [ 7; 8; 9 ]) in

       let (mem, root) = avl_from_list mem_empty None fst_elements in
       let (mem, elem) = avl_push mem root mid Left in
       let mem = add_all mem root snd_elements in

       assert_avl_invariants mem root;
       avl_assert_dangling_pointers mem [root];

       let (mem, root_) = avl_del mem elem in
       assert_equal root root_;

       assert_avl_invariants mem root;
       avl_assert_dangling_pointers mem [root];

       let actual = avl_to_list mem root in
       let expected = fst_elements @ snd_elements in
       assert_equal expected actual ~printer:show_liquidation_slice_list);

    "test_empty_from_list_to_list" >::
    (fun _ ->
       let elements = [] in
       let (mem, root) = avl_from_list mem_empty None elements in
       let actual = avl_to_list mem root in
       let expected = [] in
       assert_equal expected actual);

    (qcheck_to_ounit
     @@ QCheck.Test.make ~name:"prop_from_list_to_list" ~count:property_test_count (QCheck.list TestArbitrary.arb_liquidation_slice)
     @@ fun xs ->
     let (mem, root) = avl_from_list mem_empty None xs in
     assert_avl_invariants mem root;
     avl_assert_dangling_pointers mem [root];

     let actual = avl_to_list mem root in

     assert_equal xs actual ~printer:show_liquidation_slice_list;
     true
    );

    (qcheck_to_ounit
     @@ QCheck.Test.make ~name:"prop_del" ~count:property_test_count
       QCheck.(triple (list TestArbitrary.arb_liquidation_slice) TestArbitrary.arb_liquidation_slice (list TestArbitrary.arb_liquidation_slice))
     @@ fun (left_items, mid_item, right_items) ->

     let (mem, root) = avl_from_list mem_empty None left_items in
     assert_avl_invariants mem root;

     let (mem, to_del) = avl_push mem root mid_item Left in
     assert_avl_invariants mem root;

     let mem = add_all mem root right_items in
     assert_avl_invariants mem root;

     let (mem, root_) = avl_del mem to_del in
     assert_equal root root_;
     (*
     printf "- %s %s %s ----------\n"
       (show_liquidation_slice_list left_items)
       (show_liquidation_slice_list [mid_item])
       (show_liquidation_slice_list right_items);
     debug_avl mem root;
     *)
     assert_avl_invariants mem root;

     let actual = avl_to_list mem root in
     let expected = left_items @ right_items in

     assert_equal expected actual ~printer:show_liquidation_slice_list;
     true
    );
    (qcheck_to_ounit

     @@ QCheck.Test.make ~name:"prop_append" ~count:property_test_count QCheck.(pair (list TestArbitrary.arb_liquidation_slice) (list TestArbitrary.arb_liquidation_slice))
     @@ fun (ls, rs) ->

     let (mem, left_tree) = avl_from_list mem_empty None ls in
     let (mem, right_tree) = avl_from_list mem None rs in

     let mem = avl_append mem left_tree right_tree in

     assert_avl_invariants mem left_tree;
     avl_assert_dangling_pointers mem [left_tree];

     let actual = avl_to_list mem left_tree in
     let expected = ls @ rs in
     assert_equal expected actual ~printer:show_liquidation_slice_list;
     true
    );
    (qcheck_to_ounit
     @@ QCheck.Test.make ~name:"prop_take" ~count:property_test_count QCheck.(pair TestArbitrary.arb_tez (list TestArbitrary.arb_liquidation_slice))
     @@ fun (limit, xs) ->

     let (mem, right) = avl_from_list mem_empty None xs in
     let (mem, left) = avl_take mem right limit None in

     assert_avl_invariants mem left;
     assert_avl_invariants mem right;
     avl_assert_dangling_pointers mem [left; right];

     let actual_left = avl_to_list mem left in
     let actual_right = avl_to_list mem right in

     let rec split_list (lim: Ligo.tez) (xs: liquidation_slice list) =
       match xs with
       | [] -> ([], [])
       | x :: xs ->
         if x.contents.tez <= lim
         then
           match split_list (Ligo.sub_tez_tez lim x.contents.tez) xs with
             (l, r) -> (x::l, r)
         else
           ([], x::xs)
     in

     let (expected_left, expected_right) = split_list limit xs in

     assert_equal
       expected_left
       actual_left
       ~printer:show_liquidation_slice_list;

     assert_equal
       expected_right
       actual_right
       ~printer:show_liquidation_slice_list;

     true
    );
    (qcheck_to_ounit
     @@ QCheck.Test.make ~name:"prop_take_append" ~count:property_test_count QCheck.(pair TestArbitrary.arb_tez (list TestArbitrary.arb_liquidation_slice))
     @@ fun (limit, xs) ->

     let (mem, right) = avl_from_list mem_empty None xs in
     let (mem, left) = avl_take mem right limit None in

     let mem = avl_append mem left right in
     assert_avl_invariants mem left;
     avl_assert_dangling_pointers mem [left];

     let actual = avl_to_list mem left in
     let expected = xs in
     assert_equal expected actual ~printer:show_liquidation_slice_list;
     true
    );
    "bench_large_ops" >::
    (fun _ ->
       let (mem, root) = avl_mk_empty mem_empty None in

       let rec go i mem =
         if i <= 0
         then mem
         else
           let (mem, _) = avl_push mem root (mk_liquidation_slice i) Left in
           go (i-1) mem in

       let mem = go 100_000 mem in
       assert_avl_invariants mem root;
       avl_assert_dangling_pointers mem [root];

       Mem.reset_ops ();
       let _ = avl_take mem root (Ligo.tez_from_literal "50_000mutez") None in

       assert_equal
         {reads=104; writes=69}
         !Mem.ops
         ~printer:Mem.show_ops;
    );
    "test_all_permutations" >::
    (fun _ ->
       range 0 10
       |> permutations
       |> Stream.iter (fun xs ->
           let xs = List.map mk_liquidation_slice xs in
           let (mem, root) = avl_from_list mem_empty None xs in
           assert_avl_invariants mem root;
           let actual = avl_to_list mem root in
           assert_equal actual xs
         )
    );
  ]
