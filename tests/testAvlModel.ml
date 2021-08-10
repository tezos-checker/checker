(* Tests to check whether the double ended queue defined in avl.ml produces
   the same results as a model implementation.
*)
open OUnit2
open Core_kernel.Deque
open LiquidationAuctionPrimitiveTypes
open TestLib
open Tok

type queue_op =
  (* Place new element in back *)
    PushBack of liquidation_slice
  (* Place new element in front of queue *)
  | PushFront of liquidation_slice
  (* Remove item from front of queue *)
  | Pop
  (* Read element at queue index i, expressed as a percentage of the queue length *)
  | Get of float
  (* Remove the element at queue index i, expressed as a percentage of the queue length  *)
  | Delete of float
  (* Take slices from the front of the queue up to the specified limit *)
  | Take of tok
[@@deriving show]

type slice_option = liquidation_slice option [@@deriving show]

(* ========================================================================= *)
(* Random inputs for QCheck *)
(* ========================================================================= *)

let addr_gen = QCheck.Gen.(
    map (fun x -> Ligo.address_of_string x) (string_size (return 36))
  )

(** Generate a random tez amount that does not exceed 10Ktez. This size should
  * be sufficient to capture realistic tez amounts, and is far enough from
  * [Int64.max_int] to be safe from overflows. *)
let tok_gen : tok QCheck.Gen.t = QCheck.Gen.(
    map (fun x -> tok_of_denomination (Ligo.nat_from_literal ((string_of_int x) ^ "n"))) (0 -- 10_000_000_000)
  )

let kit_gen = QCheck.Gen.(
    map (fun x -> Kit.kit_of_denomination (Ligo.nat_from_literal (string_of_int x ^ "n"))) (0 -- max_int)
  )

let slice_gen = QCheck.Gen.(
    map (fun (addr, tok, kit) ->
        let contents = {
          burrow=(addr, Ligo.nat_from_literal "0n");
          tok=tok;
          min_kit_for_unwarranted=Some kit;
        } in
        {
          contents=contents;
          older=None;
          younger=None;
        }
      ) (triple addr_gen tok_gen kit_gen)
  )

let op_gen = QCheck.Gen.(
    (int_range 0 5) >>= (
      fun i -> match i with
        | 0 -> map (fun slice -> PushBack slice) slice_gen
        | 1 -> map (fun slice -> PushFront slice) slice_gen
        | 2 -> return Pop
        | 3 -> map (fun percent -> Get percent) (float_bound_inclusive 1.)
        | 4 -> map (fun percent -> Delete percent) (float_bound_inclusive 1.)
        | 5 -> map (fun tok -> Take (tok_of_denomination (Ligo.nat_from_literal (string_of_int tok ^ "n")))) (int_range 0 max_int)
        | _ -> failwith "Generated more cases than there are in queue_op. Check the int_range bounds in op_gen."
    )
  )
let arb_op = QCheck.make op_gen


(* ========================================================================= *)
(* Model queue *)
(* ========================================================================= *)
type model = liquidation_slice Core_kernel.Deque.t

let show_model queue =
  let _ = Format.printf "[" in
  let _ = iteri queue ~f:(fun i e -> Format.printf "(%d, %s);" i (show_liquidation_slice e)) in
  let _ = Format.printf "]" in
  ()

let model_empty () = (Core_kernel.Deque.create (): (liquidation_slice Core_kernel.Deque.t))

(* Deletes the element with the provided index (if it exists) and returns a new queue and
 *  list of indices.
*)
let model_delete (queue: model) (index: int) : model =
  let filter_index = fun (index: int) (cur_index:int) acc element ->
    if cur_index = index then acc
    else
      let _ = enqueue_front acc element in acc
  in
  let acc = model_empty () in
  (foldi' queue `back_to_front ~init:acc ~f:(filter_index index))

(* Pops liquidation slices until the resulting list contains slices with
 *  total tez >= the provided limit
*)
let model_take (limit: tok) (model: model) : (liquidation_slice list) =
  let rec rec_model_take limit current_total current_slices current_model =
    if geq_tok_tok current_total limit then
      current_slices
    else
      match peek_front current_model with
      | None -> current_slices
      | Some slice ->
        let total = tok_add current_total slice.contents.tok in
        if gt_tok_tok total limit then
          current_slices
        else
          rec_model_take limit total (List.append current_slices [dequeue_front_exn current_model]) current_model
  in
  rec_model_take limit tok_zero [] model

(* ========================================================================= *)
(* Helper functions *)
(* ========================================================================= *)

(* Helper for converting fractional indices to an actual index based on the length of the queue / list *)
let percent_to_index (percent:float) (indices:'a list) =
  let i = Float.(
      to_int (round (mul percent (float_of_int ((List.length indices) - 1))))
    ) in
  List.nth indices i

let rec avl_foldli (impl: Mem.mem * avl_ptr) (acc: 'b) (f: leaf_ptr -> 'b -> liquidation_slice -> 'b) : 'b =
  let mem, root_ptr = impl in
  let new_mem, element = Avl.avl_pop_front mem root_ptr in match element with
  | None -> acc
  | Some (leaf_ptr, slice) -> avl_foldli (new_mem, root_ptr) (f leaf_ptr acc slice) f

let rec avl_foldl (impl: Mem.mem * avl_ptr) (acc: 'b) (f: 'b -> liquidation_slice -> 'b) : 'b =
  let mem, root_ptr = impl in
  let new_mem, element = Avl.avl_pop_front mem root_ptr in match element with
  | None -> acc
  | Some (_, slice) -> avl_foldl (new_mem, root_ptr) (f acc slice) f

(* Note: head of indices should be the front of the queue *)
let model_indices model = foldi' model `front_to_back ~init:[] ~f:(fun index acc _ -> List.append acc [index])
let avl_indices impl = avl_foldli impl [] (fun index acc _ -> List.append acc [index])

(* Reads all of the items in the model and implementation queues, returning them
   as a list. *)
let get_all_elements (impl: Mem.mem * avl_ptr) (model: model) =
  (* Note: confirmed that to_list and foldl here iterate in the same order *)
  let impl_elements = avl_foldl impl [] (fun acc e -> List.append acc [e]) in
  let model_elements = to_list model in
  impl_elements, model_elements

(* ========================================================================= *)
(* Test *)
(* ========================================================================= *)
let apply_op ((impl: Mem.mem * avl_ptr), (model: model)) op =
  (* Note: Explicitely gathering indices here to support randomized calls
   * to methods which take an index as an input.
  *)
  let impl_indices = avl_indices impl in
  let model_indices = model_indices model in

  let mem, root_ptr = impl in
  match op with
  | PushBack slice ->
    (* Push to model *)
    let _ = enqueue_back model slice in
    (* Push to implementation *)
    let mem, _ = Avl.avl_push_back mem root_ptr slice in
    (mem, root_ptr), model

  | PushFront slice ->
    (* Push to model *)
    let _ = enqueue_front model slice in
    (* Push to implementation *)
    let mem, _ = Avl.avl_push_front mem root_ptr slice in
    (mem, root_ptr), model

  | Pop ->
    (* Pop from front of model *)
    let popped_model = dequeue_front model in
    (* Pop from front of implementation *)
    let mem, popped = Avl.avl_pop_front mem root_ptr in
    let popped_impl = match popped with
      | None -> None
      | Some (_, slice) -> Some slice
    in
    assert_slice_option_equal ~expected:popped_model ~real:popped_impl;
    (mem, root_ptr), model

  | Get p ->
    let _ = (match model_indices with
        (* Note: Doing nothing in cases where the queue is empty *)
        | [] -> ()
        | _ ->
          (* Get from model *)
          let got_model = get model (percent_to_index p model_indices) in
          (* Get from implementation *)
          let got_impl = Avl.avl_read_leaf mem (percent_to_index p impl_indices) in
          assert_equal got_model got_impl ~printer:show_liquidation_slice
      )
    in
    impl, model

  | Delete p -> (match model_indices with
      (* Note: Doing nothing in cases where the queue is empty *)
      | [] -> impl, model
      | _ ->
        (* Delete from model *)
        let new_model = model_delete model (percent_to_index p model_indices) in
        (* Delete from implementation *)
        let new_impl = Avl.avl_del mem (percent_to_index p impl_indices) in
        new_impl, new_model
    )

  | Take tez_limit ->
    (* Take slices from front of model *)
    let slices_model = model_take tez_limit model in
    let new_mem, split_ptr = Avl.avl_take mem root_ptr tez_limit None in
    (* Take slices from front of implementation *)
    let slices_impl = avl_foldl (new_mem, split_ptr) [] (fun acc e -> List.append acc [e]) in
    assert_liquidation_slice_list_equal ~expected:slices_model ~real:slices_impl;
    (new_mem, root_ptr), model

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t
let suite =
  "AVL model check" >::: [

    qcheck_to_ounit
    @@ QCheck.Test.make
      ~name:"Does AVL behave like a model double-ended queue? \u{1F914}"
      (* FIXME: Increase this count once long running tests are moved to their own suite *)
      ~count:100
      (QCheck.list arb_op)
    @@ fun ops -> (
      let init_model = model_empty () in
      let init_impl = Avl.avl_mk_empty Mem.mem_empty None in

      (* Apply the operations *)
      let acc = (init_impl, init_model) in
      let _, _ = List.fold_left (
          fun acc op ->
            (* Wrap call to apply_op with assertion(s) *)
            let applied = apply_op acc op in
            let impl, model = applied in
            let impl_elements, model_elements = get_all_elements impl model in
            (* Items in implementation queue must match items in model queue. *)
            assert_liquidation_slice_list_equal
              ~expected:model_elements
              ~real:impl_elements;
            let _ = Avl.assert_avl_invariants (fst impl) (snd impl) in
            applied
        ) acc ops in
      true
    );
  ]

let () =
  run_test_tt_main
    suite
