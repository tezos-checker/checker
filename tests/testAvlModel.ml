open OUnit2
open Core_kernel.Deque
open LiquidationAuctionPrimitiveTypes

type queue_op =
    PushLeft of liquidation_slice     (* Actually places new element in back *)
  | PushRight of liquidation_slice    (* Actually places new element in front *)
  | Pop                (* Remove item from front of queue *)
  | Get of float         (* Read element at queue index i *)
  | Delete of float      (* Remove the element at queue index i *)
[@@deriving show]

(* ========================================================================= *)
(* Random inputs for QCheck *)
(* ========================================================================= *)
let tez_gen = QCheck.Gen.(
    map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) (0 -- max_int)
  )

let kit_gen = QCheck.Gen.(
    map (fun x -> Kit.kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) (0 -- max_int)
  )

let slice_gen = QCheck.Gen.(
    map (fun (tez, kit) ->
        let contents = {
          (* TODO: Use arbitrary addresses as well? *)
          burrow=TestCommon.alice_addr;
          tez=tez;
          min_kit_for_unwarranted=kit;
        } in
        {
          contents=contents;
          older=None;
          younger=None;
        }
      ) (pair tez_gen kit_gen)
  )

let op_gen = QCheck.Gen.(
    (int_range 0 4) >>= (
      fun i -> match i with
        | 0 -> map (fun slice -> PushLeft slice) slice_gen
        | 1 -> map (fun slice -> PushRight slice) slice_gen
        | 2 -> return Pop
        | 3 -> map (fun percent -> Get percent) (float_bound_inclusive 1.)
        | 4 -> map (fun percent -> Delete percent) (float_bound_inclusive 1.)
        | _ -> failwith "Generated more cases then their are in queue_op. Check the int_range bounds in op_gen."
    )
  )
let arb_op = QCheck.make op_gen

(* ========================================================================= *)
(* Helper functions *)
(* ========================================================================= *)

(* Helper for converting fractional indices to an actual index based on the length of the queue / list *)
let percent_to_index (percent:float) (indices:int list) =
  let i = Float.(
      to_int (round (mul percent (float_of_int ((List.length indices) - 1))))
    ) in
  List.nth indices i

(* TODO: The below is not efficient but was easy to write *)
let drop_last l = List.rev (List.tl (List.rev l))

(* ========================================================================= *)
(* Model queue *)
(* ========================================================================= *)
type model = liquidation_slice Core_kernel.Deque.t

let model_empty () = (Core_kernel.Deque.create (): (liquidation_slice Core_kernel.Deque.t))

(* Deletes the element with the provided index (if it exists) and returns a new queue and list of indices *)
let model_delete (queue) (index: int) : (model * int list) =
  let filter_index = fun (index: int) (cur_index:int) acc element ->
    let (queue, indices) = acc in
    if cur_index = index then acc
    else
      let _ = enqueue_front queue element in
      (queue, indices @ [front_index_exn queue])
  in
  let acc = ((model_empty ()), ([]: int list)) in
  (foldi' queue `back_to_front ~init:acc ~f:(filter_index index))

(* Pops liquidation slices up to the provided tez limit *)
let model_take (limit: Ligo.tez) (model: model) : (liquidation_slice list) =
  let _ = limit in
  let _ = model in
  failwith "not yet implemented"

(* ========================================================================= *)
(* Test *)
(* ========================================================================= *)

(* Apply the given operation to both the model and implementation *)
let apply_op ((impl: unit), (model: model),(impl_indices: leaf_ptr list), (model_indices: int list)) op =
  (* Re-enable this assertion once avl calls are implemented *)
  (* let _ = assert_equal (List.length impl_indices) (List.length model_indices) in *)

  match op with
  | PushLeft slice ->
    let _ = enqueue_back model slice in
    let i_model = back_index_exn model in
    (* let _ = Avl.avl_push impl in *)
    impl, model, [], [i_model] @ model_indices

  | PushRight slice ->
    let _ = enqueue_front model slice in
    let i_model = back_index_exn model in
    (impl, model, [], model_indices @ [i_model])

  | Pop ->
    let new_model_indices = match (dequeue_front model) with
      | None -> []
      | Some _ ->
        match model_indices with
        | [] -> []
        | _ -> drop_last model_indices
    in
    impl, model, impl_indices, new_model_indices

  | Get p ->
    let _ = (match model_indices with
        | [] -> ()
        | _ ->
          (* Get from model *)
          let _ = get model (percent_to_index p model_indices) in
          (* Get from implementation *)
          ()
      )
    in
    impl, model, impl_indices, model_indices


  | Delete p ->
    let new_model, new_model_indices = (match model_indices with
        | [] -> (model, model_indices)
        | _ ->
          (* Delete from model *)
          let new_model, new_model_indices = model_delete model (percent_to_index p model_indices) in
          (* Delete from implementation *)
          new_model, new_model_indices
      )
    in
    impl, new_model, impl_indices, new_model_indices



let show_model queue =
  let _ = Format.printf "[" in
  let _ = iteri queue ~f:(fun i e -> Format.printf "(%d, %s);" i (show_liquidation_slice e)) in
  let _ = Format.printf "]" in
  ()

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t
let suite =
  "AVL model check" >::: [

    qcheck_to_ounit
    @@ QCheck.Test.make
      ~name:"TODO"
      ~count:10
      (QCheck.small_list arb_op)
    @@ fun ops -> (
      let _ = List.iter (fun op -> Format.printf "%s@," (show_queue_op op)) ops in
      (* let _ = List.iter (fun x -> Format.printf "%d@ |," x) model_indices in *)
      (* let _ = Format.printf "<=== " in *)
      let model = model_empty () in

      (* Apply the operations *)
      let acc = ((), model, ([] : leaf_ptr list), ([]: int list)) in
      let _ = List.fold_left apply_op acc ops in

      (* let _ = show_model model in *)

      (* let _ = List.iter (fun s -> Format.printf "%s@," (show_liquidation_slice s)) e in *)
      (* let _ = Format.printf "===> " in *)
      (* let _ = failwith "END HERE." in *)
      true
    )

  ]
