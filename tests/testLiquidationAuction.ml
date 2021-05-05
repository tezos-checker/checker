open Kit
open OUnit2
open TestCommon
open LiquidationAuction
open LiquidationAuctionTypes
open LiquidationAuctionPrimitiveTypes

open Error
open Ratio
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t
let property_test_count = 100

let checker_address = Ligo.address_from_literal "checker"
let checker_amount = Ligo.tez_from_literal "0mutez"
let checker_sender = Ligo.address_from_literal "somebody"

type slice_content_list = liquidation_slice_contents list [@@deriving show]

(* ========================================================================= *)
(* QCheck arbitrary values *)
(* ========================================================================= *)
let gen_liquidation_slice_contents =
  QCheck.Gen.(
    map
      (fun (tz, adrr) ->
         LiquidationAuctionPrimitiveTypes.
           ({ tez = Ligo.tez_from_literal ((string_of_int tz) ^ "mutez")
            ; burrow = Ligo.address_of_string ("burrow_" ^ adrr )
            ; min_kit_for_unwarranted = Some kit_zero
            })
      )
      (* Note: The char range here controls how many possible burrow addresses this will generate *)
      (pair (int_range 0 max_int) (string_size ~gen:(char_range 'a' 'd') (return 1)))
  )

let gen_liquidation_slice_contents_list max_length=
  QCheck.Gen.(
    list_size (1--max_length) gen_liquidation_slice_contents
  )
(* ========================================================================= *)
(* Utils for working with liquidation slices and the linked list they form *)
(* ========================================================================= *)
let repeat n x =
  let rec append_duplicate max_length xs x =
    if (List.length xs) >= max_length then
      xs
    else
      append_duplicate max_length (List.append xs [x]) x
  in
  append_duplicate (n+1) [] x

let index_of_leaf auctions burrow_id leaf =
  let rec walk_with_index storage curr_index curr_leaf_ptr =
    if curr_leaf_ptr = leaf then
      curr_index
    else
      let slice = Avl.avl_read_leaf storage curr_leaf_ptr in
      let next_leaf = slice.older in
      match next_leaf with
      | None -> failwith "slice does not exist in this linked list!"
      | Some next_leaf_ptr -> walk_with_index storage (curr_index + 1) next_leaf_ptr
  in
  let burrow_slices = match Ligo.Big_map.find_opt burrow_id auctions.burrow_slices with
    | Some burrow_slices -> burrow_slices
    | None -> failwith ("No burrow_slices found for burrow_id " ^ (Ligo.string_of_address burrow_id))
  in
  walk_with_index auctions.avl_storage 0 burrow_slices.youngest_slice

(* Gets all of the slices associated with burrow as a list *)
let get_burrow_slices auctions burrow_id = match Ligo.Big_map.find_opt burrow_id auctions.burrow_slices with
  (* Note: returns slices in list where head is youngest slice *)
  | Some burrow_slices ->
    fold_burrow_slices ~direction:FromYoungest (fun acc s -> List.append acc [s]) [] auctions.avl_storage burrow_slices
  | None -> []

let suite =
  let burrow_id_1 = Ligo.address_of_string "burrow_1" in
  let burrow_id_2 = Ligo.address_of_string "burrow_2" in
  let burrow_id_3 = Ligo.address_of_string "burrow_3" in

  "Liquidation auction tests" >::: [

    ("liquidation_auction_send_to_auction - fails when the auction queue is full" >::
     fun _ ->  (
         (* Values in this slice are arbitrary and should not matter *)
         let slice = {
           burrow = burrow_id_1;
           tez = Ligo.tez_from_literal "4_000_000mutez";
           min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "1_000_000n"))
         } in
         (* 2 ^ (Constants.max_liquidation_queue_height - 2) *)
         let max_length = 1024 in
         let slices = repeat max_length slice in
         let auctions = List.fold_left
             (fun auctions slice_contents ->
                fst (liquidation_auction_send_to_auction auctions slice_contents)
             )
             liquidation_auction_empty
             slices
         in

         assert_raises (Failure (Ligo.string_of_int error_LiquidationQueueTooLong)) (
           fun () -> liquidation_auction_send_to_auction auctions slice
         )
       )
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"liquidation_auction_send_to_auction - preserves list properties"
        ~count:property_test_count
        (QCheck.make (gen_liquidation_slice_contents_list 100))
      @@
      fun slice_contents_list ->  (
        let _ = List.fold_left
            (fun auctions slice_contents ->
               (* Send this slice to auction *)
               let auctions_out, new_slice = liquidation_auction_send_to_auction auctions slice_contents in

               (* Gather data for assertions *)
               let slices_in = get_burrow_slices auctions slice_contents.burrow in
               let slices_out = get_burrow_slices auctions_out slice_contents.burrow in
               let index_new_slice = index_of_leaf auctions_out slice_contents.burrow new_slice in
               let slices_without_new_one = List.filteri (fun i _ -> if i == index_new_slice then false else true) slices_out in

               assert_liquidation_auction_invariants auctions;
               (* We should have inserted one element into our original list *)
               assert_equal ((List.length slices_in) + 1) (List.length slices_out) ~printer:string_of_int;
               (* Removing the new element from the list should produce the input list *)
               (* Note: comparing by content since the pointers will change when inserting *)
               assert_equal slices_in slices_without_new_one ~printer:show_slice_content_list;
               auctions_out
            )
            liquidation_auction_empty
            slice_contents_list
        in
        true
      )
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"liquidation_auctions_cancel_slice - preserves list properties"
        ~count:property_test_count
        (QCheck.make (QCheck.Gen.(pair (gen_liquidation_slice_contents_list 100) (float_bound_inclusive 1.))))
      @@
      fun (slice_contents_list, percent) ->  (
        (* Cancelling a random element from the slice list *)
        let i_to_pop = Float.(
            to_int (round (mul percent (float_of_int ((List.length slice_contents_list) - 1))))
          ) in
        (* Populate the liquidation slice list, noting the pointer of the element we want to pop *)
        let auctions, _, slice_to_pop = List.fold_left
            (fun (a, curr_length, slice_to_pop) slice_contents ->
               let auctions_out, new_slice = liquidation_auction_send_to_auction a slice_contents in
               if curr_length = i_to_pop then
                 auctions_out, curr_length + 1, Some new_slice
               else
                 auctions_out, curr_length + 1, slice_to_pop
            )
            (liquidation_auction_empty, 0, None)
            slice_contents_list
        in
        let slice_to_pop = match slice_to_pop with
          | Some p -> p
          | None -> failwith "did not find the expected index when populating the slice lists"
        in

        (* Pop the slice from the list *)
        let popped_contents, auctions_out = liquidation_auctions_cancel_slice auctions slice_to_pop in

        (* Run assertions on all burrow slice lists*)
        let _ = List.map (
            fun (burrow, _) ->
              (* Read data from the list for assertions *)
              let slices_in = get_burrow_slices auctions burrow in
              let slices_out = get_burrow_slices auctions_out burrow in
              let expected_slices =
                (* If the popped slice belongs to this burrow, we don't expect to find it in the output *)
                if burrow = popped_contents.burrow then
                  let i_to_pop = index_of_leaf auctions burrow slice_to_pop in
                  (* Note: would have used List.filteri here, but it doesn't exist in OCaml 4.10 *)
                  let _, filtered = List.fold_left (fun (i, xs) x -> if i == i_to_pop then (i+1, xs) else (i+1, List.append xs [x])) (0, []) slices_in in
                  filtered
                else
                  (* Otherwise the output should just be the input *)
                  slices_in
              in

              assert_liquidation_auction_invariants auctions;
              assert_liquidation_auction_invariants auctions_out;
              assert_equal popped_contents (List.nth slice_contents_list i_to_pop);
              assert_equal ((List.length expected_slices)) (List.length slices_out) ~printer:string_of_int;
              assert_equal expected_slices slices_out ~printer:show_slice_content_list
          )
            (Ligo.Big_map.bindings auctions_out.burrow_slices)
        in
        true
      )
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"liquidation_auctions_pop_completed_slice - preserves list properties"
        ~count:property_test_count
        (QCheck.make (QCheck.Gen.(pair (gen_liquidation_slice_contents_list 100) (float_bound_inclusive 1.))))
      @@
      fun (slice_contents_list, percent) ->  (
        (* Cancelling a random element from the slice list *)
        let i_to_pop = Float.(
            to_int (round (mul percent (float_of_int ((List.length slice_contents_list) - 1))))
          ) in
        (* Populate the liquidation slice list, noting the pointer of the element we want to pop *)
        let auctions, _, slice_to_pop = List.fold_left
            (fun (a, curr_length, slice_to_pop) slice_contents ->
               let auctions_out, new_slice = liquidation_auction_send_to_auction a slice_contents in
               if curr_length = i_to_pop then
                 (* Ensure that the slice is associated with a completed auction *)
                 let auction_ptr = (Avl.avl_find_root auctions_out.avl_storage new_slice) in
                 let avl_out = Avl.avl_modify_root_data auctions_out.avl_storage auction_ptr
                     (fun outcome -> match outcome with
                        | _ -> Some {
                            sold_tez = Ligo.tez_from_literal "1mutez";
                            winning_bid = {address=Ligo.address_of_string "someone" ; kit=kit_of_mukit (Ligo.nat_from_literal "1_000_000n")};
                            younger_auction = None;
                            older_auction = None;
                          }
                     ) in
                 let completed_auctions = Some {youngest=auction_ptr; oldest=auction_ptr} in
                 {auctions_out with avl_storage=avl_out; completed_auctions=completed_auctions}, curr_length + 1, Some new_slice
               else
                 auctions_out, curr_length + 1, slice_to_pop
            )
            (liquidation_auction_empty, 0, None)
            slice_contents_list
        in
        let slice_to_pop = match slice_to_pop with
          | Some p -> p
          | None -> failwith "did not find the expected index when populating the slice lists"
        in

        (* Pop the slice from the list *)
        let popped_contents, _, auctions_out = liquidation_auctions_pop_completed_slice auctions slice_to_pop in

        (* Run assertions on all burrow slice lists*)
        let _ = List.map (
            fun (burrow, _) ->
              (* Read data from the list for assertions *)
              let slices_in = get_burrow_slices auctions burrow in
              let slices_out = get_burrow_slices auctions_out burrow in
              let expected_contents =
                if burrow = popped_contents.burrow then
                  let i_to_pop = index_of_leaf auctions burrow slice_to_pop in
                  let _, filtered = List.fold_left (fun (i, xs) x -> if i == i_to_pop then (i+1, xs) else (i+1, List.append xs [x])) (0, []) slices_in in
                  filtered
                else
                  slices_in
              in

              assert_liquidation_auction_invariants auctions;
              assert_liquidation_auction_invariants auctions_out;
              assert_equal popped_contents (List.nth slice_contents_list i_to_pop);
              assert_equal ((List.length expected_contents)) (List.length slices_out) ~printer:string_of_int;
              assert_equal expected_contents slices_out ~printer:show_slice_content_list
          )
            (Ligo.Big_map.bindings auctions_out.burrow_slices)
        in
        true
      )
    );

    ("test starts descending auction" >::
     fun _ ->
       Ligo.Tezos.reset();
       let auctions = liquidation_auction_empty in
       let (auctions, _) =
         liquidation_auction_send_to_auction auctions {
           burrow = burrow_id_1;
           tez = Ligo.tez_from_literal "2_000_000mutez";
           min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "4_000_000n")); (* note: randomly chosen *)
         } in
       let start_price = one_ratio in
       let auctions = liquidation_auction_touch auctions start_price in
       let current = Option.get auctions.current_auction in
       assert_equal
         (Some (Ligo.tez_from_literal "2_000_000mutez"))
         (liquidation_auction_current_auction_tez auctions);
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "2_000_000n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "2_000_000n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
       (* Price of descending auction should go down... *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "1_999_666n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "1_999_333n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
       Ligo.Tezos.new_transaction ~seconds_passed:58 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "1_980_098n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "1_960_394n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
    );

    ("test batches up auction lots" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auctions = liquidation_auction_empty in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "9_000_001n")); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "9_000_002n")); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "9_000_003n")); (* note: randomly chosen *)
           } in
       let start_price = one_ratio in
       let auctions = liquidation_auction_touch auctions start_price in
       assert_equal (Some (Ligo.tez_from_literal "10_000_000_000mutez")) (liquidation_auction_current_auction_tez auctions);
    );

    ("test splits up auction lots to fit batch size" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auctions = liquidation_auction_empty in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Ligo.tez_from_literal "4_000_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "9_000_004n")); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "9_000_005n")); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Ligo.tez_from_literal "3_000_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "9_000_006n")); (* note: randomly chosen *)
           } in
       let start_price = one_ratio in
       let auctions = liquidation_auction_touch auctions start_price in
       assert_equal (Some (Ligo.tez_from_literal "10_000_000_000mutez")) (liquidation_auction_current_auction_tez auctions);
    );

    ("test bidding" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auctions = liquidation_auction_empty in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Ligo.tez_from_literal "2_000_000mutez";
             min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "4_000_007n")); (* note: randomly chosen *)
           } in
       let start_price = one_ratio in
       let auctions = liquidation_auction_touch auctions start_price in
       let bidder = Ligo.address_from_literal "23456" in
       let current = Option.get auctions.current_auction in

       (* Below minimum bid *)
       assert_raises
         (Failure (Ligo.string_of_int error_BidTooLow))
         (fun () -> liquidation_auction_place_bid current { address = bidder; kit = kit_of_mukit (Ligo.nat_from_literal "1_000_000n"); });
       (* Right below minimum bid *)
       assert_raises
         (Failure (Ligo.string_of_int error_BidTooLow))
         (fun () -> liquidation_auction_place_bid current { address = bidder; kit = kit_of_mukit (Ligo.nat_from_literal "1_999_999n"); });
       (* On/Above minimum bid, we get a bid ticket and our bid plus 0.33 cNp becomes the new minimum bid *)
       let (current, _) = liquidation_auction_place_bid current { address = bidder; kit = kit_of_mukit (Ligo.nat_from_literal "2_000_000n"); } in
       assert_equal
         (kit_of_mukit (Ligo.nat_from_literal "2_006_599n"))
         (liquidation_auction_current_auction_minimum_bid current)
         ~printer:show_kit;
       (* Minimum bid does not drop over time *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* Can increase the bid.*)
       let (current, _) = liquidation_auction_place_bid current {address=bidder; kit=kit_of_mukit (Ligo.nat_from_literal "4_000_000n")} in
       (* Does not allow a lower bid.*)
       assert_raises
         (Failure (Ligo.string_of_int error_BidTooLow))
         (fun () -> liquidation_auction_place_bid current {address=bidder; kit=kit_of_mukit (Ligo.nat_from_literal "3_000_000n")});

       ()
    );

    qcheck_to_ounit
    @@ QCheck.Test.make
      ~name:"bid can not be zero"
      ~count:property_test_count
      QCheck.(0 -- 100)
    @@ fun blocks_passed ->

    (* Create an auction with one slice *)
    Ligo.Tezos.reset ();
    let auctions = liquidation_auction_empty in
    let (auctions, _) =
      liquidation_auction_send_to_auction
        auctions
        (* Note: The amounts don't matter here. We are only interested in the bidding logic *)
        { burrow = burrow_id_1; tez = Ligo.tez_from_literal "1_000_000mutez";
          min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "1n")); (* note: randomly chosen *)
        } in
    let start_price = one_ratio in
    let auctions = liquidation_auction_touch auctions start_price in
    let current = Option.get auctions.current_auction in

    Ligo.Tezos.new_transaction ~seconds_passed:(blocks_passed*3600) ~blocks_passed:blocks_passed ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_BidTooLow))
      (fun () -> liquidation_auction_place_bid current {address=Ligo.address_from_literal "12345"; kit=kit_zero});
    ();
    true

  ]
