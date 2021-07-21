open Kit
open OUnit2
open TestLib
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

(* ========================================================================= *)
(* QCheck arbitrary values *)
(* ========================================================================= *)
let gen_liquidation_slice_contents =
  QCheck.Gen.(
    map
      (fun (tz, adrr) ->
        LiquidationAuctionPrimitiveTypes.
          {
            tez = Ligo.tez_from_literal (string_of_int tz ^ "mutez");
            burrow =
              ( Ligo.address_of_string ("burrow_" ^ adrr),
                Ligo.nat_from_literal "1n" );
            min_kit_for_unwarranted = Some kit_zero;
          })
      (* Note: The char range here controls how many possible burrow addresses this will generate *)
      (pair
         (int_range 0 10_000_000_000)
         (string_size ~gen:(char_range 'a' 'd') (return 1))))

let gen_liquidation_slice_contents_list max_length =
  QCheck.Gen.(list_size (1 -- max_length) gen_liquidation_slice_contents)

(* ========================================================================= *)
(* Utils for working with liquidation slices and the linked list they form *)
(* ========================================================================= *)

(* Repeats input x n times *)
let repeat n x =
  let rec append_duplicate max_length xs x =
    if List.length xs >= max_length then xs
    else append_duplicate max_length (List.append xs [ x ]) x
  in
  append_duplicate (n + 1) [] x

(* Calculates the integer index of a given burrow slice pointer in the burrow_slices list *)
let index_of_leaf auctions burrow_id leaf =
  let rec walk_with_index storage curr_index curr_leaf_ptr =
    if curr_leaf_ptr = leaf then curr_index
    else
      let slice = Avl.avl_read_leaf storage curr_leaf_ptr in
      let next_leaf = slice.older in
      match next_leaf with
      | None -> failwith "slice does not exist in this linked list!"
      | Some next_leaf_ptr ->
          walk_with_index storage (curr_index + 1) next_leaf_ptr
  in
  let burrow_slices =
    match Ligo.Big_map.find_opt burrow_id auctions.burrow_slices with
    | Some burrow_slices -> burrow_slices
    | None -> failwith "No burrow_slices found for burrow."
  in
  walk_with_index auctions.avl_storage 0 burrow_slices.youngest_slice

(* Gets all of the slices associated with burrow as a list *)
let get_burrow_slices auctions burrow_id =
  match Ligo.Big_map.find_opt burrow_id auctions.burrow_slices with
  (* Note: returns slices in a list where head is youngest slice *)
  | Some burrow_slices ->
      fold_burrow_slices ~direction:FromYoungest
        (fun acc s -> List.append acc [ s ])
        [] auctions.avl_storage burrow_slices
  | None -> []

let suite =
  let burrow_id_1 =
    (Ligo.address_of_string "burrow_1", Ligo.nat_from_literal "0n")
  in
  let burrow_id_2 =
    (Ligo.address_of_string "burrow_2", Ligo.nat_from_literal "0n")
  in
  let burrow_id_3 =
    (Ligo.address_of_string "burrow_3", Ligo.nat_from_literal "0n")
  in

  "Liquidation auction tests"
  >::: [
         ( "liquidation_auction_send_to_auction - fails when the auction queue \
            is full"
         >:: fun _ ->
           (* Values in this slice are arbitrary and should not matter *)
           let slice =
             {
               burrow = burrow_id_1;
               tez = Ligo.tez_from_literal "4_000_000mutez";
               min_kit_for_unwarranted =
                 Some (kit_of_mukit (Ligo.nat_from_literal "1_000_000n"));
             }
           in
           (* 2 ^ (Constants.max_liquidation_queue_height - 2) *)
           let max_length = 1024 in
           let slices = repeat max_length slice in
           let auctions =
             List.fold_left
               (fun auctions slice_contents ->
                 fst
                   (liquidation_auction_send_to_auction auctions slice_contents))
               liquidation_auction_empty slices
           in

           assert_raises
             (Failure (Ligo.string_of_int error_LiquidationQueueTooLong))
             (fun () -> liquidation_auction_send_to_auction auctions slice) );
         ( qcheck_to_ounit
         @@ QCheck.Test.make
              ~name:
                "liquidation_auction_send_to_auction - inserts slice into \
                 burrow_slices list"
              ~count:property_test_count
              (QCheck.make (gen_liquidation_slice_contents_list 100))
         @@ fun slice_contents_list ->
         (* This test sends a random set of slices to auction and checks that the burrow slice list
          * in the auction state is updated to include the new slice.
          *)
         let _ =
           List.fold_left
             (* For each input slice: *)
               (fun auctions slice_contents ->
               (* Send this slice to auction *)
               let auctions_out, new_slice =
                 liquidation_auction_send_to_auction auctions slice_contents
               in

               (* Gather data for assertions *)
               let slices_in =
                 get_burrow_slices auctions slice_contents.burrow
               in
               let slices_out =
                 get_burrow_slices auctions_out slice_contents.burrow
               in
               let index_new_slice =
                 index_of_leaf auctions_out slice_contents.burrow new_slice
               in
               let slices_without_new_one =
                 snd
                   (List.fold_left
                      (fun (i, xs) x ->
                        if i == index_new_slice then (i + 1, xs)
                        else (i + 1, List.append xs [ x ]))
                      (0, []) slices_out)
               in

               assert_liquidation_auction_invariants auctions;
               assert_liquidation_auction_invariants auctions_out;
               (* We should have inserted one element into our original list *)
               assert_stdlib_int_equal
                 ~expected:(List.length slices_in + 1)
                 ~real:(List.length slices_out);
               (* Removing the new element from the list should produce the input list *)
               (* Note: comparing by content since the pointers will change when inserting *)
               assert_slice_content_list_equal ~expected:slices_in
                 ~real:slices_without_new_one;
               auctions_out)
             liquidation_auction_empty slice_contents_list
         in
         true );
         ( qcheck_to_ounit
         @@ QCheck.Test.make
              ~name:
                "liquidation_auctions_cancel_slice - removes slice from \
                 burrow_slices list"
              ~count:property_test_count
              (QCheck.make
                 QCheck.Gen.(
                   pair
                     (gen_liquidation_slice_contents_list 100)
                     (float_bound_inclusive 1.)))
         @@ fun (slice_contents_list, percent) ->
         (* This test populates an auction queue with random slices and tests that
          * canceling a random slice correctly removes the burrow slice list in the auction state.
          *)
         (* Setup: Populating an auction queue with random slices and noting a slice in the queue to pop for the test. *)
         (* Pick a random element from the input slice list to cancel *)
         let i_to_pop =
           Float.(
             to_int
               (round
                  (mul percent
                     (float_of_int (List.length slice_contents_list - 1)))))
         in
         (* Populate the liquidation auction queue, noting the pointer of the element we want to pop *)
         let auctions, _, slice_to_pop =
           List.fold_left
             (fun (a, curr_length, slice_to_pop) slice_contents ->
               let auctions_out, new_slice =
                 liquidation_auction_send_to_auction a slice_contents
               in
               if curr_length = i_to_pop then
                 (auctions_out, curr_length + 1, Some new_slice)
               else (auctions_out, curr_length + 1, slice_to_pop))
             (liquidation_auction_empty, 0, None)
             slice_contents_list
         in
         let slice_to_pop =
           match slice_to_pop with
           | Some p -> p
           | None ->
               failwith
                 "did not find the expected index when populating the slice \
                  lists"
         in

         (* Pop the slice from the list *)
         let popped_contents, auctions_out =
           liquidation_auctions_cancel_slice auctions slice_to_pop
         in

         (* Run assertions on all burrow slice lists*)
         let _ =
           List.map
             (fun (burrow, _) ->
               (* Read data from the list for assertions *)
               let slices_in = get_burrow_slices auctions burrow in
               let slices_out = get_burrow_slices auctions_out burrow in
               let expected_slices =
                 (* If the popped slice belongs to this burrow, we don't expect to find it in the output *)
                 if burrow = popped_contents.burrow then
                   let i_to_pop = index_of_leaf auctions burrow slice_to_pop in
                   (* Note: would have used List.filteri here, but it doesn't exist in OCaml 4.10 *)
                   List.filteri
                     (fun i _ -> if i == i_to_pop then false else true)
                     slices_in
                 else
                   (* Otherwise the output should just be the input *)
                   slices_in
               in

               assert_liquidation_auction_invariants auctions;
               assert_liquidation_auction_invariants auctions_out;
               assert_liquidation_slice_contents_equal ~expected:popped_contents
                 ~real:(List.nth slice_contents_list i_to_pop);
               assert_stdlib_int_equal
                 ~expected:(List.length expected_slices)
                 ~real:(List.length slices_out);
               assert_slice_content_list_equal ~expected:expected_slices
                 ~real:slices_out)
             (Ligo.Big_map.bindings auctions_out.burrow_slices)
         in
         true );
         ( qcheck_to_ounit
         @@ QCheck.Test.make
              ~name:
                "liquidation_auctions_pop_completed_slice - removes slice from \
                 burrow_slices list"
              ~count:property_test_count
              (QCheck.make
                 QCheck.Gen.(
                   pair
                     (gen_liquidation_slice_contents_list 100)
                     (float_bound_inclusive 1.)))
         @@ (* This test populates a completed auction with random slices and tests that
             * popping a completed slice correctly removes the slice from the burrow slice list in the auction state.
             *)
         fun (slice_contents_list, percent) ->
         (* Setup: Populating a completed auction with random slices and noting a slice in the queue to pop for the test. *)
         (* Pick a random element from the input slice list to pop *)
         let i_to_pop =
           Float.(
             to_int
               (round
                  (mul percent
                     (float_of_int (List.length slice_contents_list - 1)))))
         in
         (* Populate the liquidation auction queue, noting the pointer of the element we want to pop *)
         let auctions, _, slice_to_pop =
           List.fold_left
             (fun (a, curr_length, slice_to_pop) slice_contents ->
               let auctions_out, new_slice =
                 liquidation_auction_send_to_auction a slice_contents
               in
               if curr_length = i_to_pop then
                 (* For this test to succeeed, the slice must be associated with a completed auction.
                  * Creating a completed auction manually here for simplicity.
                  *)
                 let auction_ptr =
                   Avl.avl_find_root auctions_out.avl_storage new_slice
                 in
                 let avl_out =
                   Avl.avl_modify_root_data auctions_out.avl_storage auction_ptr
                     (fun outcome ->
                       match outcome with
                       | _ ->
                           Some
                             {
                               sold_tez = Ligo.tez_from_literal "1mutez";
                               winning_bid =
                                 {
                                   address = Ligo.address_of_string "someone";
                                   kit =
                                     kit_of_mukit
                                       (Ligo.nat_from_literal "1_000_000n");
                                 };
                               younger_auction = None;
                               older_auction = None;
                             })
                 in
                 let completed_auctions =
                   Some { youngest = auction_ptr; oldest = auction_ptr }
                 in
                 ( {
                     auctions_out with
                     avl_storage = avl_out;
                     completed_auctions;
                   },
                   curr_length + 1,
                   Some new_slice )
               else (auctions_out, curr_length + 1, slice_to_pop))
             (liquidation_auction_empty, 0, None)
             slice_contents_list
         in
         let slice_to_pop =
           match slice_to_pop with
           | Some p -> p
           | None ->
               failwith
                 "did not find the expected index when populating the slice \
                  lists"
         in

         (* Pop the slice from the list *)
         let popped_contents, _, auctions_out =
           liquidation_auctions_pop_completed_slice auctions slice_to_pop
         in

         (* Run assertions on all burrow slice lists*)
         let _ =
           List.map
             (fun (burrow, _) ->
               (* Read data from the list for assertions *)
               let slices_in = get_burrow_slices auctions burrow in
               let slices_out = get_burrow_slices auctions_out burrow in
               let expected_contents =
                 if burrow = popped_contents.burrow then
                   let i_to_pop = index_of_leaf auctions burrow slice_to_pop in
                   List.filteri
                     (fun i _ -> if i == i_to_pop then false else true)
                     slices_in
                 else slices_in
               in

               assert_liquidation_auction_invariants auctions;
               assert_liquidation_auction_invariants auctions_out;
               assert_liquidation_slice_contents_equal ~expected:popped_contents
                 ~real:(List.nth slice_contents_list i_to_pop);
               assert_stdlib_int_equal
                 ~expected:(List.length expected_contents)
                 ~real:(List.length slices_out);
               assert_slice_content_list_equal ~expected:expected_contents
                 ~real:slices_out)
             (Ligo.Big_map.bindings auctions_out.burrow_slices)
         in
         true );
         ( "test initial auction minimum bid has expected value" >:: fun _ ->
           Ligo.Tezos.reset ();
           let auctions = liquidation_auction_empty in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_1;
                 tez = Ligo.tez_from_literal "2_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "4_000_000n"))
                   (* note: randomly chosen *);
               }
           in
           let start_price =
             {
               num = Ligo.int_from_literal "3";
               den = Ligo.int_from_literal "7";
             }
           in
           let auctions = liquidation_auction_touch auctions start_price in
           let current = Option.get auctions.current_auction in
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "4_666_667n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current) );
         ( "test starts descending auction" >:: fun _ ->
           Ligo.Tezos.reset ();
           let auctions = liquidation_auction_empty in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_1;
                 tez = Ligo.tez_from_literal "2_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "4_000_000n"))
                   (* note: randomly chosen *);
               }
           in
           let start_price = one_ratio in
           let auctions = liquidation_auction_touch auctions start_price in
           let current = Option.get auctions.current_auction in
           assert_tez_option_equal
             ~expected:(Some (Ligo.tez_from_literal "2_000_000mutez"))
             ~real:(liquidation_auction_current_auction_tez auctions);
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "2_000_000n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current);
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "2_000_000n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current);
           (* Price of descending auction should go down... *)
           Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1
             ~sender:alice_addr
             ~amount:(Ligo.tez_from_literal "0mutez");
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "1_999_666n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current);
           Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1
             ~sender:alice_addr
             ~amount:(Ligo.tez_from_literal "0mutez");
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "1_999_333n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current);
           Ligo.Tezos.new_transaction ~seconds_passed:58 ~blocks_passed:1
             ~sender:alice_addr
             ~amount:(Ligo.tez_from_literal "0mutez");
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "1_980_098n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current);
           Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1
             ~sender:alice_addr
             ~amount:(Ligo.tez_from_literal "0mutez");
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "1_960_394n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current) );
         ( "test batches up auction lots" >:: fun _ ->
           Ligo.Tezos.reset ();
           let auctions = liquidation_auction_empty in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_1;
                 tez = Ligo.tez_from_literal "5_000_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "9_000_001n"))
                   (* note: randomly chosen *);
               }
           in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_2;
                 tez = Ligo.tez_from_literal "5_000_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "9_000_002n"))
                   (* note: randomly chosen *);
               }
           in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_3;
                 tez = Ligo.tez_from_literal "5_000_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "9_000_003n"))
                   (* note: randomly chosen *);
               }
           in
           let start_price = one_ratio in
           let auctions = liquidation_auction_touch auctions start_price in
           assert_tez_option_equal
             ~expected:(Some (Ligo.tez_from_literal "10_000_000_000mutez"))
             ~real:(liquidation_auction_current_auction_tez auctions) );
         ( "test splits up auction lots to fit batch size" >:: fun _ ->
           Ligo.Tezos.reset ();
           let auctions = liquidation_auction_empty in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_1;
                 tez = Ligo.tez_from_literal "4_000_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "9_000_004n"))
                   (* note: randomly chosen *);
               }
           in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_2;
                 tez = Ligo.tez_from_literal "5_000_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "9_000_005n"))
                   (* note: randomly chosen *);
               }
           in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_3;
                 tez = Ligo.tez_from_literal "3_000_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "9_000_006n"))
                   (* note: randomly chosen *);
               }
           in
           let start_price = one_ratio in
           let auctions = liquidation_auction_touch auctions start_price in
           assert_tez_option_equal
             ~expected:(Some (Ligo.tez_from_literal "10_000_000_000mutez"))
             ~real:(liquidation_auction_current_auction_tez auctions) );
         ( "test bidding" >:: fun _ ->
           Ligo.Tezos.reset ();
           let auctions = liquidation_auction_empty in
           let auctions, _ =
             liquidation_auction_send_to_auction auctions
               {
                 burrow = burrow_id_1;
                 tez = Ligo.tez_from_literal "2_000_000mutez";
                 min_kit_for_unwarranted =
                   Some (kit_of_mukit (Ligo.nat_from_literal "4_000_007n"))
                   (* note: randomly chosen *);
               }
           in
           let start_price = one_ratio in
           let auctions = liquidation_auction_touch auctions start_price in
           let bidder = Ligo.address_from_literal "23456" in
           let current = Option.get auctions.current_auction in

           (* Below minimum bid *)
           assert_raises
             (Failure (Ligo.string_of_int error_BidTooLow))
             (fun () ->
               liquidation_auction_place_bid current
                 {
                   address = bidder;
                   kit = kit_of_mukit (Ligo.nat_from_literal "1_000_000n");
                 });
           (* Right below minimum bid *)
           assert_raises
             (Failure (Ligo.string_of_int error_BidTooLow))
             (fun () ->
               liquidation_auction_place_bid current
                 {
                   address = bidder;
                   kit = kit_of_mukit (Ligo.nat_from_literal "1_999_999n");
                 });
           (* On/Above minimum bid, we get a bid ticket and our bid plus 0.33 cNp becomes the new minimum bid *)
           let current, _ =
             liquidation_auction_place_bid current
               {
                 address = bidder;
                 kit = kit_of_mukit (Ligo.nat_from_literal "2_000_000n");
               }
           in
           assert_kit_equal
             ~expected:(kit_of_mukit (Ligo.nat_from_literal "2_006_599n"))
             ~real:(liquidation_auction_current_auction_minimum_bid current);
           (* Minimum bid does not drop over time *)
           Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1
             ~sender:alice_addr
             ~amount:(Ligo.tez_from_literal "0mutez");
           (* Can increase the bid.*)
           let current, _ =
             liquidation_auction_place_bid current
               {
                 address = bidder;
                 kit = kit_of_mukit (Ligo.nat_from_literal "4_000_000n");
               }
           in
           (* Does not allow a lower bid.*)
           assert_raises
             (Failure (Ligo.string_of_int error_BidTooLow))
             (fun () ->
               liquidation_auction_place_bid current
                 {
                   address = bidder;
                   kit = kit_of_mukit (Ligo.nat_from_literal "3_000_000n");
                 });

           () );
         ( qcheck_to_ounit
         @@ QCheck.Test.make ~name:"bid can not be zero"
              ~count:property_test_count
              QCheck.(0 -- 100)
         @@ fun blocks_passed ->
         (* Create an auction with one slice *)
         Ligo.Tezos.reset ();
         let auctions = liquidation_auction_empty in
         let auctions, _ =
           liquidation_auction_send_to_auction auctions
             (* Note: The amounts don't matter here. We are only interested in the bidding logic *)
             {
               burrow = burrow_id_1;
               tez = Ligo.tez_from_literal "1_000_000mutez";
               min_kit_for_unwarranted =
                 Some (kit_of_mukit (Ligo.nat_from_literal "1n"))
                 (* note: randomly chosen *);
             }
         in
         let start_price = one_ratio in
         let auctions = liquidation_auction_touch auctions start_price in
         let current = Option.get auctions.current_auction in

         Ligo.Tezos.new_transaction ~seconds_passed:(blocks_passed * 3600)
           ~blocks_passed ~sender:alice_addr
           ~amount:(Ligo.tez_from_literal "0mutez");
         assert_raises
           (Failure (Ligo.string_of_int error_BidTooLow))
           (fun () ->
             liquidation_auction_place_bid current
               { address = Ligo.address_from_literal "12345"; kit = kit_zero });
         ();
         true );
       ]

let () = run_test_tt_main suite
