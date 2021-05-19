open OUnit2
open Kit
open LiquidationAuctionTypes
open SliceList
open LiquidationAuctionPrimitiveTypes

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t
let property_test_count = 100

let burrow_id_1 = (Ligo.address_of_string "burrow_a"), Ligo.nat_from_literal "1n"

(* ========================================================================= *)
(* QCheck arbitrary values *)
(* ========================================================================= *)
let gen_liquidation_slice_contents_single_burrow_id =
  QCheck.Gen.(
    map
      (fun (tz, kit) ->
         LiquidationAuctionPrimitiveTypes.
           ({ tez = Ligo.tez_from_literal ((string_of_int tz) ^ "mutez")
            ; burrow = burrow_id_1
            ; min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal ((string_of_int kit) ^ "n")))
            })
      )
      (pair (int_range 0 max_int) (int_range 0 max_int))
  )

let gen_liquidation_slice_contents_list max_length =
  QCheck.Gen.(
    list_size (1--max_length) gen_liquidation_slice_contents_single_burrow_id
  )
(* ========================================================================= *)

type slice_content_list = liquidation_slice_contents list [@@deriving show]
let suite =
  "SliceListTests" >::: [

    ("slice_list_from_auction - creates empty list when auction has no corresponding burrow_slices" >::
     fun _ ->
       let burrow_slices = slice_list_from_auction_state liquidation_auction_empty burrow_id_1 in
       assert_equal (slice_list_empty burrow_id_1) burrow_slices ~printer:show_slice_list
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"slice_list_append - added slice is the youngest in list"
        ~count:property_test_count
        (QCheck.make (gen_liquidation_slice_contents_list 10))
      @@
      fun slice_contents_list ->
      (
        let _ = List.fold_left (
            fun (auctions, burrow_slices) slice_contents ->
              let storage, burrow_slices, element = slice_list_append burrow_slices auctions.avl_storage auctions.queued_slices Avl.Front slice_contents in
              let youngest = match (slice_list_youngest burrow_slices storage) with
                | Some expected_element -> expected_element
                | None -> failwith "slice list should have bounds after appending but has none."
              in
              assert_equal youngest element ~printer:show_slice_list_element ~msg:"new element is youngest one";
              ({auctions with avl_storage = storage;}, burrow_slices)
          )
            (liquidation_auction_empty, slice_list_empty burrow_id_1)
            slice_contents_list
        in
        true
      )
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"slice_list_append - added slice is added to AVL storage"
        ~count:property_test_count
        (QCheck.make (gen_liquidation_slice_contents_list 10))
      @@
      fun slice_contents_list ->
      (
        let _ = List.fold_left (
            fun (auctions, burrow_slices) slice_contents ->
              let root_ptr = auctions.queued_slices in
              let storage = auctions.avl_storage in

              (* Test adding to the front of AVL queue *)
              let storage, burrow_slices, element = slice_list_append burrow_slices storage root_ptr Avl.Front slice_contents in
              let _ = match element with SliceListElement (ptr, slice) ->
                let avl_value = Avl.avl_peek_front storage root_ptr in
                match avl_value with
                | None -> failwith "AVL was empty after appending slice to list"
                | Some (avl_ptr, avl_slice) ->
                  assert_equal slice avl_slice.value ~printer:show_liquidation_slice;
                  assert_equal ptr avl_ptr ~printer:show_leaf_ptr;
              in

              (* Test adding to the back of AVL queue *)
              let storage, burrow_slices, element = slice_list_append burrow_slices storage root_ptr Avl.Back slice_contents in
              let _ = match element with SliceListElement (_, slice) ->
                (* Note: didn't know how to get the last item in the queue without doing this. *)
                let avl_value = List.hd (List.rev (TestAvl.avl_to_list storage root_ptr)) in
                assert_equal slice avl_value ~printer:show_liquidation_slice;
              in

              ({auctions with avl_storage = storage;}, burrow_slices)

          )
            (liquidation_auction_empty, slice_list_empty burrow_id_1)
            slice_contents_list
        in
        true
      )
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"slice_list_remove - removed element is also removed from AVL storage"
        ~count:property_test_count
        (QCheck.triple (QCheck.make (gen_liquidation_slice_contents_list 5)) (QCheck.make gen_liquidation_slice_contents_single_burrow_id) (QCheck.make (gen_liquidation_slice_contents_list 5)))
      @@
      fun (first_section, slice_to_remove, second_section) ->
      (
        let auctions = liquidation_auction_empty in
        let root_ptr = auctions.queued_slices in
        let queue_end = Avl.Front in
        (* Pre-populate first part of list*)
        let storage, burrow_slices = List.fold_left (
            fun (storage, burrow_slices) slice_contents ->
              let storage, burrow_slices, _ = slice_list_append burrow_slices storage root_ptr queue_end slice_contents in
              (storage, burrow_slices)
          )
            (auctions.avl_storage, slice_list_empty burrow_id_1) first_section in
        (* Add the element which we will later remove *)
        let storage, burrow_slices, to_remove = slice_list_append burrow_slices storage root_ptr queue_end slice_to_remove in
        (* Populate the rest of the list *)
        let storage, burrow_slices = List.fold_left (
            fun (storage, burrow_slices) slice_contents ->
              let storage, burrow_slices, _ = slice_list_append burrow_slices storage root_ptr queue_end slice_contents in
              (storage, burrow_slices)
          )
            (storage, burrow_slices) second_section in

        (* Remove an element from the list *)
        let removed_ptr = match to_remove with SliceListElement (ptr, _) -> ptr in
        let storage, _, _, _  = slice_list_remove burrow_slices storage to_remove in
        (* Retrieving the slice from the AVL backend should now fail *)
        assert_raises (Failure "mem_get: not found") (fun () -> Avl.avl_read_leaf storage removed_ptr);
        true
      )
    );
  ]
