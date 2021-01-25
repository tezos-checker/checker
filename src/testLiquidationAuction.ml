open Ptr
open Kit
open OUnit2
open TestCommon
open FixedPoint

let checker_address = Ligo.address_from_literal "checker"
let checker_amount = Ligo.tez_from_literal "0mutez"
let checker_sender = Ligo.address_from_literal "somebody"

let suite =
  let burrow_id_1 = ptr_init in
  let burrow_id_2 = ptr_next burrow_id_1 in
  let burrow_id_3 = ptr_next burrow_id_2 in

  "Liquidation auction tests" >::: [
    ("test starts descending auction" >::
     fun _ ->
       Ligo.Tezos.reset();
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction auctions {
           burrow = burrow_id_1;
           tez = Ligo.tez_from_literal "2_000_000mutez";
           min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "4_000_000"); (* note: randomly chosen *)
           younger = None; older = None;
         } in
       let start_price = fixedpoint_one in
       let auctions = LiquidationAuction.touch auctions start_price in
       let current = Option.get auctions.current_auction in
       assert_equal
         (Some (Ligo.tez_from_literal "2_000_000mutez"))
         (LiquidationAuction.current_auction_tez auctions);
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "2_000_000"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "2_000_000"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
       (* Price of descending auction should go down... *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "1_999_666"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "1_999_333"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
       Ligo.Tezos.new_transaction ~seconds_passed:58 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "1_980_098"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "1_960_394"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
    );

    ("test batches up auction lots" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "9_000_001"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "9_000_002"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "9_000_003"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_price = fixedpoint_one in
       let auctions = LiquidationAuction.touch auctions start_price in
       assert_equal (Some (Ligo.tez_from_literal "10_000_000_000mutez")) (LiquidationAuction.current_auction_tez auctions);
    );

    ("test splits up auction lots to fit batch size" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Ligo.tez_from_literal "4_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "9_000_004"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "9_000_005"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Ligo.tez_from_literal "3_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "9_000_006"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_price = fixedpoint_one in
       let auctions = LiquidationAuction.touch auctions start_price in
       assert_equal (Some (Ligo.tez_from_literal "10_000_000_000mutez")) (LiquidationAuction.current_auction_tez auctions);
    );

    ("test bidding" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Ligo.tez_from_literal "2_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.int_from_literal "4_000_007"); (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_price = fixedpoint_one in
       let auctions = LiquidationAuction.touch auctions start_price in
       let bidder = Ligo.address_from_literal "23456" in
       let current = Option.get auctions.current_auction in

       (* Below minimum bid *)
       assert_raises
         (Failure "BidTooLow")
         (fun () -> LiquidationAuction.place_bid current { address = bidder; kit = kit_of_mukit (Ligo.int_from_literal "1_000_000"); });
       (* Right below minimum bid *)
       assert_raises
         (Failure "BidTooLow")
         (fun () -> LiquidationAuction.place_bid current { address = bidder; kit = kit_of_mukit (Ligo.int_from_literal "1_999_999"); });
       (* On/Above minimum bid, we get a bid ticket and our bid plus 0.33 cNp becomes the new minimum bid *)
       let (current, _) = LiquidationAuction.place_bid current { address = bidder; kit = kit_of_mukit (Ligo.int_from_literal "2_000_000"); } in
       assert_equal
         (kit_of_mukit (Ligo.int_from_literal "2_006_599"))
         (LiquidationAuction.current_auction_minimum_bid current)
         ~printer:show_kit;
       (* Minimum bid does not drop over time *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* Can increase the bid.*)
       let (current, _) = LiquidationAuction.place_bid current {address=bidder; kit=kit_of_mukit (Ligo.int_from_literal "4_000_000")} in
       (* Does not allow a lower bid.*)
       assert_raises
         (Failure "BidTooLow")
         (fun () -> LiquidationAuction.place_bid current {address=bidder; kit=kit_of_mukit (Ligo.int_from_literal "3_000_000")});

       ()
    );
  ]
