open Kit
open OUnit2
open TestCommon
open LiquidationAuction
open LiquidationAuctionTypes
open Error
open Ratio

let checker_address = Ligo.address_from_literal "checker"
let checker_amount = Ligo.tez_from_literal "0mutez"
let checker_sender = Ligo.address_from_literal "somebody"

let suite =
  let burrow_id_1 = Ligo.address_of_string "burrow_1" in
  let burrow_id_2 = Ligo.address_of_string "burrow_2" in
  let burrow_id_3 = Ligo.address_of_string "burrow_3" in

  "Liquidation auction tests" >::: [
    ("test starts descending auction" >::
     fun _ ->
       Ligo.Tezos.reset();
       let auctions = liquidation_auction_empty in
       let (auctions, _) =
         liquidation_auction_send_to_auction auctions {
           burrow = burrow_id_1;
           tez = Ligo.tez_from_literal "2_000_000mutez";
           min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "4_000_000n"); (* note: randomly chosen *)
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
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "9_000_001n"); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "9_000_002n"); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "9_000_003n"); (* note: randomly chosen *)
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
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "9_000_004n"); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Ligo.tez_from_literal "5_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "9_000_005n"); (* note: randomly chosen *)
           } in
       let (auctions, _) =
         liquidation_auction_send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Ligo.tez_from_literal "3_000_000_000mutez";
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "9_000_006n"); (* note: randomly chosen *)
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
             min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "4_000_007n"); (* note: randomly chosen *)
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
  ]
