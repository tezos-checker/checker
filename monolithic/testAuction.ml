open OUnit2


let suite =
  let burrow_id_1 = Ptr.init in
  let burrow_id_2 = Ptr.next burrow_id_1 in
  let burrow_id_3 = Ptr.next burrow_id_2 in

  "Auction tests" >::: [
    ("test starts descending auction" >::
     fun _ ->
       let auctions = Auction.empty in
       let (auctions, _) =
         Auction.send_to_auction auctions {
           burrow = burrow_id_1;
           tez = Tez.of_mutez 2_000_000;
           min_kit_for_unwarranted = Kit.of_mukit 4_000_000; (* note: randomly chosen *)
           younger = None; older = None;
         } in
       let start_time = (Timestamp.of_seconds 0) in
       let start_price = Kit.one in
       let auctions = Auction.touch auctions start_time start_price in
       let current = Option.get auctions.current_auction in
       assert_equal (Some (Tez.of_mutez 2_000_000)) (Auction.current_auction_tez auctions);
       assert_equal (Kit.of_mukit 2_000_000) (Auction.current_auction_minimum_bid start_time current) ~printer:Kit.show;
       assert_equal (Kit.of_mukit 2_000_000) (Auction.current_auction_minimum_bid start_time current) ~printer:Kit.show;
       (* Price of descending auction should go down... *)
       let one_second_later = Timestamp.add_seconds start_time 1 in
       assert_equal (Kit.of_mukit 1_998_000) (Auction.current_auction_minimum_bid one_second_later current) ~printer:Kit.show;
       let two_seconds_later = Timestamp.add_seconds start_time 2 in
       assert_equal (Kit.of_mukit 1_996_002) (Auction.current_auction_minimum_bid two_seconds_later current) ~printer:Kit.show;
    );

    ("test batches up auction lots" >::
     fun _ ->
       let auctions = Auction.empty in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_001; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_002; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_003; (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_time = (Timestamp.of_seconds 0) in
       let start_price = Kit.one in
       let auctions = Auction.touch auctions start_time start_price in
       assert_equal (Some (Tez.of_mutez 10_000_000_000)) (Auction.current_auction_tez auctions);
    );

    ("test splits up auction lots to fit batch size" >::
     fun _ ->
       let auctions = Auction.empty in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Tez.of_mutez 4_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_004; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_005; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Tez.of_mutez 3_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_006; (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_time = (Timestamp.of_seconds 0) in
       let start_price = Kit.one in
       let auctions = Auction.touch auctions start_time start_price in
       assert_equal (Some (Tez.of_mutez 10_000_000_000)) (Auction.current_auction_tez auctions);
    );

    ("test initial bidding" >::
     fun _ ->
       let auctions = Auction.empty in
       let (auctions, _) =
         Auction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Tez.of_mutez 2_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 4_000_007; (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_time = (Timestamp.of_seconds 0) in
       let start_price = Kit.one in
       let auctions = Auction.touch auctions start_time start_price in
       let bidder = Address.of_string "23456" in
       let current = Option.get auctions.current_auction in

       (* Below minimum bid *)
       assert_equal (Error Auction.BidTooLow) (Auction.place_bid start_time current { address = bidder; kit = Kit.of_mukit 1_000_000; });
       (* Right below minimum bid *)
       assert_equal (Error Auction.BidTooLow) (Auction.place_bid start_time current { address = bidder; kit = Kit.of_mukit 1_999_999; });
       (* On/Above minimum bid, we get a bid ticket and our bid plus 0.33 cNp becomes the new minimum bid *)
       let (current, _ticket) = Result.get_ok (Auction.place_bid start_time current { address = bidder; kit = Kit.of_mukit 2_000_000; }) in
       assert_equal (Kit.of_mukit 2_006_600) (Auction.current_auction_minimum_bid start_time current) ~printer:Kit.show;
       (* Minimum bid does not drop over time *)
       assert_equal (Kit.of_mukit 2_006_600) (Auction.current_auction_minimum_bid (Timestamp.add_seconds start_time 10) current) ~printer:Kit.show;
    );
  ]
