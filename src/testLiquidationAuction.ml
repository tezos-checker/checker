open OUnit2

let checker_address = Address.of_string "checker"

let suite =
  let burrow_id_1 = Ptr.init in
  let burrow_id_2 = Ptr.next burrow_id_1 in
  let burrow_id_3 = Ptr.next burrow_id_2 in

  "Auction tests" >::: [
    ("test starts descending auction" >::
     fun _ ->
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction auctions {
           burrow = burrow_id_1;
           tez = Tez.of_mutez 2_000_000;
           min_kit_for_unwarranted = Kit.of_mukit 4_000_000; (* note: randomly chosen *)
           younger = None; older = None;
         } in
       let start_time = Timestamp.of_seconds 0 in
       let start_level = Level.of_int 0 in
       let start_tezos = Tezos.{now = start_time; level = start_level; self = checker_address;} in
       let start_price = FixedPoint.one in
       let auctions = LiquidationAuction.touch auctions start_tezos start_price in
       let current = Option.get auctions.current_auction in
       assert_equal (Some (Tez.of_mutez 2_000_000)) (LiquidationAuction.current_auction_tez auctions);
       assert_equal (Kit.of_mukit 2_000_000) (LiquidationAuction.current_auction_minimum_bid start_tezos current) ~printer:Kit.show;
       assert_equal (Kit.of_mukit 2_000_000) (LiquidationAuction.current_auction_minimum_bid start_tezos current) ~printer:Kit.show;
       (* Price of descending auction should go down... *)
       let one_second_later = Tezos.{now = Timestamp.add_seconds start_time 1; level = Level.of_int 0; self = checker_address;} in
       assert_equal (Kit.of_mukit 1_999_666) (LiquidationAuction.current_auction_minimum_bid one_second_later current) ~printer:Kit.show;
       let two_seconds_later = Tezos.{now = Timestamp.add_seconds start_time 2; level = Level.of_int 0; self = checker_address;} in
       assert_equal (Kit.of_mukit 1_999_333) (LiquidationAuction.current_auction_minimum_bid two_seconds_later current) ~printer:Kit.show;
       let one_minute_later = Tezos.{now = Timestamp.add_seconds start_time 60; level = Level.of_int 1; self = checker_address;} in
       assert_equal (Kit.of_mukit 1_980_097) (LiquidationAuction.current_auction_minimum_bid one_minute_later current) ~printer:Kit.show;
       let two_minutes_later = Tezos.{now = Timestamp.add_seconds start_time (2 * 60); level = Level.of_int 2; self = checker_address;} in
       assert_equal (Kit.of_mukit 1_960_393) (LiquidationAuction.current_auction_minimum_bid two_minutes_later current) ~printer:Kit.show;
    );

    ("test batches up auction lots" >::
     fun _ ->
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_001; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_002; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_003; (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_time = Timestamp.of_seconds 0 in
       let start_level = Level.of_int 0 in
       let start_tezos = Tezos.{now = start_time; level = start_level; self = checker_address;} in
       let start_price = FixedPoint.one in
       let auctions = LiquidationAuction.touch auctions start_tezos start_price in
       assert_equal (Some (Tez.of_mutez 10_000_000_000)) (LiquidationAuction.current_auction_tez auctions);
    );

    ("test splits up auction lots to fit batch size" >::
     fun _ ->
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Tez.of_mutez 4_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_004; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_2; tez = Tez.of_mutez 5_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_005; (* note: randomly chosen *)
             younger = None; older = None; } in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_3; tez = Tez.of_mutez 3_000_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 9_000_006; (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_time = Timestamp.of_seconds 0 in
       let start_level = Level.of_int 0 in
       let start_tezos = Tezos.{now = start_time; level = start_level; self = checker_address;} in
       let start_price = FixedPoint.one in
       let auctions = LiquidationAuction.touch auctions start_tezos start_price in
       assert_equal (Some (Tez.of_mutez 10_000_000_000)) (LiquidationAuction.current_auction_tez auctions);
    );

    ("test bidding" >::
     fun _ ->
       let auctions = LiquidationAuction.empty in
       let (auctions, _) =
         LiquidationAuction.send_to_auction
           auctions
           { burrow = burrow_id_1; tez = Tez.of_mutez 2_000_000;
             min_kit_for_unwarranted = Kit.of_mukit 4_000_007; (* note: randomly chosen *)
             younger = None; older = None; } in
       let start_time = Timestamp.of_seconds 0 in
       let start_level = Level.of_int 0 in
       let start_tezos = Tezos.{now = start_time; level = start_level; self = checker_address;} in
       let start_price = FixedPoint.one in
       let auctions = LiquidationAuction.touch auctions start_tezos start_price in
       let bidder = Address.of_string "23456" in
       let current = Option.get auctions.current_auction in

       (* Below minimum bid *)
       assert_equal
         (Error LiquidationAuction.BidTooLow)
         (LiquidationAuction.place_bid start_tezos current { address = bidder; kit = Kit.of_mukit 1_000_000; });
       (* Right below minimum bid *)
       assert_equal
         (Error LiquidationAuction.BidTooLow)
         (LiquidationAuction.place_bid start_tezos current { address = bidder; kit = Kit.of_mukit 1_999_999; });
       (* On/Above minimum bid, we get a bid ticket and our bid plus 0.33 cNp becomes the new minimum bid *)
       let (current, _) = Result.get_ok (LiquidationAuction.place_bid start_tezos current { address = bidder; kit = Kit.of_mukit 2_000_000; }) in
       assert_equal
         (Kit.of_mukit 2_006_600)
         (LiquidationAuction.current_auction_minimum_bid start_tezos current)
         ~printer:Kit.show;
       (* Minimum bid does not drop over time *)
       let ten_seconds_later = {
         start_tezos with now = Timestamp.add_seconds start_tezos.now 10
       } in
       (* Can increase the bid.*)
       let (current, _) = Result.get_ok @@
         (LiquidationAuction.place_bid ten_seconds_later current {address=bidder; kit=Kit.of_mukit 4_000_000}) in
       (* Does not allow a lower bid.*)
       assert_equal
         (Error LiquidationAuction.BidTooLow)
         (LiquidationAuction.place_bid ten_seconds_later current {address=bidder; kit=Kit.of_mukit 3_000_000});

       ()
    );
  ]
