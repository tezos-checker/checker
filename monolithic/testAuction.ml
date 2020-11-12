open OUnit2
open Address

type some_kit = Kit.t option [@@deriving show]

let suite =
  "Auction tests" >::: [
    ("test starts descending auction" >::
     fun _ ->
       let auctions = Auction.empty in
       let start_time = (Timestamp.of_seconds 0) in
       let start_price = Kit.one in
       let (auctions, _) =
         Auction.send_to_auction auctions { burrow = Address.of_string "12345"; tez = Tez.of_mutez 2_000_000; } in
       let auctions = Auction.touch auctions start_time start_price in
       assert_equal (Some (Tez.of_mutez 2_000_000)) (Auction.current_auction_tez auctions);
       assert_equal (Some (Kit.of_mukit 2_000_000)) (Option.map (Auction.current_auction_bid_threshold start_time) auctions.current_auction) ~printer:show_some_kit;
       assert_equal (Some (Kit.of_mukit 2_000_000)) (Option.map (Auction.current_auction_bid_threshold start_time) auctions.current_auction) ~printer:show_some_kit;
       (* Price of descending auction should go down... *)
       let one_second_later = Timestamp.add_seconds start_time 1 in
       assert_equal (Some (Kit.of_mukit 1_998_000)) (Option.map (Auction.current_auction_bid_threshold one_second_later) auctions.current_auction) ~printer:show_some_kit;
       let two_seconds_later = Timestamp.add_seconds start_time 2 in
       assert_equal (Some (Kit.of_mukit 1_996_002)) (Option.map (Auction.current_auction_bid_threshold two_seconds_later) auctions.current_auction) ~printer:show_some_kit;
    );
  ]
