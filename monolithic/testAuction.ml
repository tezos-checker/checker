open OUnit2
open Address

let suite =
  "Auction tests" >::: [
    ("test starts auction" >::
     fun _ ->
       let auctions = Auction.empty in
       let (auctions, _) =
         Auction.send_to_auction auctions { burrow = Address.of_string "12345"; tez = Tez.of_mutez 2_000_000; } in
       let auctions = Auction.touch auctions (Timestamp.of_seconds 0) Kit.one in
       assert_equal (Some (Tez.of_mutez 2_000_000)) (Auction.current_auction_tez auctions);
       assert_equal (Some (Kit.of_mutez 2_000_000)) (Auction.current_auction_bid_threshold auctions);

    );
  ]
