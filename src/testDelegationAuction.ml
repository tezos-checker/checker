open OUnit2

let checker_address = Address.of_string "checker"
let start_time = Timestamp.of_seconds 0
let start_level = Level.of_int 0
let start_tezos = Tezos.{now = start_time; level = start_level; self = checker_address;}

type address_option = Address.t option [@@deriving show]

let suite =
  "Delegation auction tests" >::: [
    ("test initialisation" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option
    );

    ("test single bidder" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let bidder = Address.of_string "5678" in
       let amount = Tez.of_mutez (Z.of_int 1) in
       let (ticket, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder ~amount:amount) in
       (* New bidder does not immediately become the delegate and cannot claim the win *)
       let (delegate) = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction start_tezos ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction start_tezos ~bid_ticket:ticket));
       (* Nor at any time in the current cycle... *)
       let tezos = {start_tezos with level = Level.of_int 4095} in
       let auction = DelegationAuction.touch auction tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket));
       (* But in the next cycle they can claim the win... *)
       let tezos = {start_tezos with level = Level.of_int 4096} in
       let auction = DelegationAuction.touch auction tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~bid_ticket:ticket));
       let auction = Result.get_ok (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket) in
       let auction = DelegationAuction.touch auction tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal (Some bidder) delegate ~printer:show_address_option;
       (* And in the subsequent cycle they cease to be the delegate again *)
       let tezos = {start_tezos with level = Level.of_int (2 * 4096)} in
       let auction = DelegationAuction.touch auction tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket));
    );

    ("test outbidding" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let bidder1 = Address.of_string "1111" in
       let bidder2 = Address.of_string "2222" in
       let bidder3 = Address.of_string "3333" in
       let bidder4 = Address.of_string "4444" in
       let amount1 = Tez.of_mutez (Z.of_int 1) in
       let amount2 = Tez.of_mutez (Z.of_int 2) in
       let amount3 = Tez.of_mutez (Z.of_int 3) in
       let amount4 = Tez.of_mutez (Z.of_int 4) in
       let (ticket1, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder1 ~amount:amount1) in
       assert_bool "must match bid" (Result.is_error (DelegationAuction.place_bid auction start_tezos ~sender:bidder2 ~amount:amount1));
       let (ticket2, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder2 ~amount:amount2) in
       let (ticket3, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder3 ~amount:amount3) in
       let (ticket4, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder4 ~amount:amount4) in
       (* First bidder can now reclaim their bid *)
       let (refund, auction) = Result.get_ok (DelegationAuction.reclaim_bid auction start_tezos ~bid_ticket:ticket1) in
       assert_equal amount1 refund;
       (* But new leading bidder cannot reclaim their bid *)
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction start_tezos ~bid_ticket:ticket4));
       (* Then in the next cycle... *)
       let tezos = {start_tezos with level = Level.of_int 4096} in
       (* Refunds can still be claimed *)
       let (refund, auction) = Result.get_ok (DelegationAuction.reclaim_bid auction tezos ~bid_ticket:ticket2) in
       assert_equal amount2 refund;
       (* And the winner can claim their win *)
       let auction = Result.get_ok (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket4) in
       let auction = DelegationAuction.touch auction tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal (Some bidder4) delegate ~printer:show_address_option;
       (* But in the following cycle... *)
       let tezos = {start_tezos with level = Level.of_int 8200} in
       (* Refunds can no longer be claimed *)
       assert_bool "too late to reclaim losing bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~bid_ticket:ticket3));
    );

    ("test sanity when skipping multiple levels" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let bidder = Address.of_string "5678" in
       let amount = Tez.of_mutez (Z.of_int 1) in
       let (ticket, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder ~amount:amount) in
       (* And in the subsequent cycle they cease to be the delegate again *)
       let tezos = {start_tezos with level = Level.of_int (3 * 4096)} in
       let auction = DelegationAuction.touch auction tezos in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~bid_ticket:ticket));
    )
  ]
