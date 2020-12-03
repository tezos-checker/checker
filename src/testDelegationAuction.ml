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
       let (delegate, _auction) = DelegationAuction.delegate auction start_tezos in
       assert_equal None delegate ~printer:show_address_option
    );

    ("test single bidder" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let bidder = Address.of_string "5678" in
       let amount = Tez.of_mutez 1 in
       let (ticket, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder ~amount:amount) in
       (* New bidder does not immediately become the delegate and cannot claim the win *)
       let (delegate, auction) = DelegationAuction.delegate auction start_tezos in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction start_tezos ~address:bidder ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction start_tezos ~bid_ticket:ticket));
       (* Nor at any time in the current cycle... *)
       let tezos = {start_tezos with level = Level.of_int 4095} in
       let (delegate, auction) = DelegationAuction.delegate auction tezos in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~address:bidder ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket));
       (* But in the next cycle they can claim the win... *)
       let tezos = {start_tezos with level = Level.of_int 4096} in
       let (delegate, auction) = DelegationAuction.delegate auction tezos in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~address:bidder ~bid_ticket:ticket));
       let auction = Result.get_ok (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket) in
       let (delegate, auction) = DelegationAuction.delegate auction tezos in
       assert_equal (Some bidder) delegate ~printer:show_address_option;
       (* And in the subsequent cycle they cease to be the delegate again *)
       let tezos = {start_tezos with level = Level.of_int (2 * 4096)} in
       let (delegate, _auction) = DelegationAuction.delegate auction tezos in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~address:bidder ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket));
    );

    ("test outbidding" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let bidder1 = Address.of_string "1111" in
       let bidder2 = Address.of_string "2222" in
       let amount1 = Tez.of_mutez 1 in
       let amount2 = Tez.of_mutez 2 in
       let (ticket1, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder1 ~amount:amount1) in
       assert_bool "must match bid" (Result.is_error (DelegationAuction.place_bid auction start_tezos ~sender:bidder2 ~amount:amount1));
       let (ticket2, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder2 ~amount:amount2) in
       (* First bidder can now reclaim their bid *)
       let (refund, auction) = Result.get_ok (DelegationAuction.reclaim_bid auction start_tezos ~address:bidder1 ~bid_ticket:ticket1) in
       assert_equal amount1 refund;
       (* But new leading bidder cannot reclaim their bid *)
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction start_tezos ~address:bidder2 ~bid_ticket:ticket2));
       (* And they can claim their win in the next round *)
       let tezos = {start_tezos with level = Level.of_int 4096} in
       let auction = Result.get_ok (DelegationAuction.claim_win auction tezos ~bid_ticket:ticket2) in
       let (delegate, _auction) = DelegationAuction.delegate auction tezos in
       assert_equal (Some bidder2) delegate ~printer:show_address_option;
    );

    ("test sanity when skipping multiple levels" >::
     fun _ ->
       let auction = DelegationAuction.empty start_tezos in
       let bidder = Address.of_string "5678" in
       let amount = Tez.of_mutez 1 in
       let (ticket, auction) = Result.get_ok (DelegationAuction.place_bid auction start_tezos ~sender:bidder ~amount:amount) in
       (* And in the subsequent cycle they cease to be the delegate again *)
       let tezos = {start_tezos with level = Level.of_int (3 * 4096)} in
       let (delegate, _auction) = DelegationAuction.delegate auction tezos in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction tezos ~address:bidder ~bid_ticket:ticket));
    )
  ]
