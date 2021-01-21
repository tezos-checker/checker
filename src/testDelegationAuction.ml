open OUnit2
open TestCommon

let checker_address = Ligo.address_from_literal "checker"
let checker_amount = Ligo.tez_from_literal "0mutez"
let checker_sender = Ligo.address_from_literal "somebody"

type address_option = Ligo.address option [@@deriving show]

let suite =
  "Delegation auction tests" >::: [
    ("test initialisation" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auction = DelegationAuction.empty in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option
    );

    ("test single bidder" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auction = DelegationAuction.empty in
       let bidder = Ligo.address_from_literal "5678" in
       let amount = Ligo.tez_from_literal "1mutez" in
       let (ticket, auction) = Result.get_ok (DelegationAuction.place_bid auction ~sender:bidder ~amount:amount) in
       (* New bidder does not immediately become the delegate and cannot claim the win *)
       let (delegate) = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction ~bid_ticket:ticket));
       (* Nor at any time in the current cycle... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4095 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction ~bid_ticket:ticket));
       (* But in the next cycle they can claim the win... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket));
       let auction = Result.get_ok (DelegationAuction.claim_win auction ~bid_ticket:ticket) in
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal (Some bidder) delegate ~printer:show_address_option;
       (* And in the subsequent cycle they cease to be the delegate again *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4096 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket));
       assert_bool "cannot claim win" (Result.is_error (DelegationAuction.claim_win auction ~bid_ticket:ticket));
    );

    ("test outbidding" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auction = DelegationAuction.empty in
       let bidder1 = Ligo.address_from_literal "1111" in
       let bidder2 = Ligo.address_from_literal "2222" in
       let bidder3 = Ligo.address_from_literal "3333" in
       let bidder4 = Ligo.address_from_literal "4444" in
       let amount1 = Ligo.tez_from_literal "1mutez" in
       let amount2 = Ligo.tez_from_literal "2mutez" in
       let amount3 = Ligo.tez_from_literal "3mutez" in
       let amount4 = Ligo.tez_from_literal "4mutez" in
       let (ticket1, auction) = Result.get_ok (DelegationAuction.place_bid auction ~sender:bidder1 ~amount:amount1) in
       assert_bool "must match bid" (Result.is_error (DelegationAuction.place_bid auction ~sender:bidder2 ~amount:amount1));
       let (ticket2, auction) = Result.get_ok (DelegationAuction.place_bid auction ~sender:bidder2 ~amount:amount2) in
       let (ticket3, auction) = Result.get_ok (DelegationAuction.place_bid auction ~sender:bidder3 ~amount:amount3) in
       let (ticket4, auction) = Result.get_ok (DelegationAuction.place_bid auction ~sender:bidder4 ~amount:amount4) in
       (* First bidder can now reclaim their bid *)
       let (refund, auction) = Result.get_ok (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket1) in
       assert_equal amount1 refund;
       (* But new leading bidder cannot reclaim their bid *)
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket4));
       (* Then in the next cycle... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4096 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* Refunds can still be claimed *)
       let (refund, auction) = Result.get_ok (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket2) in
       assert_equal amount2 refund;
       (* And the winner can claim their win *)
       let auction = Result.get_ok (DelegationAuction.claim_win auction ~bid_ticket:ticket4) in
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal (Some bidder4) delegate ~printer:show_address_option;
       (* But in the following cycle... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4104 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* Refunds can no longer be claimed *)
       assert_bool "too late to reclaim losing bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket3));
    );

    ("test sanity when skipping multiple levels" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auction = DelegationAuction.empty in
       let bidder = Ligo.address_from_literal "5678" in
       let amount = Ligo.tez_from_literal "1mutez" in
       let (ticket, auction) = Result.get_ok (DelegationAuction.place_bid auction ~sender:bidder ~amount:amount) in
       (* And in the subsequent cycle they cease to be the delegate again *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:(3 * 4096) ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_bool "cannot reclaim leading bid" (Result.is_error (DelegationAuction.reclaim_bid auction ~bid_ticket:ticket));
    )
  ]
