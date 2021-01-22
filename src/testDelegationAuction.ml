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
       let (ticket, auction) = DelegationAuction.place_bid auction bidder amount in
       (* New bidder does not immediately become the delegate and cannot claim the win *)
       let (delegate) = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;

       assert_raises
         (Failure "CannotReclaimLeadingBid")
         (fun () -> DelegationAuction.reclaim_bid auction ticket);

       assert_raises
         (Failure "NotAWinningBid")
         (fun () -> DelegationAuction.claim_win auction ticket);

       (* Nor at any time in the current cycle... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4095 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;

       assert_raises
         (Failure "CannotReclaimLeadingBid")
         (fun () -> DelegationAuction.reclaim_bid auction ticket);

       assert_raises
         (Failure "NotAWinningBid")
         (fun () -> DelegationAuction.claim_win auction ticket);

       (* But in the next cycle they can claim the win... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_raises
         (Failure "CannotReclaimWinningBid")
         (fun () -> DelegationAuction.reclaim_bid auction ticket);
       let auction = DelegationAuction.claim_win auction ticket in
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal (Some bidder) delegate ~printer:show_address_option;
       (* And in the subsequent cycle they cease to be the delegate again *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4096 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_raises
         (Failure "BidTicketExpired")
         (fun () -> DelegationAuction.reclaim_bid auction ticket);

       assert_raises
         (Failure "NotAWinningBid")
         (fun () -> DelegationAuction.claim_win auction ticket);
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
       let (ticket1, auction) = DelegationAuction.place_bid auction bidder1 amount1 in
       assert_raises
         (Failure "BidTooLow")
         (fun () -> DelegationAuction.place_bid auction bidder2 amount1);

       let (ticket2, auction) = DelegationAuction.place_bid auction bidder2 amount2 in
       let (ticket3, auction) = DelegationAuction.place_bid auction bidder3 amount3 in
       let (ticket4, auction) = DelegationAuction.place_bid auction bidder4 amount4 in
       (* First bidder can now reclaim their bid *)
       let (refund, auction) = DelegationAuction.reclaim_bid auction ticket1 in
       assert_equal amount1 refund;
       (* But new leading bidder cannot reclaim their bid *)
       assert_raises
         (Failure "CannotReclaimLeadingBid")
         (fun () -> DelegationAuction.reclaim_bid auction ticket4);
       (* Then in the next cycle... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4096 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* Refunds can still be claimed *)
       let (refund, auction) = DelegationAuction.reclaim_bid auction ticket2 in
       assert_equal amount2 refund;
       (* And the winner can claim their win *)
       let auction = DelegationAuction.claim_win auction ticket4 in
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal (Some bidder4) delegate ~printer:show_address_option;
       (* But in the following cycle... *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:4104 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* Refunds can no longer be claimed *)
       assert_raises
         (Failure "BidTicketExpired")
         (fun () -> DelegationAuction.reclaim_bid auction ticket3)
    );

    ("test sanity when skipping multiple levels" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let auction = DelegationAuction.empty in
       let bidder = Ligo.address_from_literal "5678" in
       let amount = Ligo.tez_from_literal "1mutez" in
       let (ticket, auction) = DelegationAuction.place_bid auction bidder amount in
       (* And in the subsequent cycle they cease to be the delegate again *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:(3 * 4096) ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = DelegationAuction.touch auction in
       let delegate = DelegationAuction.delegate auction in
       assert_equal None delegate ~printer:show_address_option;
       assert_raises
         (Failure "CannotReclaimLeadingBid")
         (fun () -> DelegationAuction.reclaim_bid auction ticket)
    )
  ]
