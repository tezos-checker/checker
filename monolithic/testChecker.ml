open OUnit2
open Checker
open Burrow

let bob = Address.of_string "bob"
let alice = Address.of_string "alice"

let assert_ok (r: ('a, Error.error) result) : 'a =
  match r with
    | Ok a -> a
    | Error Auction.BidTooLow -> assert_failure "BidTooLow"
    | Error (Burrow.InsufficientFunds _) -> assert_failure "InsufficientFunds"
    | Error Burrow.WithdrawTezFailure -> assert_failure "WithdrawTezFailure"
    | Error Burrow.MintKitFailure -> assert_failure "MintKitFailure"
    | Error Checker.OwnershipMismatch _ -> assert_failure "OwnershipMismatch"
    | Error Checker.NonExistentBurrow _ -> assert_failure "NonExistentBurrow"
    | Error Checker.NotLiquidationCandidate _ -> assert_failure "NotLiquidationCandidate"
    | Error _ -> assert_failure "Unknown Error"

let suite =
  "Checker tests" >::: [
    ("can complete an auction" >::
     fun _ ->
       let t0 = Timestamp.of_seconds 0 in
       let checker = Checker.empty_checker_state t0 in

       let (burrow_id, checker) = assert_ok @@
         Checker.create_burrow
           checker
           ~owner:bob
           ~amount:(Tez.of_mutez 2_000_000) in

       let (kit, checker) = assert_ok @@
         Checker.mint_kit
           checker
           ~owner:bob
           ~burrow_id:burrow_id
           ~amount:(Kit.of_mukit 476_190) in
       assert_equal kit (Kit.of_mukit 476_190);

       let t1 = Timestamp.of_seconds 300 in
       let checker =
         Checker.touch
           checker
           ~now:t1
           ~index:(FixedPoint.of_string "1.2") in

       let checker = assert_ok @@
         Checker.touch_burrow checker burrow_id in

       let (reward, checker) = assert_ok @@
         Checker.mark_for_liquidation
           checker
           ~liquidator:alice
           ~burrow_id:burrow_id in
       assert_equal reward (Tez.of_mutez 1_001_000) ~printer:Tez.show;

       let t2 = Timestamp.of_seconds 600 in

       let checker =
         Checker.touch
           checker
           ~now:t2
           ~index:(FixedPoint.of_string "1.2") in

       assert_bool "should start an auction"
         (Option.is_some checker.auctions.current_auction);

       (* Now we should make a bid to the auction, let it finish,
        * touch the liquidation slice and see if it propagates
        * to the burrow correctly. *)
       assert_bool "TODO" true
    );
  ]
