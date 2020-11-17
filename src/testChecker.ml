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
       let checker = Checker.initialize t0 in

       let (burrow_id, checker) = assert_ok @@
         Checker.create_burrow
           checker
           ~owner:bob
           ~amount:(Tez.of_mutez 10_000_000) in

       let (kit, checker) = assert_ok @@
         Checker.mint_kit
           checker
           ~owner:bob
           ~burrow_id:burrow_id
           ~amount:(Kit.of_mukit 4_285_714) in
       assert_equal kit (Kit.of_mukit 4_285_714);

       let height = 5 in
       let now = Timestamp.of_seconds @@ height * 60 in

       let checker =
         Checker.touch
           checker
           ~now ~height
           ~index:(FixedPoint.of_string "1.2") in

       let checker = assert_ok @@
         Checker.touch_burrow checker burrow_id in

       let (reward, checker) = assert_ok @@
         Checker.mark_for_liquidation
           checker
           ~liquidator:alice
           ~burrow_id:burrow_id in
       assert_equal reward (Tez.of_mutez 1_009_000) ~printer:Tez.show;

       let height = 10 in
       let now = Timestamp.of_seconds @@ height * 60 in

       let checker =
         Checker.touch
           checker
           ~now ~height
           ~index:(FixedPoint.of_string "1.2") in

       assert_bool "should start an auction"
         (Option.is_some checker.auctions.current_auction);

       let height = 15 in
       let now = Timestamp.of_seconds @@ height * 60 in

       let checker =
         Checker.touch
           checker
           ~now ~height
           ~index:(FixedPoint.of_string "1.2") in

       let (bid, checker) = assert_ok @@
         Checker.place_bid
           checker
           ~now ~height
           ~sender:alice
           ~amount:(Kit.of_mukit 4_200_000) in

       let height = 45 in
       let now = Timestamp.of_seconds @@ height * 60 in

       let checker =
         Checker.touch
           checker
           ~now ~height
           ~index:(FixedPoint.of_string "1.2") in

       assert_bool "auction should be completed"
         (Option.is_none checker.auctions.current_auction);

       let tez_from_bid = assert_ok @@
         Checker.reclaim_winning_bid
           checker
           ~address:alice
           ~bid_ticket:bid in

       assert_equal (Tez.of_mutez 3_156_177) tez_from_bid
         ~printer:Tez.show;

       let slice =
         (PtrMap.find burrow_id checker.burrows)
         .liquidation_slices
         |> Option.get
         |> fun i -> i.youngest in

       let checker =
         Checker.touch_liquidation_slices
           checker
           [slice] in

       let result = PtrMap.find burrow_id checker.burrows in
       assert_bool "burrow should have no liquidation slices"
         (Option.is_none result.liquidation_slices);

       assert_equal
         Tez.zero
         result.collateral_at_auction
         ~printer:Tez.show;
    );
  ]
