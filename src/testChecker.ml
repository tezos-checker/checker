open OUnit2
open Checker

let bob = Address.of_string "bob"
let alice = Address.of_string "alice"

let checker_address = Address.of_string "checker"

let assert_ok (r: ('a, Error.error) result) : 'a =
  match r with
  | Ok a -> a
  | Error LiquidationAuction.BidTooLow -> assert_failure "BidTooLow"
  | Error LiquidationAuction.NotAWinningBid -> assert_failure "NotAWinningBid"
  | Error LiquidationAuction.NotAllSlicesClaimed -> assert_failure "NotAllSlicesClaimed"
  | Error (Burrow.InsufficientFunds _) -> assert_failure "InsufficientFunds"
  | Error Burrow.WithdrawTezFailure -> assert_failure "WithdrawTezFailure"
  | Error Burrow.MintKitFailure -> assert_failure "MintKitFailure"
  | Error Checker.NonExistentBurrow _ -> assert_failure "NonExistentBurrow"
  | Error Checker.NotLiquidationCandidate _ -> assert_failure "NotLiquidationCandidate"
  | Error _ -> assert_failure "Unknown Error"

let suite =
  "Checker tests" >::: [
    ("can complete a liquidation auction" >::
     fun _ ->
       let tezos = Tezos.{
           now = Timestamp.of_seconds @@ 0;
           level = Level.of_int 0;
           self = checker_address;
         } in
       let checker = Checker.initialize tezos in

       let (_lqt_minted, _ret_tez, _ret_kit, checker) = assert_ok @@
         Checker.add_liquidity
           checker
           ~tezos
           ~call:{sender=alice; amount=Tez.one;}
           ~max_kit_deposited:(Kit.issue ~tezos Kit.one)
           ~min_lqt_minted:1
           ~deadline:(Timestamp.of_seconds 1) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = Tez.of_mutez 12_345_678 in
         let (burrow_id, admin_permission, checker0) = assert_ok @@
           Checker.create_burrow checker ~tezos ~call:{sender = bob; amount = tez;} in
         let (payment, checker1) = assert_ok @@
           Checker.deactivate_burrow
             checker0
             ~permission:admin_permission
             ~tezos ~call:{sender = bob; amount = Tez.zero;}
             ~burrow_id
             ~recipient:bob in
         assert_equal tez payment.amount ~printer:Tez.show;
         (* deactivation/activation = identity (if conditions are met ofc). *)
         let checker2 = assert_ok @@
           Checker.activate_burrow
             checker1
             ~permission:admin_permission
             ~tezos ~call:{sender = bob; amount = tez;}
             ~burrow_id in
         assert_equal checker0 checker2;
         () in

       let (burrow_id, admin_permission, checker) = assert_ok @@
         Checker.create_burrow
           checker
           ~tezos
           ~call:{sender = bob; amount = Tez.of_mutez 10_000_000;} in

       let (kit_token, checker) = assert_ok @@
         Checker.mint_kit
           checker
           ~tezos
           ~call:{sender=bob; amount=Tez.zero;}
           ~permission:admin_permission
           ~burrow_id:burrow_id
           ~kit:(Kit.of_mukit 4_285_714) in
       let kit, _same_token = Kit.read_kit kit_token in
       assert_equal (Kit.of_mukit 4_285_714) kit;

       let int_level = 5 in
       let tezos = Tezos.{
           now = Timestamp.of_seconds @@ int_level * 60;
           level = Level.of_int int_level;
           self = checker_address;
         } in

       let touch_reward, checker =
         Checker.touch
           checker
           ~tezos
           ~index:(Tez.of_mutez 1_200_000) in

       let checker = assert_ok @@
         Checker.touch_burrow checker burrow_id in

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit 500_000))
         touch_reward
         ~printer:Kit.show_token;

       let (reward_payment, checker) = assert_ok @@
         Checker.mark_for_liquidation
           checker
           ~call:{sender=alice; amount=Tez.zero;}
           ~burrow_id:burrow_id in
       assert_equal (Tez.of_mutez 1_008_999) reward_payment.amount ~printer:Tez.show;

       let int_level = 10 in
       let tezos = Tezos.{
           now = Timestamp.of_seconds @@ int_level * 60;
           level = Level.of_int int_level;
           self = checker_address;
         } in

       assert_equal
         (Error LiquidationAuction.NoOpenAuction)
         (Checker.liquidation_auction_place_bid
           checker
           ~tezos:tezos
           ~call:{sender=bob; amount = Tez.zero;}
           ~kit:(Kit.issue ~tezos (Kit.of_mukit 1_000)));

       let touch_reward, checker =
         Checker.touch
           checker
           ~tezos
           ~index:(Tez.of_mutez 1_200_000) in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit 500_000))
         touch_reward
         ~printer:Kit.show_token;

       let int_level = 15 in
       let tezos = Tezos.{
           now = Timestamp.of_seconds @@ int_level * 60;
           level = Level.of_int int_level;
           self = checker_address;
         } in

       let touch_reward, checker =
         Checker.touch
           checker
           ~tezos
           ~index:(Tez.of_mutez 1_200_000) in

       let (bid, checker) = assert_ok @@
         Checker.liquidation_auction_place_bid
           checker
           ~tezos
           ~call:{sender=alice; amount=Tez.zero;}
           ~kit:(Kit.issue ~tezos (Kit.of_mukit 4_200_000)) in

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit 500_000))
         touch_reward
         ~printer:Kit.show_token;

       let int_level = 45 in
       let tezos = Tezos.{
           now = Timestamp.of_seconds @@ int_level * 60;
           level = Level.of_int int_level;
           self = checker_address;
         } in

       let touch_reward, checker =
         Checker.touch
           checker
           ~tezos
           ~index:(Tez.of_mutez 1_200_000) in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit 21_000_000))
         touch_reward
         ~printer:Kit.show_token;

       (* We don't need to touch the slice on this test case since Checker.touch
        * already touches the oldest 5 slices. *)
       (*
       let slice =
         (PtrMap.find burrow_id checker.burrows)
         |> Burrow.liquidation_slices
         |> Option.get
         |> fun i -> i.youngest in

       let checker =
         Checker.touch_liquidation_slices
           checker
           [slice] in
       *)

       let result = PtrMap.find burrow_id checker.burrows in
       assert_bool "burrow should have no liquidation slices"
         (Option.is_none (Burrow.liquidation_slices result));

       assert_equal
         Tez.zero
         (Burrow.collateral_at_auction result)
         ~printer:Tez.show;

       let (tez_from_bid, _checker) = assert_ok @@
         Checker.liquidation_auction_reclaim_winning_bid
           checker
           ~tezos
           ~call:{sender=alice; amount=Tez.zero;}
           ~bid_ticket:bid in

       assert_equal
         (Tez.{destination = alice; amount = Tez.of_mutez 3_156_178;})
         tez_from_bid
         ~printer:Tez.show_payment;
    );
  ]
