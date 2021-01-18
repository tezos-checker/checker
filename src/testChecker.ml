open OUnit2
open TestCommon

module PtrMap = Map.Make(Ptr)

let bob = Ligo.address_from_literal "bob"
let alice = Ligo.address_from_literal "alice"

let make_tezos int_level =
  Tezos.{
    now = Ligo.timestamp_from_seconds_literal @@ int_level * 60;
    level = Level.of_int int_level;
    self = Ligo.address_from_literal "checker";
  }

let suite =
  "Checker tests" >::: [
    ("can complete a liquidation auction" >::
     fun _ ->
       let int_level = 0 in
       let tezos = make_tezos int_level in
       let checker = Checker.initialize tezos in

       let _lqt_minted, _ret_kit, checker = assert_ok @@
         Checker.add_liquidity
           checker
           ~tezos
           ~call:{sender = alice; amount = Ligo.tez_from_mutez_literal 1_000_000;}
           ~max_kit_deposited:(Kit.issue ~tezos Kit.one)
           ~min_lqt_minted:(Ligo.nat_from_literal 1)
           ~deadline:(Ligo.timestamp_from_seconds_literal 1) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = Ligo.tez_from_mutez_literal 12_345_678 in
         let (burrow_id, admin_permission, checker0) = assert_ok @@
           Checker.create_burrow checker ~tezos ~call:{sender = bob; amount = tez;} in
         let (payment, checker1) = assert_ok @@
           Checker.deactivate_burrow
             checker0
             ~permission:admin_permission
             ~tezos ~call:{sender = bob; amount = (Ligo.tez_from_mutez_literal 0);}
             ~burrow_id
             ~recipient:bob in
         assert_equal tez payment.amount ~printer:Ligo.string_of_tez;
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
           ~call:{sender = bob; amount = Ligo.tez_from_mutez_literal 10_000_000;} in

       (* Mint as much kit as possible *)
       let (kit_token, checker) = assert_ok @@
         Checker.mint_kit
           checker
           ~tezos
           ~call:{sender=bob; amount=(Ligo.tez_from_mutez_literal 0);}
           ~permission:admin_permission
           ~burrow_id:burrow_id
           ~kit:(Kit.of_mukit (Ligo.int_from_literal 4_285_714)) in
       let kit, _same_token = Kit.read_kit kit_token in
       assert_equal (Kit.of_mukit (Ligo.int_from_literal 4_285_714)) kit;

       assert_bool
         "should not be overburrowed right after minting"
         (not
          @@ Burrow.is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* Minting another kit should fail *)
       let () = TestCommon.assert_failwith Burrow.MintKitFailure @@
         Checker.mint_kit
           checker
           ~tezos
           ~call:{sender=bob; amount=(Ligo.tez_from_mutez_literal 0);}
           ~permission:admin_permission
           ~burrow_id:burrow_id
           ~kit:(Kit.of_mukit (Ligo.int_from_literal 1)) in

       (* Over time the burrows with outstanding kit should be overburrowed
          	* (NOTE: even if the index stays where it was before, but that would
          	* take more time I guess). *)
       let int_level = 1 in
       let tezos = make_tezos int_level in

       let _touch_reward, checker =
         Checker.touch checker ~tezos ~index:(Ligo.tez_from_mutez_literal 1_000_001) in

       let checker = assert_ok @@
         Checker.touch_burrow checker burrow_id in

       assert_bool
         "if the index goes up, then burrows should become overburrowed"
         (Burrow.is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* If enough time passes and the index remains up, then the burrow is even liquidatable. *)
       let int_level = 212 in
       let tezos = make_tezos int_level in

       let touch_reward, checker =
         Checker.touch checker ~tezos ~index:(Ligo.tez_from_mutez_literal 1_200_000) in

       let checker = assert_ok @@
         Checker.touch_burrow checker burrow_id in

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit (Ligo.int_from_literal 202_000_000))) (* wow, high reward, many blocks have passed. *)
         touch_reward
         ~printer:Kit.show_token;

       let (reward_payment, checker) = assert_ok @@
         Checker.mark_for_liquidation
           checker
           ~call:{sender=alice; amount=(Ligo.tez_from_mutez_literal 0);}
           ~burrow_id:burrow_id in
       assert_equal (Ligo.tez_from_mutez_literal 1_008_999) reward_payment.amount ~printer:Ligo.string_of_tez;

       let int_level = 217 in
       let tezos = make_tezos int_level in

       assert_equal
         (Error LiquidationAuction.NoOpenAuction)
         (Checker.liquidation_auction_place_bid
            checker
            ~tezos:tezos
            ~call:{sender=bob; amount = (Ligo.tez_from_mutez_literal 0);}
            ~kit:(Kit.issue ~tezos (Kit.of_mukit (Ligo.int_from_literal 1_000))));

       let touch_reward, checker =
         Checker.touch checker ~tezos ~index:(Ligo.tez_from_mutez_literal 1_200_000) in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit (Ligo.int_from_literal 500_000)))
         touch_reward
         ~printer:Kit.show_token;

       let int_level = 222 in
       let tezos = make_tezos int_level in

       let touch_reward, checker =
         Checker.touch checker ~tezos ~index:(Ligo.tez_from_mutez_literal 1_200_000) in

       let (bid, checker) = assert_ok @@
         Checker.liquidation_auction_place_bid
           checker
           ~tezos
           ~call:{sender=alice; amount=(Ligo.tez_from_mutez_literal 0);}
           ~kit:(Kit.issue ~tezos (Kit.of_mukit (Ligo.int_from_literal 4_200_000))) in

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit (Ligo.int_from_literal 500_000)))
         touch_reward
         ~printer:Kit.show_token;

       let int_level = 252 in
       let tezos = make_tezos int_level in

       let touch_reward, checker =
         Checker.touch checker ~tezos ~index:(Ligo.tez_from_mutez_literal 1_200_000) in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_equal
         (Kit.issue ~tezos (Kit.of_mukit (Ligo.int_from_literal 21_000_000)))
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

       let result = Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows) in
       assert_bool "burrow should have no liquidation slices"
         (Option.is_none (Burrow.liquidation_slices result));

       assert_equal
         (Ligo.tez_from_mutez_literal 0)
         (Burrow.collateral_at_auction result)
         ~printer:Ligo.string_of_tez;

       let (tez_from_bid, _checker) = assert_ok @@
         Checker.liquidation_auction_reclaim_winning_bid
           checker
           ~tezos
           ~call:{sender=alice; amount=(Ligo.tez_from_mutez_literal 0);}
           ~bid_ticket:bid in

       assert_equal
         (Tez.{destination = alice; amount = Ligo.tez_from_mutez_literal 3_155_960;})
         tez_from_bid
         ~printer:Tez.show_payment;
    );
  ]
