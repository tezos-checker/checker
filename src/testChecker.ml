open Kit
open OUnit2
open TestCommon

module PtrMap = Map.Make(Ptr)

let suite =
  "Checker tests" >::: [
    ("can complete a liquidation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = Checker.initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let _lqt_minted, _ret_kit, checker =
         Checker.add_liquidity
           checker
           ~max_kit_deposited:(kit_issue kit_one)
           ~min_lqt_minted:(Ligo.nat_from_literal "1n")
           ~deadline:(Ligo.timestamp_from_seconds_literal 1) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = Ligo.tez_from_literal "12_345_678mutez" in
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let (burrow_id, admin_permission, checker0) = Checker.create_burrow checker in
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let (payment, checker1) =
           Checker.deactivate_burrow
             checker0
             ~permission:admin_permission
             ~burrow_id
             ~recipient:bob_addr in
         assert_equal tez payment.amount ~printer:Ligo.string_of_tez;
         (* deactivation/activation = identity (if conditions are met ofc). *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let checker2 =
           Checker.activate_burrow
             checker1
             ~permission:admin_permission
             ~burrow_id in
         assert_equal checker0 checker2;
         () in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let (burrow_id, admin_permission, checker) = Checker.create_burrow checker in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (kit_token, checker) =
         Checker.mint_kit
           checker
           ~permission:admin_permission
           ~burrow_id:burrow_id
           ~kit:(kit_of_mukit (Ligo.int_from_literal "4_285_714")) in
       let kit, _same_token = read_kit kit_token in
       assert_equal (kit_of_mukit (Ligo.int_from_literal "4_285_714")) kit;

       assert_bool
         "should not be overburrowed right after minting"
         (not
          @@ Burrow.is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* Minting another kit should fail *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure "MintKitFailure")
         (fun () ->
            Checker.mint_kit
              checker
              ~permission:admin_permission
              ~burrow_id:burrow_id
              ~kit:(kit_of_mukit (Ligo.int_from_literal "1"))
         );

       (* Over time the burrows with outstanding kit should be overburrowed
          	* (NOTE: even if the index stays where it was before, but that would
          	* take more time I guess). *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let _touch_reward, checker =
         Checker.touch checker ~index:(Ligo.tez_from_literal "1_000_001mutez") in

       let checker = Checker.touch_burrow checker burrow_id in

       assert_bool
         "if the index goes up, then burrows should become overburrowed"
         (Burrow.is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* If enough time passes and the index remains up, then the burrow is even liquidatable. *)
       Ligo.Tezos.new_transaction ~seconds_passed:(211*60) ~blocks_passed:211 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let touch_reward, checker =
         Checker.touch checker ~index:(Ligo.tez_from_literal "1_200_000mutez") in

       let checker = Checker.touch_burrow checker burrow_id in

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.int_from_literal "202_000_000"))) (* wow, high reward, many blocks have passed. *)
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (reward_payment, checker) =
         Checker.mark_for_liquidation
           checker
           ~burrow_id:burrow_id in
       assert_equal (Ligo.tez_from_literal "1_008_999mutez") reward_payment.amount ~printer:Ligo.string_of_tez;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure "NoOpenAuction")
         (fun () ->
            Checker.liquidation_auction_place_bid
              checker
              ~kit:(kit_issue (kit_of_mukit (Ligo.int_from_literal "1_000")))
         );

       let touch_reward, checker =
         Checker.touch checker ~index:(Ligo.tez_from_literal "1_200_000mutez") in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.int_from_literal "500_000")))
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let touch_reward, checker =
         Checker.touch checker ~index:(Ligo.tez_from_literal "1_200_000mutez") in

       let (bid, checker) =
         Checker.liquidation_auction_place_bid
           checker
           ~kit:(kit_issue (kit_of_mukit (Ligo.int_from_literal "4_200_000"))) in

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.int_from_literal "500_000")))
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:(30*60) ~blocks_passed:30 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let touch_reward, checker =
         Checker.touch checker ~index:(Ligo.tez_from_literal "1_200_000mutez") in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.int_from_literal "21_000_000")))
         touch_reward
         ~printer:show_kit_token;

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
         (Ligo.tez_from_literal "0mutez")
         (Burrow.collateral_at_auction result)
         ~printer:Ligo.string_of_tez;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (tez_from_bid, _checker) =
         Checker.liquidation_auction_reclaim_winning_bid
           checker
           ~bid_ticket:bid in

       assert_equal
         (Tez.{destination = alice_addr; amount = Ligo.tez_from_literal "3_155_960mutez";})
         tez_from_bid
         ~printer:Tez.show_payment;
    );
  ]
