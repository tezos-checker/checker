open Kit
open Burrow
open OUnit2
open TestCommon
open TokenTypes

module PtrMap = Map.Make(Ptr)

type operation_list = LigoOp.operation list
[@@deriving show]

let suite =
  "Checker tests" >::: [
    ("can complete a liquidation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = Checker.initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let _lqt_minted_ret_kit_ops, checker =
         Checker.add_liquidity
           checker
           (kit_issue kit_one)
           (Ligo.nat_from_literal "1n")
           (Ligo.timestamp_from_seconds_literal 1) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = Ligo.tez_from_literal "12_345_678mutez" in
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let (ops, burrow_id, admin_permission, checker0) = Checker.create_burrow checker in

         (* created burrow should be deposited (incl. the creation deposit) *)
         match ops with
         | [CreateContract (_, _, sent_tez, _)] -> assert_equal tez sent_tez ~printer:Ligo.string_of_tez;
         | _ -> (assert_failure ("Expected CreateContract but got " ^ show_operation_list ops): unit);

           Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
           let (ops, checker1) = Checker.deactivate_burrow checker0 admin_permission burrow_id bob_addr in
           assert_equal
             [LigoOp.Tezos.unit_transaction () tez (Option.get (LigoOp.Tezos.get_contract_opt bob_addr))]
             ops;
           (* deactivation/activation = identity (if conditions are met ofc). *)
           Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
           let _ops, checker2 = Checker.activate_burrow checker1 admin_permission burrow_id in
           assert_equal checker0 checker2;
           () in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let (_ops, burrow_id, admin_permission, checker) = Checker.create_burrow checker in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.mint_kit
           checker
           admin_permission
           burrow_id
           (kit_of_mukit (Ligo.nat_from_literal "4_285_714n")) in

       let kit_token = match ops with
         | [Transaction (KitTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       let kit, _same_token = read_kit kit_token in
       assert_equal (kit_of_mukit (Ligo.nat_from_literal "4_285_714n")) kit;

       assert_bool
         "should not be overburrowed right after minting"
         (not
          @@ burrow_is_overburrowed
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
              admin_permission
              burrow_id
              (kit_of_mukit (Ligo.nat_from_literal "1n"))
         );

       (* Over time the burrows with outstanding kit should be overburrowed
          	* (NOTE: even if the index stays where it was before, but that would
          	* take more time I guess). *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let _ops, checker =
         Checker.touch checker (Ligo.tez_from_literal "1_000_001mutez") in

       let ops, checker = Checker.touch_burrow checker burrow_id in
       assert_equal [] ops;

       assert_bool
         "if the index goes up, then burrows should become overburrowed"
         (burrow_is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* If enough time passes and the index remains up, then the burrow is even liquidatable. *)
       Ligo.Tezos.new_transaction ~seconds_passed:(211*60) ~blocks_passed:211 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let ops, checker = Checker.touch checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (Transaction (KitTransactionValue ticket, _, _) :: _) -> ticket
         | _ -> assert_failure ("Expected (Transaction (KitTransactionValue ticket, _, _) :: _) but got " ^ show_operation_list ops)
       in

       let ops, checker = Checker.touch_burrow checker burrow_id in
       assert_equal [] ops;

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "202_000_000n"))) (* wow, high reward, many blocks have passed. *)
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) = Checker.mark_for_liquidation checker burrow_id in
       assert_equal
         [LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "1_008_999mutez") (Option.get (LigoOp.Tezos.get_contract_opt alice_addr))]
         ops;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure "NoOpenAuction")
         (fun () ->
            Checker.liquidation_auction_place_bid
              checker
              (kit_issue (kit_of_mukit (Ligo.nat_from_literal "1_000n")))
         );

       let ops, checker = Checker.touch checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (Transaction (KitTransactionValue ticket, _, _) :: _) -> ticket
         | _ -> assert_failure ("Expected (Transaction (KitTransactionValue ticket, _, _) :: _) but got " ^ show_operation_list ops)
       in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "500_000n")))
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let ops, checker = Checker.touch checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (Transaction (KitTransactionValue ticket, _, _) :: _) -> ticket
         | _ -> assert_failure ("Expected (Transaction (KitTransactionValue ticket, _, _) :: _) but got " ^ show_operation_list ops)
       in

       let (ops, checker) =
         Checker.liquidation_auction_place_bid
           checker
           (kit_issue (kit_of_mukit (Ligo.nat_from_literal "4_200_000n"))) in

       let bid = match ops with
         | (Transaction (LaBidTransactionValue ticket, _, _) :: _) -> ticket
         | _ -> assert_failure ("Expected (Transaction (LaBidTransactionValue ticket, _, _) :: _) but got " ^ show_operation_list ops)
       in

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "500_000n")))
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:(30*60) ~blocks_passed:30 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let ops, checker = Checker.touch checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (Transaction (KitTransactionValue ticket, _, _) :: _) -> ticket
         | _ -> assert_failure ("Expected (Transaction (KitTransactionValue ticket, _, _) :: _) but got " ^ show_operation_list ops)
       in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "21_000_000n")))
         touch_reward
         ~printer:show_kit_token;

       (* We don't need to touch the slice on this test case since Checker.touch
        * already touches the oldest 5 slices. *)
       (*
       let slice =
         (PtrMap.find burrow_id checker.burrows)
         |> burrow_liquidation_slices
         |> Option.get
         |> fun i -> i.youngest in

       let checker =
         Checker.touch_liquidation_slices
           checker
           [slice] in
       *)

       let result = Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows) in
       assert_bool "burrow should have no liquidation slices"
         (Option.is_none (burrow_liquidation_slices result));

       assert_equal
         (Ligo.tez_from_literal "0mutez")
         (burrow_collateral_at_auction result)
         ~printer:Ligo.string_of_tez;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, _checker) = Checker.liquidation_auction_reclaim_winning_bid checker bid in

       assert_equal
         [LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "3_155_960mutez") (Option.get (LigoOp.Tezos.get_contract_opt alice_addr))]
         ops;
    );

    ("Can't claim delegation too soon after winning delegation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = Checker.initial_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let ops, checker = Checker.delegation_auction_place_bid checker in

       let ticket = match ops with
         | [Transaction (DaBidTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (DaBidTransactionValue ticket, _, _)] but got " ^ show_operation_list ops)
       in

       assert_raises (Failure "NotAWinningBid") (fun _ ->
           Ligo.Tezos.new_transaction ~seconds_passed:(60 * 4095) ~blocks_passed:4095 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
           let _checker = Checker.delegation_auction_claim_win checker ticket charles_key_hash in
           ());
    );

    ("Can't claim delegation too late after winning delegation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = Checker.initial_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let ops, checker = Checker.delegation_auction_place_bid checker in

       let ticket = match ops with
         | [Transaction (DaBidTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (DaBidTransactionValue ticket, _, _)] but got " ^ show_operation_list ops)
       in

       assert_raises (Failure "NotAWinningBid") (fun _ ->
           Ligo.Tezos.new_transaction ~seconds_passed:(60 * 9000) ~blocks_passed:9000 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
           let _checker = Checker.delegation_auction_claim_win checker ticket charles_key_hash in
           ());
    );

    ("Can claim delegation after winning delegation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = Checker.initial_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let ops, checker = Checker.delegation_auction_place_bid checker in

       let ticket = match ops with
         | [Transaction (DaBidTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (DaBidTransactionValue ticket, _, _)] but got " ^ show_operation_list ops)
       in

       Ligo.Tezos.new_transaction ~seconds_passed:(60 * 4096) ~blocks_passed:4096 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _checker = Checker.delegation_auction_claim_win checker ticket  charles_key_hash in
       assert_equal [LigoOp.SetDelegate (Some charles_key_hash)] ops;
    );
  ]
