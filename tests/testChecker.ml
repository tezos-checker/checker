open Kit
open Burrow
open OUnit2
open TestCommon
open CheckerTypes
open Tickets
open Error
open Ptr

module PtrMap = Map.Make(struct type t = ptr let compare = compare_ptr end)

type operation_list = LigoOp.operation list
[@@deriving show]


(* Helper for creating new burrows and extracting their ID and admin ticket from the corresponding Ligo Ops *)
let newly_created_burrow checker =
  let ops, checker = Checker.create_burrow checker None in 
  match ops with
  | [ CreateContract _ ;
      Transaction (PermTransactionValue burrow_permission, _, _) ;
      Transaction (AddressTransactionValue burrow_id, _, _) ;
    ] -> (burrow_id, burrow_permission, checker)
  | _ -> failwith ("Expected CreateContract, PermTransaction, and AddressTransaction but got " ^ show_operation_list ops)


let suite =
  "Checker tests" >::: [
    ("initial touch" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = initial_checker in
       let _ = Checker.touch_with_index checker (Ligo.tez_from_literal "0mutez"); in
       ()
    );

    ("create_burrow - updates checker storage" >::
     fun _ -> 
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");

       let burrow_id, _, checker = newly_created_burrow initial_checker in 

        assert_bool 
          "No matching burrow found after calling create_burrow" 
          (Option.is_some (Ligo.Big_map.find_opt burrow_id checker.burrows))
    );

    ("create_burrow - collatoral in burrow representation does not include creation deposit" >::
     fun _ -> 
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Constants.creation_deposit;

       let burrow_id, _, checker = newly_created_burrow initial_checker in

       let expected_collateral = Ligo.tez_from_literal "0mutez" in
       match Ligo.Big_map.find_opt burrow_id checker.burrows with 
       | Some burrow -> assert_bool "Burrow representation has unexpected collateral value" (Ligo.eq_tez_tez (burrow_collateral burrow) expected_collateral)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("create_burrow - fails when transaction does not meet creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "42mutez");

       assert_raises 
         (Failure (Ligo.string_of_int error_InsufficientFunds))
         (fun () -> Checker.create_burrow initial_checker None)
    );

    ("deposit_tez - admin ticket holder can deposit" >::
     fun _ -> 
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let expected_collateral = Ligo.add_tez_tez deposit (Ligo.sub_tez_tez  initial_deposit Constants.creation_deposit) in

       (* Create the burrow *)
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let burrow_id, admin_ticket, checker = newly_created_burrow initial_checker in
       (* Make a deposit *)
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:deposit;
       let _, checker = Checker.deposit_tez checker (Some admin_ticket) burrow_id in  

       match Ligo.Big_map.find_opt burrow_id checker.burrows with 
       | Some burrow -> assert_bool "Burrow representation has unexpected collateral value" (Ligo.eq_tez_tez (burrow_collateral burrow) expected_collateral)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("deposit_tez - non-ticket holder can not deposit by default" >::
     fun _ -> 
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");

       let burrow_id, _, checker = newly_created_burrow initial_checker in

       assert_raises 
         (Failure (Ligo.string_of_int error_MissingPermission))
         (fun () -> Checker.deposit_tez checker None burrow_id)
    );

    ("deposit_tez - fail if the ticket to another burrow is submitted" >::
     fun _ -> 
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       let burrow_id, _, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       let _, some_other_ticket, checker = newly_created_burrow checker in

       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       assert_raises 
         (Failure (Ligo.string_of_int error_InvalidPermission))
         (fun () -> Checker.deposit_tez checker (Some some_other_ticket) burrow_id)
    );

    ("withdraw_tez - admin ticket holder can withdraw" >::
     fun _ -> 
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
       let expected_collateral = Ligo.sub_tez_tez initial_deposit (Ligo.add_tez_tez Constants.creation_deposit withdrawal) in
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       let burrow_id, admin_ticket, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.withdraw_tez checker admin_ticket withdrawal burrow_id in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with 
       | Some burrow -> assert_bool "Burrow representation has unexpected collateral value" (Ligo.eq_tez_tez (burrow_collateral burrow) expected_collateral)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("withdraw_tez - transaction with value > 0 fails" >::
     fun _ -> 
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let burrow_id, admin_ticket, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "42mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> Checker.withdraw_tez checker admin_ticket withdrawal burrow_id) 
    );

    ("withdraw_tez - fail if the ticket to another burrow is submitted" >::
     fun _ -> 
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in

       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let burrow_id, _, checker = newly_created_burrow initial_checker in
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       let _, some_other_ticket, checker = newly_created_burrow checker in

       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidPermission))
         (fun () -> Checker.withdraw_tez checker some_other_ticket withdrawal burrow_id) 
    );

    ("buy_kit - returns updated uniswap state" >::
     fun _ -> 
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "9_000_000mutez");
       let uniswap0 = Uniswap.uniswap_make_for_test
           ~tez:(Ligo.tez_from_literal "10_000_000mutez")
           ~kit:(kit_of_mukit (Ligo.nat_from_literal "5_000_000n"))
           ~lqt:(Ligo.nat_from_literal "1n")
           ~kit_in_tez_in_prev_block:Ratio.one_ratio
           ~last_level:(Ligo.nat_from_literal "0n") in
       let checker0 = {initial_checker with uniswap=uniswap0} in

       let _, checker = Checker.buy_kit checker0 (kit_of_mukit (Ligo.nat_from_literal "1n")) (Ligo.timestamp_from_seconds_literal 1) in 

       assert_bool
         "The uniswap returned by buy_kit is equal to the input uniswap but a new state was expected"
         (not (checker.uniswap = checker0.uniswap))

    );

    ("can complete a liquidation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = initial_checker in

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
         let (ops, checker0) = Checker.create_burrow checker None in

         (* created burrow should be deposited (incl. the creation deposit) *)
         let admin_permission, burrow_id = match ops with
           | [ CreateContract (_, _, sent_tez, _) ;
               Transaction (PermTransactionValue admin_permission, _, _) ;
               Transaction (AddressTransactionValue burrow_id, _, _) ;
             ] ->
             assert_equal tez sent_tez ~printer:Ligo.string_of_tez;
             (admin_permission, burrow_id)
           | _ -> assert_failure ("Expected CreateContract, PermTransaction, and AddressTransaction but got " ^ show_operation_list ops)
         in

         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let (ops, checker1) = Checker.deactivate_burrow checker0 admin_permission burrow_id in
         assert_equal
           ~printer:show_operation_list
           [LigoOp.Tezos.tez_address_transaction (tez, bob_addr) (Ligo.tez_from_literal "0mutez") (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id))]
           ops;
         (* deactivation/activation = identity (if conditions are met ofc). *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let _ops, checker2 = Checker.activate_burrow checker1 admin_permission burrow_id in
         assert_equal checker0 checker2;
         () in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let (ops, checker) = Checker.create_burrow checker None in

       let admin_permission, burrow_id = match ops with
         | [ CreateContract (_, _, _, _) ;
             Transaction (PermTransactionValue admin_permission, _, _) ;
             Transaction (AddressTransactionValue burrow_id, _, _) ;
           ] -> (admin_permission, burrow_id)
         | _ -> assert_failure ("Expected CreateContract, PermTransaction, and AddressTransaction but got " ^ show_operation_list ops)
       in

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

       assert_equal (kit_issue (kit_of_mukit (Ligo.nat_from_literal "4_285_714n"))) kit_token;

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
         (Failure (Ligo.string_of_int error_MintKitFailure))
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
         Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_001mutez") in

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

       let ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (_ :: Transaction (KitTransactionValue ticket, _, _) :: []) -> ticket
         | _ -> assert_failure ("Expected (_ :: Transaction (KitTransactionValue ticket, _, _) :: []) but got " ^ show_operation_list ops)
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
         (Failure (Ligo.string_of_int error_NoOpenAuction))
         (fun () ->
            Checker.checker_liquidation_auction_place_bid
              checker
              (kit_issue (kit_of_mukit (Ligo.nat_from_literal "1_000n")))
         );

       let ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (_ :: Transaction (KitTransactionValue ticket, _, _) :: []) -> ticket
         | _ -> assert_failure ("Expected (_ :: Transaction (KitTransactionValue ticket, _, _) :: []) but got " ^ show_operation_list ops)
       in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "500_000n")))
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (_ :: Transaction (KitTransactionValue ticket, _, _) :: []) -> ticket
         | _ -> assert_failure ("Expected (_ :: Transaction (KitTransactionValue ticket, _, _) :: []) but got " ^ show_operation_list ops)
       in

       let (ops, checker) =
         Checker.checker_liquidation_auction_place_bid
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

       let ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in

       let touch_reward = match ops with
         | (_ :: Transaction (KitTransactionValue ticket, _, _) :: _) -> ticket
         | _ -> assert_failure ("Expected (_ :: Transaction (KitTransactionValue ticket, _, _) :: _) but got " ^ show_operation_list ops)
       in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "21_000_000n")))
         touch_reward
         ~printer:show_kit_token;

       (* We don't need to touch the slice on this test case since Checker.touch_with_index
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
       let (ops, _checker) = Checker.checker_liquidation_auction_reclaim_winning_bid checker bid in

       assert_equal
         [LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "3_155_960mutez") (Option.get (LigoOp.Tezos.get_contract_opt alice_addr))]
         ops;
    );

    ("Can't claim delegation too soon after winning delegation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = initial_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let ops, checker = Checker.checker_delegation_auction_place_bid checker in

       let ticket = match ops with
         | [Transaction (DaBidTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (DaBidTransactionValue ticket, _, _)] but got " ^ show_operation_list ops)
       in

       assert_raises (Failure (Ligo.string_of_int error_NotAWinningBid)) (fun _ ->
           Ligo.Tezos.new_transaction ~seconds_passed:(60 * 4095) ~blocks_passed:4095 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
           let _checker = Checker.checker_delegation_auction_claim_win checker ticket charles_key_hash in
           ());
    );

    ("Can't claim delegation too late after winning delegation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = initial_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let ops, checker = Checker.checker_delegation_auction_place_bid checker in

       let ticket = match ops with
         | [Transaction (DaBidTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (DaBidTransactionValue ticket, _, _)] but got " ^ show_operation_list ops)
       in

       assert_raises (Failure (Ligo.string_of_int error_NotAWinningBid)) (fun _ ->
           Ligo.Tezos.new_transaction ~seconds_passed:(60 * 9000) ~blocks_passed:9000 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
           let _checker = Checker.checker_delegation_auction_claim_win checker ticket charles_key_hash in
           ());
    );

    ("Can claim delegation after winning delegation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = initial_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let ops, checker = Checker.checker_delegation_auction_place_bid checker in

       let ticket = match ops with
         | [Transaction (DaBidTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (DaBidTransactionValue ticket, _, _)] but got " ^ show_operation_list ops)
       in

       Ligo.Tezos.new_transaction ~seconds_passed:(60 * 4096) ~blocks_passed:4096 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _checker = Checker.checker_delegation_auction_claim_win checker ticket  charles_key_hash in
       assert_equal [LigoOp.SetDelegate (Some charles_key_hash)] ops;
    );
  ]
