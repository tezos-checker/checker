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


(* Helpers for setting up test environment *)

(* Calls a function using a clean, reset context *)
let with_clean_context f = 
  Ligo.Tezos.reset ();
  f ()

(* Calls a function after registering the specified transaction in the context *)
  let with_transaction sender amount f = 
  let run_with_trans = fun () -> 
    Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:amount;
    f ()
  in with_clean_context run_with_trans

  (* Calls a function which operates on the burrow_id of a new burrow created for the sender and has access to the admin ticket *)
let with_burrow : checker -> Ligo.address -> Ligo.tez -> ((burrow_id * permission * checker) -> 'a) -> 'a = fun checker sender initial_amount f -> 
  let run_with_burrow = fun () ->
    let ops, checker = Checker.create_burrow checker None in match ops with
        | [ CreateContract _ ;
            Transaction (PermTransactionValue burrow_permission, _, _) ;
            Transaction (AddressTransactionValue burrow_id, _, _) ;
          ] -> f (burrow_id, burrow_permission, checker)
       | _ -> failwith ("Expected CreateContract, PermTransaction, and AddressTransaction but got " ^ show_operation_list ops)
  in with_transaction sender initial_amount run_with_burrow


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
        with_burrow 
          initial_checker
          alice_addr 
          (Ligo.tez_from_literal "1_000_000mutez") 
          (fun (burrow_id, _, checker) -> 
            assert_bool 
              "No matching burrow found after calling create_burrow" 
              (Option.is_some (Ligo.Big_map.find_opt burrow_id checker.burrows))
          )
    );
    ("create_burrow - collatoral in burrow representation does not include creation deposit" >::
      fun _ -> 
        let (burrow_id, _, checker) = with_burrow 
          initial_checker
          alice_addr 
          Constants.creation_deposit
          (fun x -> x) in

        let expected_collateral = Ligo.tez_from_literal "0mutez" in
        match Ligo.Big_map.find_opt burrow_id checker.burrows with 
          | Some burrow -> assert_bool "Burrow representation has unexpected collateral value" (Ligo.eq_tez_tez (burrow_collateral burrow) expected_collateral)
          | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );
    ("create_burrow - fails when transaction does not meet creation deposit" >::
      fun _ ->
        assert_raises 
        (Failure (Ligo.string_of_int error_InsufficientFunds))
        (fun () -> with_burrow
          initial_checker 
          alice_addr 
          (Ligo.tez_from_literal "42mutez")
          (fun _ -> ())
        )
    );
    ("deposit_tez - admin ticket holder can deposit" >::
      fun _ -> 
        (* Create burrow *)
        let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
        let deposit = Ligo.tez_from_literal "3_000_000mutez" in
        let expected_collateral = Ligo.add_tez_tez deposit (Ligo.sub_tez_tez  initial_deposit Constants.creation_deposit) in
        let (burrow_id, admin_ticket, checker) = with_burrow 
          initial_checker
          alice_addr 
          initial_deposit
          (fun x -> x) in

        (* Make a deposit *)
        let _, checker = with_transaction 
          alice_addr 
          deposit
          (fun () ->  (Checker.deposit_tez checker (Some admin_ticket) burrow_id)) in  

        match Ligo.Big_map.find_opt burrow_id checker.burrows with 
        | Some burrow -> assert_bool "Burrow representation has unexpected collateral value" (Ligo.eq_tez_tez (burrow_collateral burrow) expected_collateral)
        | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );
    ("deposit_tez - non-ticket holder can not deposit by default" >::
    fun _ -> 
      let (burrow_id, _, checker) = with_burrow 
        initial_checker
        alice_addr 
        (Ligo.tez_from_literal "3_000_000mutez")
        (fun x -> x) in

       assert_raises 
       (Failure (Ligo.string_of_int error_MissingPermission))
        (fun () -> with_transaction 
          alice_addr 
          (Ligo.tez_from_literal "3_000_000mutez")
          (fun () -> Checker.deposit_tez checker None burrow_id)
        )
    );
    ("deposit_tez - fail if the ticket to another burrow is submitted" >::
    fun _ -> 
      let (burrow_id, _, checker) = with_burrow 
        initial_checker
        alice_addr 
        (Ligo.tez_from_literal "3_000_000mutez")
        (fun x -> x) in
      let (_, some_other_ticket, checker) = with_burrow 
        checker
        alice_addr 
        (Ligo.tez_from_literal "3_000_000mutez")
        (fun x -> x) in

       assert_raises 
       (Failure (Ligo.string_of_int error_InvalidPermission))
        (fun () -> with_transaction 
          alice_addr 
          (Ligo.tez_from_literal "3_000_000mutez")
          (fun () -> Checker.deposit_tez checker (Some some_other_ticket) burrow_id)
        )
    );

    ("withdraw_tez - admin ticket holder can withdraw" >::
      fun _ -> 
        (* Create burrow *)
        let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
        let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
        let expected_collateral = Ligo.sub_tez_tez initial_deposit (Ligo.add_tez_tez Constants.creation_deposit withdrawal) in
        let (burrow_id, admin_ticket, checker) = with_burrow 
          initial_checker
          alice_addr 
          initial_deposit
          (fun x -> x) in

        let (_, checker) = with_transaction
          alice_addr
          (Ligo.tez_from_literal "0mutez")
          (fun () -> Checker.withdraw_tez checker admin_ticket withdrawal burrow_id) in
        
        match Ligo.Big_map.find_opt burrow_id checker.burrows with 
        | Some burrow -> assert_bool "Burrow representation has unexpected collateral value" (Ligo.eq_tez_tez (burrow_collateral burrow) expected_collateral)
        | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("withdraw_tez - transaction with value > 0 fails" >::
    fun _ -> 
      let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
      let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
      let (burrow_id, admin_ticket, checker) = with_burrow 
        initial_checker
        alice_addr 
        initial_deposit
        (fun x -> x) in

       assert_raises
       (Failure (Ligo.string_of_int error_UnwantedTezGiven))
        (fun () -> 
          with_transaction
            alice_addr
            (Ligo.tez_from_literal "42mutez")
            (fun () -> Checker.withdraw_tez checker admin_ticket withdrawal burrow_id) 
        )
    );

    ("withdraw_tez - fail if the ticket to another burrow is submitted" >::
    fun _ -> 
      let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
      let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
      let (burrow_id, _, checker) = with_burrow 
        initial_checker
        alice_addr 
        initial_deposit
        (fun x -> x) in
      let (_, some_other_ticket, checker) = with_burrow 
        checker
        alice_addr 
        (Ligo.tez_from_literal "3_000_000mutez")
        (fun x -> x) in

       assert_raises
       (Failure (Ligo.string_of_int error_InvalidPermission))
        (fun () -> 
          with_transaction
            alice_addr
            (Ligo.tez_from_literal "0mutez")
            (fun () -> Checker.withdraw_tez checker some_other_ticket withdrawal burrow_id) 
        )
    );

    ("buy_kit - returns updated uniswap state" >::
      fun _ -> with_transaction
        alice_addr
        (Ligo.tez_from_literal "9_000_000mutez")
        (fun () -> 
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
        )
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
