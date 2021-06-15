open Ctez
open Kit
open Lqt
open Burrow
open OUnit2
open TestCommon
open CheckerTypes
open Fa2Interface
open Error
open Ptr
open LiquidationAuctionTypes

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module PtrMap = Map.Make(struct type t = ptr let compare = compare_ptr end)

let checker_address = !Ligo.Tezos.self_address

let empty_checker =
  initial_checker
    { ctez = Ligo.address_of_string "ctez_addr";
      oracle = Ligo.address_of_string "oracle_addr";
    }

(* The starting checker state should satisfy the invariants to begin with. *)
let _ = Checker.assert_checker_invariants empty_checker

(* Enhance the initial checker state with a populated cfmm in a consistent way. *)
let empty_checker_with_cfmm (cfmm: CfmmTypes.cfmm) =
  let checker_kit = kit_sub cfmm.kit (kit_of_mukit (Ligo.nat_from_literal "1n")) in
  let checker_liquidity = lqt_sub cfmm.lqt (lqt_of_denomination (Ligo.nat_from_literal "1n")) in
  let checker =
    { empty_checker with
      parameters = { empty_checker.parameters with circulating_kit = checker_kit };
      cfmm = cfmm;
      fa2_state =
        let fa2_state = initial_fa2_state in
        let fa2_state = ledger_issue_lqt (fa2_state, !Ligo.Tezos.self_address, checker_liquidity) in
        let fa2_state = ledger_issue_kit (fa2_state, !Ligo.Tezos.self_address, checker_kit) in
        fa2_state;
    } in
  Checker.assert_checker_invariants checker;
  checker

(* Helper for creating new burrows and extracting their ID from the corresponding Ligo Ops *)
let newly_created_burrow (checker: checker) (burrow_no: string) : burrow_id * checker =
  let _ops, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None)) in
  ((!Ligo.Tezos.sender, Ligo.nat_from_literal burrow_no), checker)

let get_balance_of (checker: checker) (addr: Ligo.address) (tok: fa2_token_id): Ligo.nat =
  let ops, _checker = Checker.strict_entrypoint_balance_of (checker, { requests = [{ owner=addr; token_id=tok }]; callback=Ligo.contract_of_address addr}) in
  match ops with
  | [ Transaction (FA2BalanceOfResponseTransactionValue [ { request = _; balance = kit } ], _, _) ] -> kit
  | _ -> failwith ("Unexpected fa2 response, got: " ^ show_operation_list ops)

let suite =
  "Checker tests" >::: [
    ("initial touch (noop)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker1 = empty_checker in
       let ops, checker2 = Checker.touch_with_index checker1 (Ligo.tez_from_literal "0mutez") in

       assert_operation_list_equal ~expected:[] ~real:ops;
       assert_equal checker1 checker2; (* NOTE: we really want them to be identical here, hence the '='. *)
       ()
    );

    ("create_burrow - updates checker storage" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");

       let burrow_id, checker = newly_created_burrow empty_checker "0n" in

       assert_bool
         "No matching burrow found after calling create_burrow"
         (Option.is_some (Ligo.Big_map.find_opt burrow_id checker.burrows));
       assert_bool
         "The burrow existed before calling create_burrow"
         (Option.is_none (Ligo.Big_map.find_opt burrow_id empty_checker.burrows))
    );

    ("create_burrow - collateral in burrow representation does not include creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Constants.creation_deposit;

       let burrow_id, checker = newly_created_burrow empty_checker "0n" in

       let expected_collateral = Ligo.tez_from_literal "0mutez" in
       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_tez_equal ~expected:expected_collateral ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("create_burrow - fails when transaction amount is one mutez below creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Ligo.sub_tez_tez Constants.creation_deposit (Ligo.tez_from_literal "1mutez") in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;

       assert_raises
         (Failure (Ligo.string_of_int error_InsufficientFunds))
         (fun () -> Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)))
    );

    ("create_burrow - passes when transaction amount is exactly the creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Constants.creation_deposit;
       let burrow_id, checker = newly_created_burrow empty_checker "0n" in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow ->
         assert_tez_equal ~expected:(Ligo.tez_from_literal "0mutez") ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("deposit_tez - owner can deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let expected_collateral = Ligo.add_tez_tez deposit (Ligo.sub_tez_tez  initial_deposit Constants.creation_deposit) in

       (* Create the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let (_, burrow_no) as burrow_id, checker = newly_created_burrow empty_checker "0n" in
       (* Make a deposit *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:deposit;
       let _, checker = Checker.entrypoint_deposit_tez (checker, burrow_no) in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_tez_equal ~expected:expected_collateral ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("deposit_tez - non-owner cannot deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");

       let _, checker = newly_created_burrow empty_checker "0n" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_NonExistentBurrow))
         (fun () -> Checker.entrypoint_deposit_tez (checker, Ligo.nat_from_literal "0n"))
    );

    ("withdraw_tez - owner can withdraw" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
       let expected_collateral = Ligo.sub_tez_tez initial_deposit (Ligo.add_tez_tez Constants.creation_deposit withdrawal) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       let burrow_id, checker = newly_created_burrow empty_checker "0n" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_withdraw_tez (checker, (withdrawal, Ligo.nat_from_literal "0n")) in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_tez_equal ~expected:expected_collateral ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("withdraw_tez - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let _, checker = newly_created_burrow empty_checker "0n" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "42mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> Checker.entrypoint_withdraw_tez (checker, (withdrawal, Ligo.nat_from_literal "0n")))
    );

    ("withdraw_tez - non-owner cannot withdraw" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let _, checker = newly_created_burrow empty_checker "0n" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_NonExistentBurrow))
         (fun () -> Checker.entrypoint_withdraw_tez (checker, (withdrawal, Ligo.nat_from_literal "0n")))
    );

    ("calculate_touch_reward - expected result for last_touched 2s ago" >::
     fun _ ->
       (* The division in this case should return a remainder < 1/2 *)
       Ligo.Tezos.reset ();
       let time_delta = 2 in
       (* remainder: 12000 / 36000 *)
       let expected_reward = Ligo.int_from_literal "3333" in
       let last_touched = Ligo.timestamp_from_seconds_literal 0 in
       Ligo.Tezos.new_transaction ~seconds_passed:time_delta ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let actual_reward = kit_to_mukit_int (Checker.calculate_touch_reward last_touched) in

       assert_int_equal ~expected:expected_reward ~real:actual_reward;
    );

    ("calculate_touch_reward - expected result for last_touched 3s ago" >::
     fun _ ->
       (* The division in this case should produce no remainder *)
       Ligo.Tezos.reset ();
       let time_delta = 3 in
       (* remainder: 0 *)
       let expected_reward = Ligo.int_from_literal "5000" in
       let last_touched = Ligo.timestamp_from_seconds_literal 0 in
       Ligo.Tezos.new_transaction ~seconds_passed:time_delta ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let actual_reward = kit_to_mukit_int (Checker.calculate_touch_reward last_touched) in

       assert_int_equal ~expected:expected_reward ~real:actual_reward;
    );

    ("calculate_touch_reward - expected result for last_touched 4s ago" >::
     fun _ ->
       (* The division in this case should return a remainder > 1/2 *)
       Ligo.Tezos.reset ();
       let time_delta = 4 in
       (* remainder: 24000 / 36000 *)
       let expected_reward = Ligo.int_from_literal "6666" in
       let last_touched = Ligo.timestamp_from_seconds_literal 0 in
       Ligo.Tezos.new_transaction ~seconds_passed:time_delta ~blocks_passed:2 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let actual_reward = kit_to_mukit_int (Checker.calculate_touch_reward last_touched) in

       assert_int_equal ~expected:expected_reward ~real:actual_reward;

    );

    ("burn_kit - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let _, checker = newly_created_burrow empty_checker "0n" in
       let some_kit = Kit.kit_of_mukit (Ligo.nat_from_literal "1n") in

       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
            Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", some_kit))
         )
    );

    ("burn_kit - owner can burn" >::
     fun _ ->
       Ligo.Tezos.reset ();

       let sender = alice_addr in

       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let _, checker = newly_created_burrow empty_checker "0n" in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "4_285_714n"))
           ) in

       (* There should be no operations emitted. *)
       assert_operation_list_equal ~expected:[] ~real:ops;

       (* The owner should be able to burn it back. *)
       let kit_token = kit_of_mukit (Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender)) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", kit_token)) in

       ()
    );

    ("burn_kit - non-owner cannot burn" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let _, checker = newly_created_burrow empty_checker "0n" in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "4_285_714n"))
           ) in

       (* There should be no operations emitted. *)
       assert_operation_list_equal ~expected:[] ~real:ops;

       (* Have the wrong person try to burn it back; this should fail. *)
       assert_raises
         (Failure (Ligo.string_of_int error_NonExistentBurrow))
         (fun () ->
            let kit_token = kit_of_mukit (Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, bob_addr)) in
            Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
            Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", kit_token))
         );

       ()
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_respects_min_kit_expected"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctez_amount, min_kit_expected, deadline) ->

      let sender = alice_addr in
      let checker = empty_checker_with_cfmm cfmm in

      let senders_old_mukit = Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender) in (* before *)

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, checker = Checker.entrypoint_buy_kit (checker, (ctez_amount, min_kit_expected, deadline)) in

      let senders_new_mukit = Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender) in (* after *)

      begin match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:sender ~real:transfer.address_from;
            assert_address_equal ~expected:checker_address ~real:transfer.address_to;
            assert_nat_equal ~expected:(ctez_to_muctez_nat ctez_amount) ~real:transfer.value;
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      end;

      Ligo.geq_nat_nat
        senders_new_mukit
        (Ligo.add_nat_nat senders_old_mukit (kit_to_mukit_nat min_kit_expected))
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_preserves_kit"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctez_amount, min_kit_expected, deadline) ->

      let checker = empty_checker_with_cfmm cfmm in
      let sender = alice_addr in

      let checker_cfmm_old_mukit = kit_to_mukit_nat checker.cfmm.kit in
      let senders_old_mukit = Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender) in (* before *)

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, checker = Checker.entrypoint_buy_kit (checker, (ctez_amount, min_kit_expected, deadline)) in

      let checker_cfmm_new_mukit = kit_to_mukit_nat checker.cfmm.kit in
      let senders_new_mukit = Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender) in (* after *)

      begin match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:sender ~real:transfer.address_from;
            assert_address_equal ~expected:checker_address ~real:transfer.address_to;
            assert_nat_equal ~expected:(ctez_to_muctez_nat ctez_amount) ~real:transfer.value;
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      end;

      Ligo.eq_nat_nat
        (Ligo.add_nat_nat checker_cfmm_old_mukit senders_old_mukit)
        (Ligo.add_nat_nat checker_cfmm_new_mukit senders_new_mukit)
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_preserves_tez"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctez_amount, min_kit_expected, deadline) ->
      let checker = empty_checker_with_cfmm cfmm in
      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
      let _, new_checker = Checker.entrypoint_buy_kit (checker, (ctez_amount, min_kit_expected, deadline)) in
      ctez_add checker.cfmm.ctez ctez_amount = new_checker.cfmm.ctez
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_sell_kit_respects_min_tez_expected"
        ~count:property_test_count
        make_inputs_for_sell_kit_to_succeed
      @@ fun (cfmm, kit_amount, min_ctez_expected, deadline) ->
      let sender = alice_addr in
      let checker =
        let checker = empty_checker_with_cfmm cfmm in
        { checker with
          parameters =
            { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_amount };
          fa2_state = ledger_issue_kit (checker.fa2_state, sender, kit_amount);
        } in
      Checker.assert_checker_invariants checker;

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, _ = Checker.entrypoint_sell_kit (checker, (kit_amount, min_ctez_expected, deadline)) in
      let bought_muctez = match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:checker_address ~real:transfer.address_from;
            assert_address_equal ~expected:sender ~real:transfer.address_to;
            transfer.value
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      in
      ctez_of_muctez bought_muctez >= min_ctez_expected
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_sell_kit_preserves_kit"
        ~count:property_test_count
        make_inputs_for_sell_kit_to_succeed
      @@ fun (cfmm, kit_amount, min_ctez_expected, deadline) ->
      let sender = alice_addr in
      let checker =
        let checker = empty_checker_with_cfmm cfmm in
        { checker with
          parameters =
            { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_amount };
          fa2_state = ledger_issue_kit (checker.fa2_state, sender, kit_amount);
        } in
      Checker.assert_checker_invariants checker;

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let _, new_checker = Checker.entrypoint_sell_kit (checker, (kit_amount, min_ctez_expected, deadline)) in
      kit_add checker.cfmm.kit kit_amount = new_checker.cfmm.kit
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_sell_kit_preserves_tez"
        ~count:property_test_count
        make_inputs_for_sell_kit_to_succeed
      @@ fun (cfmm, kit_amount, min_ctez_expected, deadline) ->
      let sender = alice_addr in
      let checker =
        let checker = empty_checker_with_cfmm cfmm in
        { checker with
          parameters =
            { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_amount };
          fa2_state = ledger_issue_kit (checker.fa2_state, sender, kit_amount);
        } in
      Checker.assert_checker_invariants checker;

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, new_checker = Checker.entrypoint_sell_kit (checker, (kit_amount, min_ctez_expected, deadline)) in

      let bought_muctez = match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:checker_address ~real:transfer.address_from;
            assert_address_equal ~expected:sender ~real:transfer.address_to;
            transfer.value
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      in
      ctez_add new_checker.cfmm.ctez (ctez_of_muctez bought_muctez) = checker.cfmm.ctez
    );

    ("set_burrow_delegate - transaction with value > 0 fails" >::
     fun _ ->
       (* Create a burrow *)
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let _, checker = newly_created_burrow empty_checker "0n" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Checker.entrypoint_set_burrow_delegate (checker, (Ligo.nat_from_literal "0n", None))
         )
    );

    (
      let cfmm_kit = Ligo.nat_from_literal ("1_000n") in
      let cfmm_ctez = ctez_of_muctez (Ligo.nat_from_literal ("1_000n")) in
      (* The maximum amount of kit that you can buy with a finite amount of tez is
       * (1 - fee) * cfmm.kit - 1
      *)
      let max_buyable_kit = 997 in
      let arb_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(1 -- max_buyable_kit) in
      let arb_tez = TestArbitrary.arb_small_tez in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"buy_kit - returns geq min_kit_expected kit for transactions with sufficient tez"
        ~count:property_test_count
        (QCheck.pair arb_kit arb_tez)
      @@ fun (min_expected_kit, additional_tez) ->

      Ligo.Tezos.reset();
      let sender = alice_addr in

      (* Populate cfmm with initial liquidity *)
      let open Ratio in
      let checker =
        empty_checker_with_cfmm
          { empty_checker.cfmm with
            ctez = cfmm_ctez;
            kit = kit_of_mukit cfmm_kit;
          } in

      (* Calculate minimum tez to get the min_expected kit given the state of the cfmm defined above*)
      let ratio_minimum_tez = div_ratio
          (ratio_of_nat cfmm_kit)
          (
            sub_ratio
              (div_ratio (ratio_of_nat (Ligo.nat_from_literal "998n")) (ratio_of_nat (kit_to_mukit_nat min_expected_kit)))
              (ratio_of_nat (Ligo.nat_from_literal "1n"))
          ) in
      let minimum_tez = Ligo.mul_nat_tez (Ligo.abs (Common.cdiv_int_int ratio_minimum_tez.num ratio_minimum_tez.den)) (Ligo.tez_from_literal "1mutez") in
      (* Adjust transaction by a random amount of extra tez *)
      let tez_provided = Ligo.add_tez_tez minimum_tez additional_tez in

      let senders_old_mukit = Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender) in (* before *)

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, checker = Checker.entrypoint_buy_kit (checker, (ctez_from_tez tez_provided, min_expected_kit, Ligo.timestamp_from_seconds_literal 1)) in

      begin match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:sender ~real:transfer.address_from;
            assert_address_equal ~expected:checker_address ~real:transfer.address_to;
            assert_nat_equal ~expected:(Ligo.abs (Common.tez_to_mutez tez_provided)) ~real:transfer.value;
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      end;

      let senders_new_mukit = Fa2Interface.get_fa2_ledger_value checker.fa2_state.ledger (Fa2Interface.kit_token_id, sender) in (* after *)

      Ligo.geq_nat_nat
        senders_new_mukit
        (Ligo.add_nat_nat senders_old_mukit (kit_to_mukit_nat min_expected_kit))
        (* FIXME: This test only rarely evaluates the 'eq' part of 'geq'. Reducing the range of possible `additional_tez` or increasing the
         * number of QCheck samples may improve this.
        *)
    );

    ("buy_kit - returns expected kit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Populate the cfmm with some liquidity *)
       let checker =
         empty_checker_with_cfmm
           { empty_checker.cfmm with
             ctez = ctez_of_muctez (Ligo.nat_from_literal "2n");
             kit = kit_of_mukit (Ligo.nat_from_literal "2n");
           } in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.entrypoint_buy_kit (checker, (ctez_of_muctez (Ligo.nat_from_literal "1_000_000n"), kit_of_mukit (Ligo.nat_from_literal "1n"), Ligo.timestamp_from_seconds_literal 1)) in

       let kit = get_balance_of checker alice_addr kit_token_id in
       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:kit;
       ()
    );

    ("sell_kit - returns expected tez" >::
     fun _ ->
       Ligo.Tezos.reset ();

       let kit_to_sell = kit_of_mukit (Ligo.nat_from_literal "1_000_000n") in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in

       let checker =
         let checker =
           empty_checker_with_cfmm
             { empty_checker.cfmm with
               ctez = ctez_of_muctez (Ligo.nat_from_literal "2n");
               kit = kit_of_mukit (Ligo.nat_from_literal "2n");
               lqt = lqt_of_denomination (Ligo.nat_from_literal "1n");
             } in
         { checker with
           parameters =
             { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_to_sell };
           fa2_state = ledger_issue_kit (checker.fa2_state, alice_addr, kit_to_sell);
         } in
       Checker.assert_checker_invariants checker;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_sell_kit (checker, (kit_to_sell, min_ctez_expected, Ligo.timestamp_from_seconds_literal 1)) in
       let muctez = match ops with
         | [Transaction (FA12TransferTransactionValue transfer, _, _)] -> transfer.value
         | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:muctez
    );

    ("sell_kit - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let kit_to_sell = kit_of_mukit (Ligo.nat_from_literal "1n") in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Checker.entrypoint_sell_kit (empty_checker, (kit_to_sell, min_ctez_expected, Ligo.timestamp_from_seconds_literal 1))
         )
    );

    ("remove_liquidity - returns expected kit and tez" >::
     fun _ ->
       Ligo.Tezos.reset ();

       let min_kit_expected = kit_of_mukit (Ligo.nat_from_literal "1n") in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in
       let my_liquidity_tokens = lqt_of_denomination (Ligo.nat_from_literal "1n") in
       let sender = alice_addr in

       (* Populate the cfmm with some liquidity (carefully crafted) *)
       let checker =
         { empty_checker with
           parameters = { empty_checker.parameters with circulating_kit = kit_of_mukit (Ligo.nat_from_literal "1n")};
           cfmm =
             { empty_checker.cfmm with
               ctez = ctez_of_muctez (Ligo.nat_from_literal "2n");
               kit = kit_of_mukit (Ligo.nat_from_literal "2n");
               lqt = lqt_of_denomination (Ligo.nat_from_literal "2n");
             };
           fa2_state =
             let fa2_state = initial_fa2_state in
             let fa2_state = ledger_issue_lqt (fa2_state, sender, my_liquidity_tokens) in
             let fa2_state = ledger_issue_kit (fa2_state, !Ligo.Tezos.self_address, kit_of_mukit (Ligo.nat_from_literal "1n")) in
             fa2_state;
         } in
       Checker.assert_checker_invariants checker;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, checker = Checker.entrypoint_remove_liquidity (checker, (my_liquidity_tokens, min_ctez_expected, min_kit_expected, Ligo.timestamp_from_seconds_literal 1)) in
       let ctez = match ops with
         | [ Transaction (FA12TransferTransactionValue transfer, _, _); ] ->
           begin
             assert_address_equal ~expected:checker_address ~real:transfer.address_from;
             transfer.value
           end
         | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _); Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in
       let kit = get_balance_of checker sender kit_token_id in

       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:kit;
       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:ctez;
       ()
    );

    ("remove_liquidity - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let min_kit_expected = kit_of_mukit (Ligo.nat_from_literal "1n") in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in
       let my_liquidity_tokens = lqt_of_denomination (Ligo.nat_from_literal "1n") in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Checker.entrypoint_remove_liquidity (empty_checker, (my_liquidity_tokens, min_ctez_expected, min_kit_expected, Ligo.timestamp_from_seconds_literal 1))
         )
    );

    (* ************************************************************************* *)
    (**                               FA2                                        *)
    (* ************************************************************************* *)
    ("strict_entrypoint_transfer (FA2) - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> Checker.strict_entrypoint_transfer (empty_checker, []))
    );

    ("strict_entrypoint_balance_of (FA2) - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");

       let fa2_balance_of_param =
         { requests = [];
           callback = Ligo.contract_of_address (Ligo.address_of_string "test address");
         } in
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> Checker.strict_entrypoint_balance_of (empty_checker, fa2_balance_of_param))
    );

    ("entrypoint_update_operators (FA2) - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> Checker.entrypoint_update_operators (empty_checker, []))
    );

    ("can complete a liquidation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in

       (* mint some kit to convert to liquidity *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "200_000_000mutez");
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None)) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _lqt_minted_ret_kit_ops, checker =
         Checker.entrypoint_add_liquidity
           ( checker
           , ( ctez_of_muctez (Ligo.nat_from_literal "1_000_000n")
             , kit_one
             , lqt_of_denomination (Ligo.nat_from_literal "1n")
             , Ligo.timestamp_from_seconds_literal 1
             )
           ) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = Ligo.tez_from_literal "12_345_678mutez" in
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let (ops, checker0) = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None)) in
         (* created burrow should be deposited (incl. the creation deposit) *)
         let () = match ops with
           | [ CreateContract (_, _, sent_tez, _) ; ] -> assert_tez_equal ~expected:tez ~real:sent_tez
           | _ -> assert_failure ("Expected [CreateContract (_, _, _, _)] but got " ^ show_operation_list ops) in

         let burrow_addr =
           burrow_address
             (Option.get (Ligo.Big_map.find_opt (bob_addr, Ligo.nat_from_literal "0n") checker0.burrows)) in
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let (ops, checker1) = Checker.entrypoint_deactivate_burrow (checker0, (Ligo.nat_from_literal "0n", alice_addr)) in
         assert_operation_list_equal
           ~expected:[
             LigoOp.Tezos.tez_address_transaction
               (tez, alice_addr)
               (Ligo.tez_from_literal "0mutez")
               (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_addr))
           ]
           ~real:ops;
         (* deactivation/activation = identity (if conditions are met ofc). *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let _ops, checker2 = Checker.entrypoint_activate_burrow (checker1, Ligo.nat_from_literal "0n") in
         (* FIXME: cfmm contains a ratio, which cannot be compared for equality using (=). So, the next line can give false positives. *)
         assert_equal checker0 checker2;
         () in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let (_, checker) = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None)) in
       let burrow_id = (bob_addr, Ligo.nat_from_literal "0n") in
       let burrow_addr =
         burrow_address
           (Option.get (Ligo.Big_map.find_opt (bob_addr, Ligo.nat_from_literal "0n") checker.burrows)) in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "4_285_714n"))
           ) in

       let kit = get_balance_of checker bob_addr kit_token_id in
       assert_nat_equal ~expected:(Ligo.nat_from_literal "4_285_714n") ~real:kit;

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
            Checker.entrypoint_mint_kit
              ( checker
              , (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "1n"))
              )
         );

       (* Over time the burrows with outstanding kit should be overburrowed
          	* (NOTE: even if the index stays where it was before, but that would
          	* take more time I guess). *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let _ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_001mutez") in

       let ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in
       assert_operation_list_equal ~expected:[] ~real:ops;

       assert_bool
         "if the index goes up, then burrows should become overburrowed"
         (burrow_is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* If enough time passes and the index remains up, then the burrow is even liquidatable. *)
       Ligo.Tezos.new_transaction ~seconds_passed:(211*60) ~blocks_passed:211 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let kit_before_reward = get_balance_of checker bob_addr kit_token_id in
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in
       let kit_after_reward = get_balance_of checker bob_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       let ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in
       assert_operation_list_equal ~expected:[] ~real:ops;

       assert_int_equal
         ~expected:(Ligo.int_from_literal "202_000_000") (* wow, high reward, many blocks have passed. *)
         ~real:touch_reward;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in

       assert_operation_list_equal
         ~expected:[
           LigoOp.Tezos.tez_address_transaction
             (Ligo.tez_from_literal "1_009_000mutez", alice_addr)
             (Ligo.tez_from_literal "0mutez")
             (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_addr))
         ]
         ~real:ops;

       let slice =
         (Ligo.Big_map.find_opt burrow_id checker.liquidation_auctions.burrow_slices)
         |> Option.get
         |> fun i -> i.youngest_slice in

       (* We shouldn't be able to cancel the liquidation of this slice if the
        * prices don't change, even if it's not in an auction yet. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwarrantedCancellation))
         (fun () -> Checker.entrypoint_cancel_liquidation_slice (checker, slice));

       (* Trying to cancel a liquidation using an invalid pointer should fail. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidLeafPtr))
         (fun () ->
            let undefined_slice = LiquidationAuctionPrimitiveTypes.LeafPtr (ptr_next checker.liquidation_auctions.avl_storage.last_ptr) in
            Checker.entrypoint_cancel_liquidation_slice (checker, undefined_slice)
         );

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_NoOpenAuction))
         (fun () ->
            Checker.entrypoint_liquidation_auction_place_bid
              ( checker
              , (kit_of_mukit (Ligo.nat_from_literal "1_000n"))
              )
         );

       let kit_before_reward = get_balance_of checker bob_addr kit_token_id in
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in
       let kit_after_reward = get_balance_of checker bob_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_int_equal
         ~expected:(Ligo.int_from_literal "500_000")
         ~real:touch_reward;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let kit_before_reward = get_balance_of checker alice_addr kit_token_id in
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in
       let kit_after_reward = get_balance_of checker alice_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       let (ops, checker) =
         Checker.entrypoint_liquidation_auction_place_bid
           ( checker
           , (kit_of_mukit (Ligo.nat_from_literal "4_200_000n"))
           ) in

       let auction_id =
         match checker.liquidation_auctions.current_auction with
         | None -> assert_failure "entrypoint_liquidation_auction_place_bid should have succeeded"
         | Some current_auction -> current_auction.contents in

       assert_operation_list_equal ~expected:[] ~real:ops;

       assert_int_equal
         ~expected:(Ligo.int_from_literal "500_000")
         ~real:touch_reward;

       Ligo.Tezos.new_transaction ~seconds_passed:(30*60) ~blocks_passed:30 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let kit_before_reward = get_balance_of checker alice_addr kit_token_id in
       let ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_200_000mutez") in
       let kit_after_reward = get_balance_of checker alice_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_int_equal
         ~expected:(Ligo.int_from_literal "21_000_000")
         ~real:touch_reward;

       (* Check that all the requests for burrows to send tez come _before_ the
        * request to the oracle to update the index. *)
       begin match ops with
         | [
           Transaction (TezTransactionValue _, _, _);
           Transaction (NatContractTransactionValue _, _, _);
         ] -> ()
         | _ -> assert_failure ("Unexpected operations/operation order: " ^ show_operation_list ops)
       end;

       (* We don't need to touch the slice on this test case since
        * Checker.entrypoint_touch_with_index already touches the oldest 5
        * slices. *)
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidLeafPtr))
         (fun () -> Checker.entrypoint_touch_liquidation_slices (checker, [slice]));

       assert_bool "burrow should have no liquidation slices"
         (Ligo.Big_map.find_opt burrow_id checker.liquidation_auctions.burrow_slices= None);

       let result = Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows) in
       assert_tez_equal
         ~expected:(Ligo.tez_from_literal "0mutez")
         ~real:(burrow_collateral_at_auction result);

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) = Checker.entrypoint_liquidation_auction_claim_win (checker, auction_id) in

       assert_operation_list_equal
         ~expected:[LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "3_156_451mutez") (Option.get (LigoOp.Tezos.get_contract_opt alice_addr))]
         ~real:ops;

       (* This should fail; shouldn't be able to claim the win twice. *)
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidAvlPtr))
         (fun () -> Checker.entrypoint_liquidation_auction_claim_win (checker, auction_id));

       ()
    );

    ("entrypoint_mark_for_liquidation - should not create empty slices" >::
     fun _ ->
       (* Setup. *)
       Ligo.Tezos.reset ();
       let sender = alice_addr in
       let checker = empty_checker in

       (* Create a burrow with a very little tez in it. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "2_001_001mutez");
       let (_, burrow_no) as burrow_id, checker = newly_created_burrow checker "0n" in

       (* CALCULATIONS
          ~~~~~~~~~~~~
          Tez in the burrow is (1_001_001mutez + 1tez) so the reward is
          (1tez + 1_001mutez = 1_001_001). This means that
          - The slice we WOULD send to auctions is empty.
          - The burrow remains is empty so the next liquidation WOULD create another empty slice to auctions.
       *)

       (* Mint as much kit as possible. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) = Checker.entrypoint_mint_kit (checker, (burrow_no, kit_of_mukit (Ligo.nat_from_literal "476_667n"))) in

       (* Let some time pass. Over time the burrows with outstanding kit should
          	* become overburrowed, and eventually liquidatable. Note that this
          	* could be because of the index, but also it can happen because of the
          	* fees alone if the index remains the same. *)
       let blocks_passed = 211 in (* NOTE: I am a little surprised/worried about this being again 211... *)
       Ligo.Tezos.new_transaction ~seconds_passed:(60*blocks_passed) ~blocks_passed:blocks_passed ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_105_283mutez") in (* sup *)
       let _ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in

       (* Ensure that the burrow is liquidatable. *)
       begin match Ligo.Big_map.find_opt burrow_id checker.burrows with
         | None -> assert_failure "bug"
         | Some burrow -> assert_bool "burrow needs to be liquidatable for the test to be potent." (Burrow.burrow_is_liquidatable checker.parameters burrow);
       end;

       (* Let's mark the burrow for liquidation now (first pass: leaves it empty but active). *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in
       Checker.assert_checker_invariants checker; (* Ensures no empty slices in the queue. *)

       (* Let's mark the burrow for liquidation now (second pass: deactivates it). *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in
       Checker.assert_checker_invariants checker; (* Ensures no empty slices in the queue. *)

       ()
    );

    ("deposit_tez - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to deposit some tez to the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_deposit_tez (checker, Ligo.nat_from_literal "0n"))
    );

    ("entrypoint_withdraw_tez - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Ligo.add_tez_tez Constants.creation_deposit Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to withdraw some tez from the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_withdraw_tez (checker, (Constants.creation_deposit, Ligo.nat_from_literal "0n")))
    );

    ("entrypoint_mint_kit - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       let amount = Ligo.add_tez_tez Constants.creation_deposit Constants.creation_deposit in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to mint some kit out of the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "1n"))))
    );

    ("entrypoint_burn_kit - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Ligo.add_tez_tez Constants.creation_deposit Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Mint some kit out of the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "1n"))) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to burn some kit into the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", kit_of_mukit (Ligo.nat_from_literal "1n"))))
    );

    ("entrypoint_activate_burrow - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Deactivate the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.entrypoint_deactivate_burrow (checker, (Ligo.nat_from_literal "0n", !Ligo.Tezos.sender)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to activate the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_activate_burrow (checker, Ligo.nat_from_literal "0n"))
    );

    ("entrypoint_deactivate_burrow - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to deactivate the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_deactivate_burrow (checker, (Ligo.nat_from_literal "0n", !Ligo.Tezos.sender)))
    );

    ("entrypoint_mark_for_liquidation - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to mark the untouched burrow for liquidation *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_mark_for_liquidation (checker, burrow_id))
    );

    (* TODO: Add test "entrypoint_cancel_liquidation_slice - fails on untouched burrows" *)

    ("entrypoint_set_burrow_delegate - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to set the delegate of the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.entrypoint_set_burrow_delegate (checker, (Ligo.nat_from_literal "0n", None)))
    );

    ("view_burrow_max_mintable_kit - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to view the max mintable kit from the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.view_burrow_max_mintable_kit (burrow_id, checker))
    );

    ("view_is_burrow_overburrowed - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to view whether the untouched burrow is overburrowed *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.view_is_burrow_overburrowed (burrow_id, checker))
    );

    ("view_is_burrow_liquidatable - fails on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_000mutez") in
       (* Try to view whether the untouched burrow is liquidatable *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () -> Checker.view_is_burrow_liquidatable (burrow_id, checker))
    );
  ]
