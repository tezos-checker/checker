open Ctez
open Kit
open Burrow
open OUnit2
open TestCommon
open CheckerTypes
open Tickets
open Error
open Ptr

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module PtrMap = Map.Make(struct type t = ptr let compare = compare_ptr end)

type operation_list = LigoOp.operation list
[@@deriving show]

(* Helper for creating new burrows and extracting their ID from the corresponding Ligo Ops *)
let newly_created_burrow checker =
  let ops, checker = Checker.entrypoint_create_burrow (checker, None) in
  match ops with
  | [ CreateContract _ ;
      Transaction (AddressTransactionValue burrow_id, _, _) ;
    ] -> (burrow_id, checker)
  | _ -> failwith ("Expected [CreateContract _; Transaction (AddressTransactionValue _, _, _)] but got " ^ show_operation_list ops)

let suite =
  "Checker tests" >::: [
    ("initial touch (noop)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker1 = initial_checker in
       let ops, checker2 = Checker.touch_with_index checker1 (Ligo.tez_from_literal "0mutez") in

       assert_equal [] ops ~printer:show_operation_list;
       assert_equal checker1 checker2; (* NOTE: we really want them to be identical here, hence the '='. *)
       ()
    );

    ("create_burrow - updates checker storage" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");

       let burrow_id, checker = newly_created_burrow initial_checker in

       assert_bool
         "No matching burrow found after calling create_burrow"
         (Option.is_some (Ligo.Big_map.find_opt burrow_id checker.burrows));
       assert_bool
         "The burrow existed before calling create_burrow"
         (Option.is_none (Ligo.Big_map.find_opt burrow_id initial_checker.burrows))
    );

    ("create_burrow - collatoral in burrow representation does not include creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Constants.creation_deposit;

       let burrow_id, checker = newly_created_burrow initial_checker in

       let expected_collateral = Ligo.tez_from_literal "0mutez" in
       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_equal (burrow_collateral burrow) expected_collateral ~printer:Ligo.string_of_tez
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("create_burrow - fails when transaction amount is one mutez below creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Ligo.sub_tez_tez Constants.creation_deposit (Ligo.tez_from_literal "1mutez") in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:amount;

       assert_raises
         (Failure (Ligo.string_of_int error_InsufficientFunds))
         (fun () -> Checker.entrypoint_create_burrow (initial_checker, None))
    );

    ("create_burrow - passes when transaction amount is exactly the creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Constants.creation_deposit;
       let burrow_id, checker = newly_created_burrow initial_checker in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow ->
         assert_equal (burrow_collateral burrow) (Ligo.tez_from_literal "0mutez") ~printer:Ligo.string_of_tez
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
       let burrow_id, checker = newly_created_burrow initial_checker in
       (* Make a deposit *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:deposit;
       let _, checker = Checker.entrypoint_deposit_tez (checker, Checker.deticketify_deposit_tez burrow_id) in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_equal (burrow_collateral burrow) expected_collateral ~printer:Ligo.string_of_tez
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("deposit_tez - non-owner cannot deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");

       let burrow_id, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_AuthenticationError))
         (fun () -> Checker.entrypoint_deposit_tez (checker, Checker.deticketify_deposit_tez burrow_id))
    );

    ("withdraw_tez - owner can withdraw" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
       let expected_collateral = Ligo.sub_tez_tez initial_deposit (Ligo.add_tez_tez Constants.creation_deposit withdrawal) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "3_000_000mutez");
       let burrow_id, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_withdraw_tez (checker, Checker.deticketify_withdraw_tez (withdrawal, burrow_id)) in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_equal (burrow_collateral burrow) expected_collateral ~printer:Ligo.string_of_tez
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("withdraw_tez - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let burrow_id, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "42mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> Checker.entrypoint_withdraw_tez (checker, Checker.deticketify_withdraw_tez (withdrawal, burrow_id)))
    );

    ("withdraw_tez - non-owner cannot withdraw" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = Ligo.tez_from_literal "3_000_000mutez" in
       let withdrawal = Ligo.tez_from_literal "1_000_000mutez" in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:initial_deposit;
       let burrow_id, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_AuthenticationError))
         (fun () -> Checker.entrypoint_withdraw_tez (checker, Checker.deticketify_withdraw_tez (withdrawal, burrow_id)))
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

       assert_equal expected_reward actual_reward ~printer:Ligo.string_of_int;
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

       assert_equal expected_reward actual_reward ~printer:Ligo.string_of_int;
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

       assert_equal expected_reward actual_reward ~printer:Ligo.string_of_int;

    );

    ("burn_kit - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1_000_000mutez");
       let burrow_id, checker = newly_created_burrow initial_checker in
       let some_kit = Tickets.kit_issue (Kit.kit_of_mukit (Ligo.nat_from_literal "1n")) in

       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
            Checker.entrypoint_burn_kit (checker, Checker.deticketify_burn_kit (burrow_id, some_kit))
         )
    );

    ("burn_kit - owner can burn" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let burrow_id, checker = newly_created_burrow initial_checker in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , Checker.deticketify_mint_kit
               ( burrow_id
               , kit_of_mukit (Ligo.nat_from_literal "4_285_714n")
               )
           ) in
       let kit_token = match ops with
         | [Transaction (KitTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       (* Burn it back *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_burn_kit (checker, Checker.deticketify_burn_kit (burrow_id, kit_token)) in
       ()
    );

    ("burn_kit - non-owner cannot burn" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let burrow_id, checker = newly_created_burrow initial_checker in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , Checker.deticketify_mint_kit
               ( burrow_id
               , kit_of_mukit (Ligo.nat_from_literal "4_285_714n")
               )
           ) in
       let kit_token = match ops with
         | [Transaction (KitTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       (* Have the wrong person try to burn it back *)
       assert_raises
         (Failure (Ligo.string_of_int error_AuthenticationError))
         (fun () ->
            Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
            Checker.entrypoint_burn_kit (checker, Checker.deticketify_burn_kit (burrow_id, kit_token))
         )
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_respects_min_kit_expected"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctez_amount, min_kit_expected, deadline) ->
      let checker = { initial_checker with cfmm = cfmm } in
      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, _ = Checker.entrypoint_buy_kit (checker, (ctez_amount, min_kit_expected, deadline)) in
      let bought_kit = match ops with
        | [ _; Transaction (KitTransactionValue ticket, _, _) ] -> snd (snd (fst (Ligo.Tezos.read_ticket ticket)))
        | _ -> failwith ("Unexpected transactions, got " ^ show_operation_list ops)
      in
      bought_kit >= kit_to_mukit_nat min_kit_expected
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_preserves_kit"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctez_amount, min_kit_expected, deadline) ->
      let checker = { initial_checker with cfmm = cfmm } in
      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, new_checker = Checker.entrypoint_buy_kit (checker, (ctez_amount, min_kit_expected, deadline)) in
      let bought_kit = match ops with
        | [ _; Transaction (KitTransactionValue ticket, _, _) ] -> snd (snd (fst (Ligo.Tezos.read_ticket ticket)))
        | _ -> failwith ("Unexpected transactions, got " ^ show_operation_list ops)
      in
      checker.cfmm.kit = kit_add new_checker.cfmm.kit (kit_of_mukit bought_kit)
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_preserves_tez"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctez_amount, min_kit_expected, deadline) ->
      let checker = { initial_checker with cfmm = cfmm } in
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
      let checker = { initial_checker with cfmm = cfmm } in
      let sender = alice_addr in

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, _ = Checker.entrypoint_sell_kit (checker, Checker.deticketify_sell_kit (Tickets.kit_issue kit_amount, min_ctez_expected, deadline)) in
      let bought_muctez = match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_equal Common.checker_address transfer.address_from ~printer:Ligo.string_of_address;
            assert_equal sender transfer.address_to ~printer:Ligo.string_of_address;
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
      let checker = { initial_checker with cfmm = cfmm } in

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
      let _, new_checker = Checker.entrypoint_sell_kit (checker, Checker.deticketify_sell_kit (Tickets.kit_issue kit_amount, min_ctez_expected, deadline)) in
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
      let checker = { initial_checker with cfmm = cfmm } in
      let sender = alice_addr in

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, new_checker = Checker.entrypoint_sell_kit (checker, Checker.deticketify_sell_kit (Tickets.kit_issue kit_amount, min_ctez_expected, deadline)) in

      let bought_muctez = match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_equal Common.checker_address transfer.address_from ~printer:Ligo.string_of_address;
            assert_equal sender transfer.address_to ~printer:Ligo.string_of_address;
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
       let burrow_id, checker = newly_created_burrow initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Checker.entrypoint_set_burrow_delegate (checker, Checker.deticketify_set_burrow_delegate (burrow_id, None))
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

      (* Populate cfmm with initial liquidity *)
      let open Ratio in
      let checker = {
        initial_checker with
        cfmm={
          initial_checker.cfmm with
          ctez = cfmm_ctez;
          kit = kit_of_mukit cfmm_kit;
        };
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

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, _ = Checker.entrypoint_buy_kit (checker, (ctez_from_tez tez_provided, min_expected_kit, Ligo.timestamp_from_seconds_literal 1)) in
      let (_, (_, kit)), _ = match ops with
        | [ _; Transaction (KitTransactionValue ticket, _, _) ] -> Ligo.Tezos.read_ticket ticket
        | _ -> failwith ("Expected [_; Transaction (KitTransactionValue (ticket, _, _))] but got " ^ show_operation_list ops)
      in
      (* FIXME: This test only rarely evaluates the 'eq' part of 'geq'. Reducing the range of possible `additional_tez` or increasing the
       * number of QCheck samples may improve this.
      *)
      Ligo.geq_nat_nat kit (kit_to_mukit_nat min_expected_kit)
    );

    ("buy_kit - returns expected kit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Populate the cfmm with some liquidity *)
       let checker = {
         initial_checker with
         cfmm={
           initial_checker.cfmm with
           ctez = ctez_of_muctez (Ligo.nat_from_literal "2n");
           kit = kit_of_mukit (Ligo.nat_from_literal "2n");
         };
       } in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_buy_kit (checker, (ctez_of_muctez (Ligo.nat_from_literal "1_000_000n"), kit_of_mukit (Ligo.nat_from_literal "1n"), Ligo.timestamp_from_seconds_literal 1)) in
       let (_, (_, kit)), _ = match ops with
         | [ _; Transaction (KitTransactionValue ticket, _, _) ] -> Ligo.Tezos.read_ticket ticket
         | _ -> failwith ("Expected [_; Transaction (KitTransactionValue (ticket, _, _))] but got " ^ show_operation_list ops)
       in

       assert_equal (Ligo.nat_from_literal "1n") kit ~printer:Ligo.string_of_nat
    );

    ("sell_kit - returns expected tez" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Populate the cfmm with some liquidity *)
       let checker = {
         initial_checker with
         cfmm={
           initial_checker.cfmm with
           ctez = ctez_of_muctez (Ligo.nat_from_literal "2n");
           kit = kit_of_mukit (Ligo.nat_from_literal "2n");
         };
       } in
       let kit_to_sell = Tickets.kit_issue (kit_of_mukit (Ligo.nat_from_literal "1_000_000n")) in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_sell_kit (checker, Checker.deticketify_sell_kit (kit_to_sell, min_ctez_expected, Ligo.timestamp_from_seconds_literal 1)) in
       let muctez = match ops with
         | [Transaction (FA12TransferTransactionValue transfer, _, _)] -> transfer.value
         | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       assert_equal (Ligo.nat_from_literal "1n") muctez ~printer:Ligo.string_of_nat
    );

    ("sell_kit - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let kit_to_sell = Tickets.kit_issue (kit_of_mukit (Ligo.nat_from_literal "1n")) in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Checker.entrypoint_sell_kit (initial_checker, Checker.deticketify_sell_kit (kit_to_sell, min_ctez_expected, Ligo.timestamp_from_seconds_literal 1))
         )
    );

    ("remove_liquidity - returns expected kit and tez" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Populate the cfmm with some liquidity *)
       let checker = {
         initial_checker with
         cfmm={
           initial_checker.cfmm with
           ctez = ctez_of_muctez (Ligo.nat_from_literal "2n");
           kit = kit_of_mukit (Ligo.nat_from_literal "2n");
           lqt = Ligo.nat_from_literal "2n";
         };
       } in
       let min_kit_expected = kit_of_mukit (Ligo.nat_from_literal "1n") in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in
       let my_liquidity_tokens = Tickets.issue_liquidity_tokens (Ligo.nat_from_literal "1n") in
       let sender = alice_addr in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_remove_liquidity (checker, Checker.deticketify_remove_liquidity (my_liquidity_tokens, min_ctez_expected, min_kit_expected, Ligo.timestamp_from_seconds_literal 1)) in
       let kit, ctez = match ops with
         | [
           Transaction (FA12TransferTransactionValue transfer, _, _);
           Transaction (KitTransactionValue ticket, _, _);
           ] ->
           begin
             assert_equal Common.checker_address transfer.address_from ~printer:Ligo.string_of_address;
             assert_equal sender transfer.address_to ~printer:Ligo.string_of_address;
             let (_, (_, kit)), _ = Ligo.Tezos.read_ticket ticket in
             let ctez = transfer.value in
             (kit, ctez)
           end
         | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _); Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in
       assert_equal (Ligo.nat_from_literal "1n") kit ~printer:Ligo.string_of_nat;
       assert_equal (Ligo.nat_from_literal "1n") ctez ~printer:Ligo.string_of_nat;
    );

    ("remove_liquidity - transaction with value > 0 fails" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let min_kit_expected = kit_of_mukit (Ligo.nat_from_literal "1n") in
       let min_ctez_expected = ctez_of_muctez (Ligo.nat_from_literal "1n") in
       let my_liquidity_tokens = Tickets.issue_liquidity_tokens (Ligo.nat_from_literal "1n") in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () ->
            Checker.entrypoint_remove_liquidity (initial_checker, Checker.deticketify_remove_liquidity (my_liquidity_tokens, min_ctez_expected, min_kit_expected, Ligo.timestamp_from_seconds_literal 1))
         )
    );

    ("can complete a liquidation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = initial_checker in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _lqt_minted_ret_kit_ops, checker =
         Checker.entrypoint_add_liquidity
           ( checker
           , Checker.deticketify_add_liquidity
               ( ctez_of_muctez (Ligo.nat_from_literal "1_000_000n")
               , kit_issue kit_one
               , Ligo.nat_from_literal "1n"
               , Ligo.timestamp_from_seconds_literal 1
               )
           ) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = Ligo.tez_from_literal "12_345_678mutez" in
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let (ops, checker0) = Checker.entrypoint_create_burrow (checker, None) in

         (* created burrow should be deposited (incl. the creation deposit) *)
         let burrow_id = match ops with
           | [ CreateContract (_, _, sent_tez, _) ;
               Transaction (AddressTransactionValue burrow_id, _, _) ;
             ] ->
             assert_equal tez sent_tez ~printer:Ligo.string_of_tez;
             burrow_id
           | _ -> assert_failure ("Expected [CreateContract (_, _, _, _); Transaction (AddressTransactionValue _, _, _)] but got " ^ show_operation_list ops)
         in

         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let (ops, checker1) = Checker.entrypoint_deactivate_burrow (checker0, Checker.deticketify_deactivate_burrow burrow_id) in
         assert_equal
           ~printer:show_operation_list
           [LigoOp.Tezos.tez_address_transaction (tez, bob_addr) (Ligo.tez_from_literal "0mutez") (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id))]
           ops;
         (* deactivation/activation = identity (if conditions are met ofc). *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:tez;
         let _ops, checker2 = Checker.entrypoint_activate_burrow (checker1, Checker.deticketify_activate_burrow burrow_id) in
         (* FIXME: cfmm contains a ratio, which cannot be compared for equality using (=). So, the next line can give false positives. *)
         assert_equal checker0 checker2;
         () in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "10_000_000mutez");
       let (ops, checker) = Checker.entrypoint_create_burrow (checker, None) in

       let burrow_id = match ops with
         | [ CreateContract (_, _, _, _) ;
             Transaction (AddressTransactionValue burrow_id, _, _) ;
           ] -> burrow_id
         | _ -> assert_failure ("Expected [CreateContract (_, _, _, _); Transaction (AddressTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , Checker.deticketify_mint_kit
               ( burrow_id
               , kit_of_mukit (Ligo.nat_from_literal "4_285_714n")
               )
           ) in

       let kit_token = match ops with
         | [Transaction (KitTransactionValue ticket, _, _)] -> ticket
         | _ -> assert_failure ("Expected [Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in

       assert_equal (kit_issue (kit_of_mukit (Ligo.nat_from_literal "4_285_714n"))) kit_token ~printer:show_kit_token;

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
              , Checker.deticketify_mint_kit
                  ( burrow_id
                  , kit_of_mukit (Ligo.nat_from_literal "1n")
                  )
              )
         );

       (* Over time the burrows with outstanding kit should be overburrowed
          	* (NOTE: even if the index stays where it was before, but that would
          	* take more time I guess). *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let _ops, checker =
         Checker.touch_with_index checker (Ligo.tez_from_literal "1_000_001mutez") in

       let ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in
       assert_equal [] ops ~printer:show_operation_list;

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

       let ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in
       assert_equal [] ops ~printer:show_operation_list;

       assert_equal
         (kit_issue (kit_of_mukit (Ligo.nat_from_literal "202_000_000n"))) (* wow, high reward, many blocks have passed. *)
         touch_reward
         ~printer:show_kit_token;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in
       assert_equal
         ~printer:show_operation_list
         [LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "1_009_000mutez") (Option.get (LigoOp.Tezos.get_contract_opt alice_addr))]
         ops;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_NoOpenAuction))
         (fun () ->
            Checker.entrypoint_liquidation_auction_place_bid
              ( checker
              , Checker.deticketify_liquidation_auction_place_bid (kit_issue (kit_of_mukit (Ligo.nat_from_literal "1_000n")))
              )
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
         Checker.entrypoint_liquidation_auction_place_bid
           ( checker
           , Checker.deticketify_liquidation_auction_place_bid (kit_issue (kit_of_mukit (Ligo.nat_from_literal "4_200_000n")))
           ) in

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

       (* We don't need to touch the slice on this test case since Checker.entrypoint_touch_with_index
        * already touches the oldest 5 slices. *)
       (*
       let slice =
         (PtrMap.find burrow_id checker.burrows)
         |> burrow_liquidation_slices
         |> Option.get
         |> fun i -> i.youngest in

       let checker =
         Checker.entrypoint_touch_liquidation_slices
           checker
           [slice] in
       *)

       let result = Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows) in
       assert_bool "burrow should have no liquidation slices"
         (Ligo.Big_map.find_opt burrow_id checker.liquidation_auctions.burrow_slices= None);

       assert_equal
         (Ligo.tez_from_literal "0mutez")
         (burrow_collateral_at_auction result)
         ~printer:Ligo.string_of_tez;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, _checker) = Checker.entrypoint_liquidation_auction_claim_win (checker, Checker.deticketify_liquidation_auction_claim_win bid) in

       assert_equal
         [LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "3_155_961mutez") (Option.get (LigoOp.Tezos.get_contract_opt alice_addr))]
         ops
         ~printer:show_operation_list;
    );
  ]
