open OUnit2
open TestLib
open CheckerMain
open Error
open LiquidationAuctionPrimitiveTypes

(* NOTE: The tests in this file mainly try to cover all execution paths in
 * CheckerMain.main, but don't do much more. At this high a level most of the
 * actual code to be run is wrapped in {BEGIN/END}_LIGO, so I think e2e tests
 * are more appropriate for checking the actual outputs. *)

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

(* This function lets us test all different branches of
 * CheckerEntrypoints.lazyParamsToLazyFunctionId. This is the reason we pass
 * the lazy parameters here, instead of the function id and the bytes
 * separately, so that we can call lazyParamsToLazyFunctionId on each possible
 * argument and increase test coverage. *)
let test_deploy_function_with_lazy_params_succeeds lazy_params =
  Ligo.Tezos.reset ();
  Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:Common.tez_zero;

  let wrapper = CheckerMain.initial_wrapper leena_addr in (* unsealed *)

  let fn_id, fn_bytes = CheckerEntrypoints.(lazyParamsToLazyFunctionId lazy_params) in
  let op = CheckerMain.DeployFunction (fn_id, fn_bytes) in

  (* This call should succeed (first time, no previous entry). *)
  let _ops, wrapper = CheckerMain.main (op, wrapper) in
  (* This call should also succeed (second time, concatenation). *)
  (* Note: it does not really make sense deploying the exact same bytes twice,
   * but DeployFunction is not meant to inspect the bytes, only concatenate
   * them, so the call should succeed regardless. *)
  let _ops, _wrapper = CheckerMain.main (op, wrapper) in
  ()

let lift_to_sealed_wrapper (f: CheckerTypes.checker -> 'a) : CheckerTypes.wrapper -> 'a =
  fun sealed_wrapper ->
  CheckerTypes.(
    match sealed_wrapper.deployment_state with
    | Unsealed _ -> failwith "lift_to_sealed_wrapper: Unsealed"
    | Sealed state -> f state
  )

let set_last_price_in_wrapper wrapper price_option =
  CheckerTypes.(
    match wrapper.deployment_state with
    | Unsealed _ -> wrapper
    | Sealed state ->
      {wrapper with deployment_state = Sealed {state with last_price = price_option}}
  )

let find_burrow_in_sealed_wrapper sealed_wrapper burrow_id =
  lift_to_sealed_wrapper
    (fun state -> Checker.find_burrow state.burrows burrow_id)
    sealed_wrapper

let suite =
  "CheckerMainTests" >::: [
    (* initial_wrapper *)
    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"initial wrapper - owner is set upon creation"
        ~count:property_test_count
        TestArbitrary.arb_address
      @@ fun (addr) ->
      let wrapper = initial_wrapper addr in
      (wrapper.deployment_state = Unsealed addr)
    );

    ("initial wrapper - lazy_functions bigmap is empty" >::
     fun _ ->
       assert_bool
         "initial lazy_functions bigmap must be empty"
         (List.length (Ligo.Big_map.bindings (initial_wrapper bob_addr).lazy_functions) = 0)
    );

    ("initial wrapper - metadata bigmap is empty" >::
     fun _ ->
       assert_bool
         "initial metadata bigmap must be empty"
         (List.length (Ligo.Big_map.bindings (initial_wrapper bob_addr).metadata) = 0)
    );


    (* Succeeding cases (DeployFunction) when checker is not sealed yet *)
    ("If checker is not sealed, the deployer should be able to call DeployFunction - Touch" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Touch ())
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Create_burrow" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Create_burrow (Ligo.nat_from_literal "63n", Some charles_key_hash, Tok.tok_zero)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Deposit_collateral" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Deposit_collateral (Ligo.nat_from_literal "128n", Tok.tok_zero)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Withdraw_collateral" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Withdraw_collateral (Ligo.nat_from_literal "32n", Tok.tok_of_denomination (Ligo.nat_from_literal "4_231_643n"))) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Mint_kit" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Mint_kit (Ligo.nat_from_literal "29n", Kit.kit_of_denomination (Ligo.nat_from_literal "231_643n"))) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Burn_kit" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Burn_kit (Ligo.nat_from_literal "826n", Kit.kit_of_denomination (Ligo.nat_from_literal "5_473_525_867n"))) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Activate_burrow" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Activate_burrow (Ligo.nat_from_literal "62_516n", Tok.tok_zero)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Deactivate_burrow" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Deactivate_burrow (Ligo.nat_from_literal "674n", alice_addr)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Mark_for_liquidation" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Mark_for_liquidation (alice_addr, Ligo.nat_from_literal "674n")) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Touch_liquidation_slices" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Touch_liquidation_slices ([]))
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Cancel_liquidation_slice" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Cancel_liquidation_slice (LeafPtr (Ptr.(ptr_next (ptr_next (ptr_next ptr_null)))))) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Touch_burrow" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Touch_burrow (bob_addr, Ligo.nat_from_literal "2n")) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Set_burrow_delegate" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Set_burrow_delegate (Ligo.nat_from_literal "2n", Some charles_key_hash)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Buy_kit" >::
     fun _ ->
       let ctok = Ctok.ctok_of_muctok (Ligo.nat_from_literal "13n") in
       let kit = Kit.kit_of_denomination (Ligo.nat_from_literal "29n") in
       let deadline = !Ligo.Tezos.now in
       test_deploy_function_with_lazy_params_succeeds
         (Buy_kit (ctok, kit, deadline)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Sell_kit" >::
     fun _ ->
       let kit = Kit.kit_of_denomination (Ligo.nat_from_literal "31n") in
       let ctok = Ctok.ctok_of_muctok (Ligo.nat_from_literal "5n") in
       let deadline = !Ligo.Tezos.now in
       test_deploy_function_with_lazy_params_succeeds
         (Sell_kit (kit, ctok, deadline)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Add_liquidity" >::
     fun _ ->
       let ctok = Ctok.ctok_of_muctok (Ligo.nat_from_literal "97n") in
       let kit = Kit.kit_of_denomination (Ligo.nat_from_literal "3n") in
       let lqt = Lqt.lqt_of_denomination (Ligo.nat_from_literal "59n") in
       let deadline = !Ligo.Tezos.now in
       test_deploy_function_with_lazy_params_succeeds
         (Add_liquidity (ctok, kit, lqt, deadline)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Remove_liquidity" >::
     fun _ ->
       let lqt = Lqt.lqt_of_denomination (Ligo.nat_from_literal "41n") in
       let ctok = Ctok.ctok_of_muctok (Ligo.nat_from_literal "47n") in
       let kit = Kit.kit_of_denomination (Ligo.nat_from_literal "19n") in
       let deadline = !Ligo.Tezos.now in
       test_deploy_function_with_lazy_params_succeeds
         (Remove_liquidity (lqt, ctok, kit, deadline)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Liquidation_auction_place_bid" >::
     fun _ ->
       let auction_id = AVLPtr (Ptr.(ptr_next (ptr_next ptr_null))) in (* 2 *)
       let kit = Kit.kit_of_denomination (Ligo.nat_from_literal "98n") in
       test_deploy_function_with_lazy_params_succeeds
         (Liquidation_auction_place_bid (auction_id, kit)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Liquidation_auction_claim_win" >::
     fun _ ->
       let auction_id = AVLPtr (Ptr.(ptr_next (ptr_next (ptr_next (ptr_next ptr_null))))) in (* 4 *)
       test_deploy_function_with_lazy_params_succeeds
         (Liquidation_auction_claim_win (auction_id)) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Receive_price" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Receive_price (Ligo.nat_from_literal "4_234n")) (* note: values randomly chosen *)
    );

    ("If checker is not sealed, the deployer should be able to call DeployFunction - Update_operators" >::
     fun _ ->
       test_deploy_function_with_lazy_params_succeeds
         (Update_operators ([]))
    );

    (* Succeeding cases (DeployMetadata) when checker is not sealed yet *)
    ("If checker is not sealed, the deployer should be able to call DeployMetadata" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let wrapper = CheckerMain.initial_wrapper leena_addr in (* unsealed *)

       let bs = Ligo.bytes_from_literal "0x021324" in
       let op = CheckerMain.DeployMetadata bs in

       (* This call should succeed (first time, no previous entry). *)
       let _ops, wrapper = CheckerMain.main (op, wrapper) in
       (* This call should also succeed (second time, concatenation). *)
       let _ops, _wrapper = CheckerMain.main (op, wrapper) in
       ()
    );

    (* Failing cases when checker is not sealed yet *)
    ("If checker is not sealed, only the deployer should be able to work with it" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let wrapper = CheckerMain.initial_wrapper leena_addr in (* unsealed *)

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let op = CheckerMain.SealContract (oracle_addr, collateral_fa2_addr, cfmm_token_fa12_addr) in
       assert_raises
         (Failure (Ligo.string_of_int error_UnauthorisedCaller))
         (fun () -> CheckerMain.main (op, wrapper))
    );

    ("If checker is not sealed, we shouldn't be able to invoke entrypoints" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let wrapper = CheckerMain.initial_wrapper leena_addr in (* unsealed *)
       let op = CheckerMain.(CheckerEntrypoint (StrictParams (Transfer []))) in
       assert_raises
         (Failure (Ligo.string_of_int error_ContractNotDeployed))
         (fun () -> CheckerMain.main (op, wrapper))
    );

    (* (Strict) Succeeding cases when checker is already sealed *)
    ("If checker is sealed, users should be able to call CheckerEntrypoint/StrictParams/Transfer" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.(CheckerEntrypoint (StrictParams (Transfer []))) in
          (* This call should succeed *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let _ops, _wrapper = CheckerMain.main (op, sealed_wrapper) in
          ()
       )
    );

    ("If checker is sealed, users should be able to call CheckerEntrypoint/StrictParams/Balance_of" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let requests = [] in
          let callback =
            Option.get
              (LigoOp.Tezos.get_entrypoint_opt "%some_entrypoint_name_here" alice_addr
               : (Fa2Interface.fa2_balance_of_response list) Ligo.contract option) in
          let fa2_balance_of_param = Fa2Interface.{requests; callback;} in
          let op = CheckerMain.(CheckerEntrypoint (StrictParams (Balance_of fa2_balance_of_param))) in
          (* This call should succeed *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let _ops, _wrapper = CheckerMain.main (op, sealed_wrapper) in
          ()
       )
    );

    (* (Lazy) Succeeding cases when checker is already sealed *)
    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/Touch" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          (* This call should succeed *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let _ops, _wrapper = CheckerMain.main (op, sealed_wrapper) in
          ()
       )
    );

    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/<operation on burrow>" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          (* This test just runs a simple scenario of a user creating a burrow and then
           * operating on it, to increase path coverage for CheckerMain.main. *)
          Ligo.Tezos.reset ();
          let burrow_id = Ligo.nat_from_literal "199n" in
          let user_addr = alice_addr in

          (* Create_burrow *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:user_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (burrow_id, None, Tok.tok_of_denomination (Ligo.nat_from_literal "5_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Deposit_collateral *)
          Ligo.Tezos.new_transaction ~seconds_passed:62 ~blocks_passed:1 ~sender:user_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Deposit_collateral (burrow_id, Tok.tok_of_denomination (Ligo.nat_from_literal "6_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Withdraw_collateral *)
          Ligo.Tezos.new_transaction ~seconds_passed:121 ~blocks_passed:2 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Withdraw_collateral (burrow_id, Tok.tok_of_denomination (Ligo.nat_from_literal "1_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Mint_kit *)
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mint_kit (burrow_id, Kit.kit_of_denomination (Ligo.nat_from_literal "1_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Idea: Might want to touch checker here, before burning the kit, so that the
           * owed kit increases (overburrowedness), but due to US touching checker there
           * should be enough kit to pay back the burrow. *)

          (* Burn_kit *)
          Ligo.Tezos.new_transaction ~seconds_passed:200 ~blocks_passed:3 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Burn_kit (burrow_id, Kit.kit_of_denomination (Ligo.nat_from_literal "1_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Set_burrow_delegate *)
          Ligo.Tezos.new_transaction ~seconds_passed:202 ~blocks_passed:3 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Set_burrow_delegate (burrow_id, Some charles_key_hash)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Deactivate_burrow *)
          Ligo.Tezos.new_transaction ~seconds_passed:65 ~blocks_passed:1 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Deactivate_burrow (burrow_id, user_addr)))) in (* send it back to the user *)
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Activate_burrow *)
          Ligo.Tezos.new_transaction ~seconds_passed:129 ~blocks_passed:2 ~sender:user_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Activate_burrow (burrow_id, Tok.tok_of_denomination (Ligo.nat_from_literal "10_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Touch_burrow *)
          Ligo.Tezos.new_transaction ~seconds_passed:342 ~blocks_passed:5 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch_burrow (user_addr, burrow_id)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: mint as much kit as possible *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let max_mintable_kit = CheckerEntrypoints.wrapper_view_burrow_max_mintable_kit ((user_addr, burrow_id), sealed_wrapper) in
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mint_kit (burrow_id, max_mintable_kit)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: increase the index significantly (emulate the effects of Receive_price) *)
          let sealed_wrapper = set_last_price_in_wrapper sealed_wrapper (Some (Ligo.nat_from_literal "100_000_000n")) in

          (* setup: let enough time pass so that the burrow becomes liquidatable *)
          let blocks_passed = 191 in
          Ligo.Tezos.new_transaction ~seconds_passed:(60 * blocks_passed) ~blocks_passed:blocks_passed ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Mark_for_liquidation *)
          Ligo.Tezos.new_transaction ~seconds_passed:342 ~blocks_passed:5 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez"); (* the user themselves can mark it *)
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mark_for_liquidation (user_addr, burrow_id)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Note: I would have liked to be able to recollateralize the burrow and try
           * out Cancel_liquidation_slice here, but to do that we need to be able to find
           * th leaf_ptr of the slice, and currently that's hard to do. If/when we
           * address #150 we should be able to do that. *)

          (* setup: touch to start the auction *)
          Ligo.Tezos.new_transaction ~seconds_passed:63 ~blocks_passed:1 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Liquidation_auction_place_bid *)
          let min_bid = CheckerEntrypoints.wrapper_view_current_liquidation_auction_details ((), sealed_wrapper) in
          Ligo.Tezos.new_transaction ~seconds_passed:394 ~blocks_passed:6 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Liquidation_auction_place_bid (min_bid.auction_id, min_bid.minimum_bid)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Note: to avoid the unused variable warning. *)
          assert_equal sealed_wrapper sealed_wrapper
       )
    );

    ("Regression test for #209 (1)" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          Ligo.Tezos.reset ();
          let burrow_id = Ligo.nat_from_literal "199n" in
          let user_addr = alice_addr in

          (* Create_burrow *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:user_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (burrow_id, None, Tok.tok_of_denomination (Ligo.nat_from_literal "10_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: mint as much kit as possible *)
          Ligo.Tezos.new_transaction ~seconds_passed:1181 ~blocks_passed:18 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let max_mintable_kit = CheckerEntrypoints.wrapper_view_burrow_max_mintable_kit ((user_addr, burrow_id), sealed_wrapper) in
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mint_kit (burrow_id, max_mintable_kit)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: increase the index significantly (emulate the effects of Receive_price) *)
          let sealed_wrapper = set_last_price_in_wrapper sealed_wrapper (Some (Ligo.nat_from_literal "1_357_906n")) in (* lowest value I could get, assuming the rest of the setting. *)

          (* setup: let enough time pass so that the burrow becomes liquidatable *)
          let blocks_passed = 191 in
          Ligo.Tezos.new_transaction ~seconds_passed:(60 * blocks_passed) ~blocks_passed:blocks_passed ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Mark_for_liquidation *)
          Ligo.Tezos.new_transaction ~seconds_passed:342 ~blocks_passed:5 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez"); (* the user themselves can mark it *)
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mark_for_liquidation (user_addr, burrow_id)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: touch to start the auction *)
          Ligo.Tezos.new_transaction ~seconds_passed:63 ~blocks_passed:1 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Liquidation_auction_place_bid *)
          let min_bid = CheckerEntrypoints.wrapper_view_current_liquidation_auction_details ((), sealed_wrapper) in
          Ligo.Tezos.new_transaction ~seconds_passed:394 ~blocks_passed:6 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Liquidation_auction_place_bid (min_bid.auction_id, min_bid.minimum_bid)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: make enough time pass so that the auction finishes *)
          let seconds_passed = int_of_string (Ligo.string_of_int (Ligo.add_int_int Constants.max_bid_interval_in_seconds (Ligo.int_from_literal "1"))) in
          let blocks_passed = int_of_string (Ligo.string_of_nat (Ligo.add_nat_nat Constants.max_bid_interval_in_blocks (Ligo.nat_from_literal "1n"))) in
          Ligo.Tezos.new_transaction ~seconds_passed:seconds_passed ~blocks_passed:blocks_passed ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Note: to avoid the unused variable warning. *)
          assert_equal sealed_wrapper sealed_wrapper
       )
    );

    ("Regression test for #209 (2)" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          Ligo.Tezos.reset ();
          let burrow_id = Ligo.nat_from_literal "199n" in
          let user_addr = alice_addr in

          (* Create_burrow *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:user_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (burrow_id, None, Tok.tok_of_denomination (Ligo.nat_from_literal "10_000_000n"))))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: mint as much kit as possible *)
          Ligo.Tezos.new_transaction ~seconds_passed:1181 ~blocks_passed:18 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let max_mintable_kit = CheckerEntrypoints.wrapper_view_burrow_max_mintable_kit ((user_addr, burrow_id), sealed_wrapper) in
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mint_kit (burrow_id, max_mintable_kit)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* setup: increase the index significantly (emulate the effects of Receive_price) *)
          let sealed_wrapper = set_last_price_in_wrapper sealed_wrapper (Some (Ligo.nat_from_literal "1_357_906n")) in (* lowest value I could get, assuming the rest of the setting. *)

          (* setup: let enough time pass so that the burrow becomes liquidatable *)
          let blocks_passed = 191 in
          Ligo.Tezos.new_transaction ~seconds_passed:(60 * blocks_passed) ~blocks_passed:blocks_passed ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch ()))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Mark_for_liquidation *)
          Ligo.Tezos.new_transaction ~seconds_passed:342 ~blocks_passed:5 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez"); (* the user themselves can mark it *)
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mark_for_liquidation (user_addr, burrow_id)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          let real_outstanding_kit, approx_outstanding_kit =
            lift_to_sealed_wrapper Checker.compute_outstanding_dissonance sealed_wrapper in
          assert_bool
            "Regression test for #209 (2) efficacy"
            (Kit.lt_kit_kit approx_outstanding_kit real_outstanding_kit);

          (* setup: try to pay back all the outstanding kit (underapproximation should be triggered) *)
          let total_real_outstanding_kit = Burrow.burrow_outstanding_kit (find_burrow_in_sealed_wrapper sealed_wrapper (user_addr, burrow_id)) in
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:user_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Burn_kit (burrow_id, total_real_outstanding_kit)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in

          (* Note: to avoid the unused variable warning. *)
          assert_equal sealed_wrapper sealed_wrapper
       )
    );

    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/Touch_liquidation_slices" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Touch_liquidation_slices ([])))) in
          (* This call should succeed *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let _ops, _wrapper = CheckerMain.main (op, sealed_wrapper) in
          ()
       )
    );

    (* Note: rather superficial call, to increase path coverage;
     * I would have liked a call that succeeds here instead, but
     * #150 makes this annoying to achieve. *)
    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/Cancel_liquidation_slice" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_raises
            (Failure (Ligo.string_of_int error_InvalidLeafPtr))
            (fun () ->
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Cancel_liquidation_slice (LeafPtr (Ptr.(ptr_next (ptr_next (ptr_next ptr_null)))))))) in  (* note: values randomly chosen *)
               Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    (* Note: rather superficial call, to increase path coverage; once the
     * FIXME above is addressed we can claim the win more meaningfully. *)
    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/Liquidation_auction_claim_win" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_raises
            (Failure (Ligo.string_of_int error_InvalidAvlPtr))
            (fun () ->
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Liquidation_auction_claim_win (AVLPtr (Ptr.(ptr_next (ptr_next ptr_null))))))) in  (* note: values randomly chosen *)
               Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/Receive_price" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Receive_price (Ligo.nat_from_literal "756n")))) in
          (* This call should succeed *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:oracle_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let _ops, _wrapper = CheckerMain.main (op, sealed_wrapper) in
          (* But if the call does not come from the oracle it should fail. *)
          assert_raises
            (Failure (Ligo.string_of_int error_UnauthorisedCaller))
            (fun () ->
               Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams/Update_operators" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Update_operators ([])))) in
          (* This call should succeed *)
          Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
          let _ops, _wrapper = CheckerMain.main (op, sealed_wrapper) in
          ()
       )
    );

    (* Failing cases when checker is already sealed *)
    ("DeployFunction - should fail if the contract is already sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let fn_id, fn_bytes = CheckerEntrypoints.lazyParamsToLazyFunctionId (CheckerEntrypoints.Touch ()) in
          let op = CheckerMain.DeployFunction (fn_id, fn_bytes) in
          assert_raises
            (Failure (Ligo.string_of_int error_ContractAlreadyDeployed))
            (fun () -> CheckerMain.main (op, sealed_wrapper))
       )
    );

    ("SealContract - should fail if the contract is already sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.SealContract (oracle_addr, collateral_fa2_addr, cfmm_token_fa12_addr) in
          assert_raises
            (Failure (Ligo.string_of_int error_ContractAlreadyDeployed))
            (fun () -> CheckerMain.main (op, sealed_wrapper))
       )
    );

    ("DeployMetadata - should fail if the contract is already sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let op = CheckerMain.DeployMetadata (Ligo.bytes_from_literal "0x0123456789ABCDEF") in
          assert_raises
            (Failure (Ligo.string_of_int error_ContractAlreadyDeployed))
            (fun () -> CheckerMain.main (op, sealed_wrapper))
       )
    );
  ]

let () =
  run_test_tt_main
    suite
