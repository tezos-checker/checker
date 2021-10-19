open OUnit2
open TestLib
open Error
open Tok
open Kit
open Ctez
open Lqt

let assert_unsealed_contract_raises_not_deployed_error f =
  fun _ ->
  Ligo.Tezos.reset ();
  let init_wrapper = CheckerMain.initial_wrapper bob_addr in

  Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
  assert_raises
    (Failure (Ligo.string_of_int error_ContractNotDeployed))
    (fun () -> (f init_wrapper))

let assert_unsealed_contract_fails_with_unwanted_tez f =
  with_sealed_wrapper
    (fun sealed_wrapper ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "1mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwantedTezGiven))
         (fun () -> (f sealed_wrapper))
    )

let with_sealed_state_and_cfmm_setup f =
  with_sealed_wrapper
    (fun sealed_wrapper ->
       Ligo.Tezos.reset ();
       let burrow_id = Ligo.nat_from_literal "74n" in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (burrow_id, None, tok_of_denomination (Ligo.nat_from_literal "10_000_000n"))))) in
       let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
       (* Mint some kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:62 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let op = CheckerMain.(CheckerEntrypoint (LazyParams (Mint_kit (burrow_id, Kit.kit_one)))) in
       let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
       (* Add some liquidity *)
       Ligo.Tezos.new_transaction ~seconds_passed:121 ~blocks_passed:2 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ctez_to_give = Ctez.ctez_of_muctez (Ligo.nat_from_literal "400_000n") in
       let kit_to_give = Kit.kit_of_denomination (Ligo.nat_from_literal "400_000n") in
       let min_lqt_to_mint = Lqt.lqt_of_denomination (Ligo.nat_from_literal "5n") in
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
       let op = CheckerMain.(CheckerEntrypoint (LazyParams (Add_liquidity (ctez_to_give, kit_to_give, min_lqt_to_mint, deadline)))) in
       let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
       (* Proceed with what comes next *)
       Ligo.Tezos.new_transaction ~seconds_passed:59 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       f sealed_wrapper
    )

let suite =
  "CheckerEntrypointsTests" >::: [
    (* Test views on unsealed checker *)
    ("wrapper_view_buy_kit_min_kit_expected - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_buy_kit_min_kit_expected (Ctez.ctez_zero, init_wrapper));
    );

    ("wrapper_view_sell_kit_min_ctez_expected - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_sell_kit_min_ctez_expected (Kit.kit_zero, init_wrapper));
    );

    ("wrapper_view_add_liquidity_max_kit_deposited - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_add_liquidity_max_kit_deposited (Ctez.ctez_zero, init_wrapper));
    );

    ("wrapper_view_add_liquidity_min_lqt_minted - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_add_liquidity_min_lqt_minted (Ctez.ctez_zero, init_wrapper));
    );

    ("wrapper_view_remove_liquidity_min_ctez_withdrawn - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_remove_liquidity_min_ctez_withdrawn (Lqt.lqt_zero, init_wrapper));
    );

    ("wrapper_view_remove_liquidity_min_kit_withdrawn - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_remove_liquidity_min_kit_withdrawn (Lqt.lqt_zero, init_wrapper));
    );

    ("wrapper_view_current_liquidation_auction_details - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_current_liquidation_auction_details ((), init_wrapper));
    );

    ("wrapper_view_burrow_max_mintable_kit - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_burrow_max_mintable_kit ((bob_addr, Ligo.nat_from_literal "0n"), init_wrapper));
    );

    ("wrapper_view_is_burrow_overburrowed - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_is_burrow_overburrowed ((bob_addr, Ligo.nat_from_literal "0n"), init_wrapper));
    );

    ("wrapper_view_is_burrow_liquidatable - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_is_burrow_liquidatable ((bob_addr, Ligo.nat_from_literal "0n"), init_wrapper));
    );

    ("wrapper_view_get_balance - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_get_balance ((bob_addr, Lqt.lqt_token_id), init_wrapper));
    );

    ("wrapper_view_total_supply - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_total_supply (Kit.kit_token_id, init_wrapper));
    );

    ("wrapper_view_all_tokens - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_all_tokens ((), init_wrapper));
    );

    ("wrapper_view_is_operator - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_is_operator ((bob_addr, (alice_addr, Kit.kit_token_id)), init_wrapper));
    );

    (* Test views on sealed checker *)
    ("wrapper_view_buy_kit_min_kit_expected - sealed" >::
     with_sealed_state_and_cfmm_setup
       (fun sealed_wrapper ->
          let ctez_to_sell = Ctez.ctez_of_muctez (Ligo.nat_from_literal "100_000n") in
          let min_kit_to_buy = CheckerEntrypoints.wrapper_view_buy_kit_min_kit_expected (ctez_to_sell, sealed_wrapper) in
          let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
          (* must succeed, otherwise wrapper_view_buy_kit_min_kit_expected overapproximated *)
          let _ =
            let op = CheckerMain.(CheckerEntrypoint (LazyParams (Buy_kit (ctez_to_sell, min_kit_to_buy, deadline)))) in
            let _ops, _sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
            () in
          (* must fail, otherwise wrapper_view_buy_kit_min_kit_expected underapproximated *)
          assert_raises
            (Failure (Ligo.string_of_int error_BuyKitPriceFailure))
            (fun () ->
               let min_kit_to_buy = Kit.kit_add min_kit_to_buy (Kit.kit_of_denomination (Ligo.nat_from_literal "1n")) in
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Buy_kit (ctez_to_sell, min_kit_to_buy, deadline)))) in
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    ("wrapper_view_sell_kit_min_ctez_expected - sealed" >::
     with_sealed_state_and_cfmm_setup
       (fun sealed_wrapper ->
          let kit_to_sell = Kit.kit_of_denomination (Ligo.nat_from_literal "100_000n") in
          let min_ctez_to_buy = CheckerEntrypoints.wrapper_view_sell_kit_min_ctez_expected (kit_to_sell, sealed_wrapper) in
          let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
          (* must succeed, otherwise wrapper_view_sell_kit_min_ctez_expected overapproximated *)
          let _ =
            let op = CheckerMain.(CheckerEntrypoint (LazyParams (Sell_kit (kit_to_sell, min_ctez_to_buy, deadline)))) in
            let _ops, _sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
            () in
          (* must fail, otherwise wrapper_view_sell_kit_min_ctez_expected underapproximated *)
          assert_raises
            (Failure (Ligo.string_of_int error_SellKitPriceFailure))
            (fun () ->
               let min_ctez_to_buy = Ctez.ctez_add min_ctez_to_buy (Ctez.ctez_of_muctez (Ligo.nat_from_literal "1n")) in
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Sell_kit (kit_to_sell, min_ctez_to_buy, deadline)))) in
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    ("wrapper_view_add_liquidity_max_kit_deposited / wrapper_view_add_liquidity_min_lqt_minted - sealed" >::
     with_sealed_state_and_cfmm_setup
       (fun sealed_wrapper ->
          let ctez_to_sell = Ctez.ctez_of_muctez (Ligo.nat_from_literal "100_000n") in
          let max_kit_to_sell = CheckerEntrypoints.wrapper_view_add_liquidity_max_kit_deposited (ctez_to_sell, sealed_wrapper) in
          let min_lqt_to_buy = CheckerEntrypoints.wrapper_view_add_liquidity_min_lqt_minted (ctez_to_sell, sealed_wrapper) in
          let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
          (* must succeed, otherwise
           * wrapper_view_add_liquidity_max_kit_deposited underapproximated or
           * wrapper_view_add_liquidity_min_lqt_minted overapproximated (or both of them did) *)
          let _ =
            let op = CheckerMain.(CheckerEntrypoint (LazyParams (Add_liquidity (ctez_to_sell, max_kit_to_sell, min_lqt_to_buy, deadline)))) in
            let _ops, _sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
            () in
          (* must fail, otherwise wrapper_view_add_liquidity_max_kit_deposited overapproximated *)
          assert_raises
            (Failure (Ligo.string_of_int error_AddLiquidityTooMuchKitRequired))
            (fun () ->
               let max_kit_to_sell = Kit.kit_sub max_kit_to_sell (Kit.kit_of_denomination (Ligo.nat_from_literal "1n")) in
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Add_liquidity (ctez_to_sell, max_kit_to_sell, min_lqt_to_buy, deadline)))) in
               CheckerMain.main (op, sealed_wrapper)
            );
          (* must fail, otherwise wrapper_view_add_liquidity_min_lqt_minted underapproximated *)
          assert_raises
            (Failure (Ligo.string_of_int error_AddLiquidityTooLowLiquidityMinted))
            (fun () ->
               let min_lqt_to_buy = Lqt.lqt_add min_lqt_to_buy (Lqt.lqt_of_denomination (Ligo.nat_from_literal "1n")) in
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Add_liquidity (ctez_to_sell, max_kit_to_sell, min_lqt_to_buy, deadline)))) in
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    ("wrapper_view_remove_liquidity_min_ctez_withdrawn / wrapper_view_remove_liquidity_min_kit_withdrawn - sealed" >::
     with_sealed_state_and_cfmm_setup
       (fun sealed_wrapper ->
          let lqt_to_sell = Lqt.lqt_of_denomination (Ligo.nat_from_literal "5n") in
          let min_ctez_to_buy = CheckerEntrypoints.wrapper_view_remove_liquidity_min_ctez_withdrawn (lqt_to_sell, sealed_wrapper) in
          let min_kit_to_buy = CheckerEntrypoints.wrapper_view_remove_liquidity_min_kit_withdrawn (lqt_to_sell, sealed_wrapper) in
          let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
          (* must succeed, otherwise
           * wrapper_view_remove_liquidity_min_ctez_withdrawn overapproximated or
           * wrapper_view_remove_liquidity_min_kit_withdrawn overapproximated (or both of them did) *)
          let _ =
            let op = CheckerMain.(CheckerEntrypoint (LazyParams (Remove_liquidity (lqt_to_sell, min_ctez_to_buy, min_kit_to_buy, deadline)))) in
            let _ops, _sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
            () in
          (* must fail, otherwise wrapper_view_remove_liquidity_min_ctez_withdrawn underapproximated *)
          assert_raises
            (Failure (Ligo.string_of_int error_RemoveLiquidityCantWithdrawEnoughCtez))
            (fun () ->
               let min_ctez_to_buy = Ctez.ctez_add min_ctez_to_buy (Ctez.ctez_of_muctez (Ligo.nat_from_literal "1n")) in
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Remove_liquidity (lqt_to_sell, min_ctez_to_buy, min_kit_to_buy, deadline)))) in
               CheckerMain.main (op, sealed_wrapper)
            );
          (* must fail, otherwise wrapper_view_remove_liquidity_min_kit_withdrawn underapproximated *)
          assert_raises
            (Failure (Ligo.string_of_int error_RemoveLiquidityCantWithdrawEnoughKit))
            (fun () ->
               let min_kit_to_buy = Kit.kit_add min_kit_to_buy (Kit.kit_of_denomination (Ligo.nat_from_literal "1n")) in
               let op = CheckerMain.(CheckerEntrypoint (LazyParams (Remove_liquidity (lqt_to_sell, min_ctez_to_buy, min_kit_to_buy, deadline)))) in
               CheckerMain.main (op, sealed_wrapper)
            )
       )
    );

    ("wrapper_view_current_liquidation_auction_details - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_raises
            (Failure (Ligo.string_of_int error_NoOpenAuction))
            (fun () -> CheckerEntrypoints.wrapper_view_current_liquidation_auction_details ((), sealed_wrapper))
       )
    );

    ("wrapper_view_burrow_max_mintable_kit - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let initial_amount = tok_add Constants.creation_deposit Constants.creation_deposit in
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (Ligo.nat_from_literal "0n", None, initial_amount)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
          assert_kit_equal
            ~expected:(Kit.kit_of_denomination (Ligo.nat_from_literal "476_190n"))
            ~real:(CheckerEntrypoints.wrapper_view_burrow_max_mintable_kit ((bob_addr, Ligo.nat_from_literal "0n"), sealed_wrapper))
       )
    );

    ("wrapper_view_is_burrow_overburrowed - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (Ligo.nat_from_literal "0n", None, Constants.creation_deposit)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
          assert_bool
            "burrow cannot be overburrowed already"
            (not (CheckerEntrypoints.wrapper_view_is_burrow_overburrowed ((bob_addr, Ligo.nat_from_literal "0n"), sealed_wrapper)))
       )
    );

    ("wrapper_view_is_burrow_liquidatable - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:Common.tez_zero;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (Ligo.nat_from_literal "0n", None, Constants.creation_deposit)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
          assert_bool
            "burrow cannot be liquidatable already"
            (not (CheckerEntrypoints.wrapper_view_is_burrow_liquidatable ((bob_addr, Ligo.nat_from_literal "0n"), sealed_wrapper)))
       )
    );

    ("wrapper_view_get_balance - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_nat_equal
            ~expected:(Ligo.nat_from_literal "0n")
            ~real:(CheckerEntrypoints.wrapper_view_get_balance ((bob_addr, Lqt.lqt_token_id), sealed_wrapper))
       )
    );

    ("wrapper_view_total_supply - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_nat_equal
            ~expected:(Ligo.nat_from_literal "0n")
            ~real:(CheckerEntrypoints.wrapper_view_total_supply (Kit.kit_token_id, sealed_wrapper))
       )
    );

    ("wrapper_view_all_tokens - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_nat_list_equal
            ~expected:[Kit.kit_token_id; Lqt.lqt_token_id]
            ~real:(CheckerEntrypoints.wrapper_view_all_tokens ((), sealed_wrapper))
       )
    );

    ("wrapper_view_is_operator - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_bool
            "no operators had been set"
            (not (CheckerEntrypoints.wrapper_view_is_operator ((bob_addr, (leena_addr, Kit.kit_token_id)), sealed_wrapper)))
       )
    );

    (* Test failures when checker is accidentally given any tez (sealed wrapper). *)
    ("strict_entrypoint_transfer (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper -> CheckerMain.(main (CheckerEntrypoint (StrictParams (Transfer ([]))), sealed_wrapper)))
    );

    ("strict_entrypoint_balance_of (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let fa2_balance_of_param =
            Fa2Interface.{
              requests = [];
              callback = Ligo.contract_of_address (Ligo.address_of_string "test address");
            } in
          CheckerMain.(main (CheckerEntrypoint (StrictParams (Balance_of (fa2_balance_of_param))), sealed_wrapper))
       )
    );

    ("entrypoint_update_operators (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper -> CheckerMain.(main (CheckerEntrypoint (LazyParams (Update_operators ([]))), sealed_wrapper)))
    );

    ("entrypoint_touch (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper -> CheckerMain.(main (CheckerEntrypoint (LazyParams (Touch ())), sealed_wrapper)))
    );

    ("entrypoint_create_burrow (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "123n", None, tok_of_denomination (Ligo.nat_from_literal "2_245_874n")) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Create_burrow (param))), sealed_wrapper))
       )
    );

    ("entrypoint_deposit_collateral (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "3_000n", tok_of_denomination (Ligo.nat_from_literal "1_532_321n")) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Deposit_collateral (param))), sealed_wrapper))
       )
    );

    ("entrypoint_withdraw_collateral (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "4n", tok_of_denomination (Ligo.nat_from_literal "1_321n")) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Withdraw_collateral (param))), sealed_wrapper))
       )
    );

    ("entrypoint_mint_kit (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "0n", (kit_of_denomination (Ligo.nat_from_literal "10_000_000n"))) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Mint_kit (param))), sealed_wrapper))
       )
    );

    ("entrypoint_burn_kit (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "12n", (kit_of_denomination (Ligo.nat_from_literal "11_000_000n"))) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Burn_kit (param))), sealed_wrapper))
       )
    );

    ("entrypoint_activate_burrow (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "15n", Constants.creation_deposit) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Activate_burrow (param))), sealed_wrapper))
       )
    );

    ("entrypoint_deactivate_burrow (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "119n", bob_addr) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Deactivate_burrow (param))), sealed_wrapper))
       )
    );

    ("entrypoint_mark_for_liquidation (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (bob_addr, Ligo.nat_from_literal "119n") in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Mark_for_liquidation (param))), sealed_wrapper))
       )
    );

    ("entrypoint_touch_liquidation_slices (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper -> CheckerMain.(main (CheckerEntrypoint (LazyParams (Touch_liquidation_slices ([]))), sealed_wrapper)))
    );

    ("entrypoint_cancel_liquidation_slice (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = LiquidationAuctionPrimitiveTypes.LeafPtr (Ptr.random_ptr ()) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Cancel_liquidation_slice (param))), sealed_wrapper))
       )
    );

    ("entrypoint_touch_burrow (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (bob_addr, Ligo.nat_from_literal "43235n") in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Touch_burrow (param))), sealed_wrapper))
       )
    );

    ("entrypoint_set_burrow_delegate (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (Ligo.nat_from_literal "43235n", Some charles_key_hash) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Set_burrow_delegate (param))), sealed_wrapper))
       )
    );

    ("entrypoint_buy_kit (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (
              ctez_of_muctez (Ligo.nat_from_literal "1n"),
              kit_of_denomination (Ligo.nat_from_literal "2n"),
              Ligo.timestamp_from_seconds_literal 3
            ) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Buy_kit (param))), sealed_wrapper))
       )
    );

    ("entrypoint_sell_kit (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (
              kit_of_denomination (Ligo.nat_from_literal "1n"),
              ctez_of_muctez (Ligo.nat_from_literal "2n"),
              Ligo.timestamp_from_seconds_literal 3
            ) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Sell_kit (param))), sealed_wrapper))
       )
    );

    ("entrypoint_add_liquidity (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (
              ctez_of_muctez (Ligo.nat_from_literal "1n"),
              kit_of_denomination (Ligo.nat_from_literal "2n"),
              lqt_of_denomination (Ligo.nat_from_literal "3n"),
              Ligo.timestamp_from_seconds_literal 4
            ) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Add_liquidity (param))), sealed_wrapper))
       )
    );

    ("entrypoint_remove_liquidity (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (
              lqt_of_denomination (Ligo.nat_from_literal "1n"),
              ctez_of_muctez (Ligo.nat_from_literal "2n"),
              kit_of_denomination (Ligo.nat_from_literal "3n"),
              Ligo.timestamp_from_seconds_literal 4
            ) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Remove_liquidity (param))), sealed_wrapper))
       )
    );

    ("entrypoint_liquidation_auction_place_bid (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (LiquidationAuctionPrimitiveTypes.AVLPtr (Ptr.random_ptr ()), kit_of_denomination (Ligo.nat_from_literal "3n")) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Liquidation_auction_place_bid (param))), sealed_wrapper))
       )
    );

    ("entrypoint_liquidation_auction_claim_win (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper ->
          let param = (LiquidationAuctionPrimitiveTypes.AVLPtr (Ptr.random_ptr ())) in
          CheckerMain.(main (CheckerEntrypoint (LazyParams (Liquidation_auction_claim_win (param))), sealed_wrapper))
       )
    );

    ("entrypoint_receive_price (main) - fails when unwanted tez is given - sealed state" >::
     assert_unsealed_contract_fails_with_unwanted_tez
       (fun sealed_wrapper -> CheckerMain.(main (CheckerEntrypoint (LazyParams (Receive_price (Ligo.nat_from_literal "63534655n"))), sealed_wrapper)))
    );

    (* FIXME: Add tests for the remaining entrypoints here *)

    (* FIXME: Add tests for an unsealed wrapper for each entrypoint too. *)

    (* Add tests here *)
  ]

let () =
  run_test_tt_main
    suite
