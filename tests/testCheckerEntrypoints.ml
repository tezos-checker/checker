open OUnit2
open TestLib
open Error

let assert_unsealed_contract_raises_not_deployed_error f =
  fun _ ->
  Ligo.Tezos.reset ();
  let init_wrapper = CheckerMain.initial_wrapper bob_addr in

  Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
  assert_raises
    (Failure (Ligo.string_of_int error_ContractNotDeployed))
    (fun () -> (f init_wrapper))

let suite =
  "CheckerEntrypointsTests" >::: [
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

    ("wrapper_view_current_liquidation_auction_minimum_bid - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_current_liquidation_auction_minimum_bid ((), init_wrapper));
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
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_get_balance ((bob_addr, Fa2Interface.lqt_token_id), init_wrapper));
    );

    ("wrapper_view_total_supply - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_total_supply (Fa2Interface.kit_token_id, init_wrapper));
    );

    ("wrapper_view_all_tokens - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_all_tokens ((), init_wrapper));
    );

    ("wrapper_view_is_operator - unsealed" >::
     assert_unsealed_contract_raises_not_deployed_error
       (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_is_operator ((bob_addr, (alice_addr, Fa2Interface.kit_token_id)), init_wrapper));
    );

    (* Add tests here *)
  ]
