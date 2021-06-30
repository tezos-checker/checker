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

let ctez_addr = Ligo.address_of_string "ctez_addr"
let oracle_addr = Ligo.address_of_string "oracle_addr"

let with_sealed_wrapper f =
  fun _ ->

  let checker_deployer = leena_addr in
  Ligo.Tezos.reset ();
  Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:checker_deployer ~amount:(Ligo.tez_from_literal "0mutez");

  let wrapper = CheckerMain.initial_wrapper checker_deployer in (* unsealed *)
  let op = CheckerMain.SealContract (oracle_addr, ctez_addr) in
  let _ops, wrapper = CheckerMain.main (op, wrapper) in (* sealed *)
  f wrapper

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

    (* Test views on sealed checker *)
    (* FIXME
        ("wrapper_view_buy_kit_min_kit_expected - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_buy_kit_min_kit_expected (Ctez.ctez_zero, init_wrapper));
        );

        ("wrapper_view_sell_kit_min_ctez_expected - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_sell_kit_min_ctez_expected (Kit.kit_zero, init_wrapper));
        );

        ("wrapper_view_add_liquidity_max_kit_deposited - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_add_liquidity_max_kit_deposited (Ctez.ctez_zero, init_wrapper));
        );

        ("wrapper_view_add_liquidity_min_lqt_minted - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_add_liquidity_min_lqt_minted (Ctez.ctez_zero, init_wrapper));
        );

        ("wrapper_view_remove_liquidity_min_ctez_withdrawn - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_remove_liquidity_min_ctez_withdrawn (Lqt.lqt_zero, init_wrapper));
        );

        ("wrapper_view_remove_liquidity_min_kit_withdrawn - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_remove_liquidity_min_kit_withdrawn (Lqt.lqt_zero, init_wrapper));
        );

        ("wrapper_view_current_liquidation_auction_minimum_bid - sealed" >::
         assert_unsealed_contract_raises_not_deployed_error
           (fun (init_wrapper) -> CheckerEntrypoints.wrapper_view_current_liquidation_auction_minimum_bid ((), init_wrapper));
        );
    *)

    ("wrapper_view_burrow_max_mintable_kit - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          let initial_amount = Ligo.add_tez_tez Constants.creation_deposit Constants.creation_deposit in
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:initial_amount;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (Ligo.nat_from_literal "0n", None)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
          assert_kit_equal
            ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "476_190n"))
            ~real:(CheckerEntrypoints.wrapper_view_burrow_max_mintable_kit ((bob_addr, Ligo.nat_from_literal "0n"), sealed_wrapper))
       )
    );

    ("wrapper_view_is_burrow_overburrowed - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:Constants.creation_deposit;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (Ligo.nat_from_literal "0n", None)))) in
          let _ops, sealed_wrapper = CheckerMain.main (op, sealed_wrapper) in
          assert_bool
            "burrow cannot be overburrowed already"
            (not (CheckerEntrypoints.wrapper_view_is_burrow_overburrowed ((bob_addr, Ligo.nat_from_literal "0n"), sealed_wrapper)))
       )
    );

    ("wrapper_view_is_burrow_liquidatable - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:Constants.creation_deposit;
          let op = CheckerMain.(CheckerEntrypoint (LazyParams (Create_burrow (Ligo.nat_from_literal "0n", None)))) in
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
            ~real:(CheckerEntrypoints.wrapper_view_get_balance ((bob_addr, Fa2Interface.lqt_token_id), sealed_wrapper))
       )
    );

    ("wrapper_view_total_supply - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_nat_equal
            ~expected:(Ligo.nat_from_literal "0n")
            ~real:(CheckerEntrypoints.wrapper_view_total_supply (Fa2Interface.kit_token_id, sealed_wrapper))
       )
    );

    ("wrapper_view_all_tokens - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_nat_list_equal
            ~expected:Fa2Interface.[kit_token_id; lqt_token_id]
            ~real:(CheckerEntrypoints.wrapper_view_all_tokens ((), sealed_wrapper))
       )
    );

    ("wrapper_view_is_operator - sealed" >::
     with_sealed_wrapper
       (fun sealed_wrapper ->
          assert_bool
            "no operators had been set"
            (not (CheckerEntrypoints.wrapper_view_is_operator ((bob_addr, (leena_addr, Fa2Interface.kit_token_id)), sealed_wrapper)))
       )
    );



    (* Add tests here *)
  ]
