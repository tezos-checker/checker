open OUnit2

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let empty_wctez = Wctez.initial_wctez TestLib.ctok_fa12_addr

let suite =
  "Wctez tests" >::: [
    (* OFFLINE VIEW TESTS *)

    ("view_get_balance (FA2) - initial balance" >::
     fun _ ->
       Ligo.Tezos.reset ();
       TestLib.assert_nat_equal
         ~expected:(Ligo.nat_from_literal "0n")
         ~real:(Wctez.view_get_balance ((TestLib.alice_addr, TokenMetadata.wctez_token_id), empty_wctez))
    );

    ("view_get_balance (FA2) - undefined token ids" >::
     fun _ ->
       List.iter
         (fun token_id ->
            assert_raises
              (Failure "FA2_TOKEN_UNDEFINED")
              (fun () -> Wctez.view_get_balance ((TestLib.alice_addr, token_id), empty_wctez))
         )
         TokenMetadata.[ kit_token_id; lqt_token_id; wtez_token_id; mock_fa2_token_id; ]
    );

    ("view_total_supply (FA2) - initial wctez supply" >::
     fun _ ->
       Ligo.Tezos.reset ();
       TestLib.assert_nat_equal
         ~expected:(Ligo.nat_from_literal "0n")
         ~real:(Wctez.view_total_supply (TokenMetadata.wctez_token_id, empty_wctez))
    );

    ("view_total_supply (FA2) - undefined token ids" >::
     fun _ ->
       List.iter
         (fun token_id ->
            assert_raises
              (Failure "FA2_TOKEN_UNDEFINED")
              (fun () -> Wctez.view_total_supply (token_id, empty_wctez));
         )
         TokenMetadata.[ kit_token_id; lqt_token_id; wtez_token_id; mock_fa2_token_id; ]
    );

    ("view_all_tokens (FA2)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       TestLib.assert_nat_list_equal
         ~expected:[ TokenMetadata.wctez_token_id; ]
         ~real:(Wctez.view_all_tokens ((), empty_wctez))
    );

    (* FIXME: Add tests for view_is_operator. *)
    (* FIXME: Add tests for entrypoints. *)
  ]

let () =
  run_test_tt_main
    suite
