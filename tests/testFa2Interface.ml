open OUnit2
open TestCommon
open Fa2Interface

type just_boolean = bool
[@@deriving show]

let suite =
  "Fa2Interface tests" >::: [
    (* ************************************************************************* *)
    (**                    fa2_run_update_operators tests                        *)
    (* ************************************************************************* *)
    ("fa2_run_update_operators - Add_operator - succeeds from owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op =
         Add_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in

       let _fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in
       ()
    );

    ("fa2_run_update_operators - Add_operator - fails from non-owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op =
         Add_operator {
           owner = bob_addr;
           operator = leena_addr;
           token_id = kit_token_id;
         } in

       assert_raises
         (Failure "FA2_NOT_OWNER")
         (fun () -> fa2_run_update_operators (fa2_state, [update_op]))
    );

    ("fa2_run_update_operators - Remove_operator - succeeds from owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op =
         Remove_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in

       let _fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in
       ()
    );

    ("fa2_run_update_operators - Remove_operator - fails from non-owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op =
         Remove_operator {
           owner = bob_addr;
           operator = leena_addr;
           token_id = kit_token_id;
         } in

       assert_raises
         (Failure "FA2_NOT_OWNER")
         (fun () -> fa2_run_update_operators (fa2_state, [update_op]))
    );

    ("fa2_run_update_operators - Add/Remove_operator - add/remove the operator as expected" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op =
         Add_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in

       assert_bool
         "Test potency: starting operators"
         (not (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id)));

       let fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in

       assert_bool
         "Add_operator did not add the operator"
         (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id));

       let update_op =
         Remove_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in

       let fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in

       assert_bool
         "Remove_operator did not remove the operator"
         (not (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id)));

       ()
    );

    ("fa2_run_update_operators - list should be executed from left to right" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let add_operator_op =
         Add_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in
       let remove_operator_op =
         Remove_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in

       assert_bool
         "Test potency: starting operators"
         (not (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id)));

       (* Adding first and removing afterwards should leave bob without privileges. *)
       let fa2_state = fa2_run_update_operators (fa2_state, [add_operator_op; remove_operator_op;]) in

       assert_bool
         "Add/Remove should leave no privileges"
         (not (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id)));

       (* Removing first and adding afterwards should leave bob with privileges. *)
       let fa2_state = fa2_run_update_operators (fa2_state, [remove_operator_op; add_operator_op;]) in

       assert_bool
         "Remove/Add should leave privileges"
         (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id));

       ()
    );

    (* ************************************************************************* *)
    (**                         fa2_run_transfer tests                           *)
    (* ************************************************************************* *)

    ("fa2_run_transfer - succeeds from owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let tx =
         { from_ = alice_addr;
           txs =
             [ { to_ = bob_addr;
                 token_id = kit_token_id;
                 amount = Ligo.nat_from_literal "0n";
               }
             ];
         } in
       let _fa2_state = fa2_run_transfer (fa2_state, [tx]) in
       ()
    );

    ("fa2_run_transfer - fails from non-owner and not allowed operator" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let tx =
         { from_ = alice_addr;
           txs =
             [ { to_ = bob_addr;
                 token_id = kit_token_id;
                 amount = Ligo.nat_from_literal "0n";
               }
             ];
         } in

       assert_raises
         (Failure "FA2_NOT_OPERATOR")
         (fun () -> fa2_run_transfer (fa2_state, [tx]))
    );

    ("fa2_run_transfer - succeeds from non-owner but allowed operator" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op =
         Add_operator {
           owner = alice_addr;
           operator = bob_addr;
           token_id = kit_token_id;
         } in

       let fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let tx =
         { from_ = alice_addr;
           txs =
             [ { to_ = leena_addr;
                 token_id = kit_token_id;
                 amount = Ligo.nat_from_literal "0n";
               }
             ];
         } in
       let _fa2_state = fa2_run_transfer (fa2_state, [tx]) in
       ()
    );

    (* TODO: test that the order of execution of operations is as expected (need more than on tx for that). *)
    (* TODO: test all ownership variations. *)
    (* TODO: ensure all amounts change as expected as well. *)
    (* TODO: test the rest of the functions in fa2Interface.ml too. *)
  ]
