open OUnit2
open TestCommon
open TestArbitrary
open Fa2Interface
open Kit

let property_test_count = 100

(* Utility functions *)
let ask_kit_of fa2_state addr = kit_of_mukit (fa2_get_balance (fa2_state, addr, kit_token_id))
let ask_lqt_of fa2_state addr = fa2_get_balance (fa2_state, addr, liquidity_token_id)

let mk_kit_tx ~from_ ~to_ ~amount =
  { from_ = from_;
    txs =
      [ { to_ = to_;
          token_id = kit_token_id;
          amount = kit_to_mukit_nat amount;
        }
      ];
  }

let add_kit_operator ~owner ~operator =
  Add_operator {
    owner = owner;
    operator = operator;
    token_id = kit_token_id;
  }

let remove_kit_operator ~owner ~operator =
  Remove_operator {
    owner = owner;
    operator = operator;
    token_id = kit_token_id;
  }

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
       let update_op = add_kit_operator ~owner:alice_addr ~operator:bob_addr in
       let _fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in
       ()
    );

    ("fa2_run_update_operators - Add_operator - fails from non-owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op = add_kit_operator ~owner:bob_addr ~operator:leena_addr in
       assert_raises
         (Failure "FA2_NOT_OWNER")
         (fun () -> fa2_run_update_operators (fa2_state, [update_op]))
    );

    ("fa2_run_update_operators - Remove_operator - succeeds from owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op = remove_kit_operator ~owner:alice_addr ~operator:bob_addr in

       let _fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in
       ()
    );

    ("fa2_run_update_operators - Remove_operator - fails from non-owner" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op = remove_kit_operator ~owner:bob_addr ~operator:leena_addr in

       assert_raises
         (Failure "FA2_NOT_OWNER")
         (fun () -> fa2_run_update_operators (fa2_state, [update_op]))
    );

    ("fa2_run_update_operators - Add/Remove_operator - add/remove the operator as expected" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op = add_kit_operator ~owner:alice_addr ~operator:bob_addr in

       assert_bool
         "Test potency: starting operators"
         (not (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id)));

       let fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in

       assert_bool
         "Add_operator did not add the operator"
         (fa2_is_operator (fa2_state, bob_addr, alice_addr, kit_token_id));

       let update_op = remove_kit_operator ~owner:alice_addr ~operator:bob_addr in
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
       let add_operator_op = add_kit_operator ~owner:alice_addr ~operator:bob_addr in
       let remove_operator_op = remove_kit_operator ~owner:alice_addr ~operator:bob_addr in

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
    (**                              ledger_*_kit tests                              *)
    (* ************************************************************************* *)
    (* TODO: ledger_*_kit functions do not perform any permission check. This
     * we can only check at a higher level, probably with e2e tests. *)

    ("ledger_issue_kit/ledger_withdraw_kit issues/withdraws expected kit" >::
     fun _ ->
       Ligo.Tezos.reset ();

       (* Leena's account initially empty. *)
       let fa2_state_in = initial_fa2_state in
       assert_kit_equal
         ~expected:kit_zero
         ~real:(ask_kit_of fa2_state_in leena_addr);

       (* Withdrawing an empty amount from an empty account should succeed just fine. *)
       let kit_amount = kit_zero in
       let fa2_state_out = ledger_withdraw_kit (fa2_state_in, leena_addr, kit_amount) in
       assert_kit_equal
         ~expected:(ask_kit_of fa2_state_in leena_addr)
         ~real:(ask_kit_of fa2_state_out leena_addr);

       (* Withdrawing a non-empty amount from an empty account should fail. *)
       let fa2_state_in = fa2_state_out in
       assert_raises
         (Failure "FA2_INSUFFICIENT_BALANCE")
         (fun () ->
            let kit_amount = kit_of_mukit (Ligo.nat_from_literal "1n") in
            ledger_withdraw_kit (fa2_state_in, leena_addr, kit_amount)
         );

       (* Issue an amount, starting from zero. *)
       let fa2_state_in = fa2_state_out in
       let kit_amount = kit_of_mukit (Ligo.nat_from_literal "123_456n") in
       let fa2_state_out = ledger_issue_kit (fa2_state_in, leena_addr, kit_amount) in
       assert_kit_equal
         ~expected:(kit_add (ask_kit_of fa2_state_in leena_addr) kit_amount)
         ~real:(ask_kit_of fa2_state_out leena_addr);

       (* Issue an amount, starting from non-zero. *)
       let fa2_state_in = fa2_state_out in
       let kit_amount = kit_of_mukit (Ligo.nat_from_literal "789_012_345n") in
       let fa2_state_out = ledger_issue_kit (fa2_state_in, leena_addr, kit_amount) in
       assert_kit_equal
         ~expected:(kit_add (ask_kit_of fa2_state_in leena_addr) kit_amount)
         ~real:(ask_kit_of fa2_state_out leena_addr);

       (* Withdrawing more than the entire amount should fail. *)
       let fa2_state_in = fa2_state_out in
       assert_raises
         (Failure "FA2_INSUFFICIENT_BALANCE")
         (fun () ->
            let kit_amount = kit_add (ask_kit_of fa2_state_in leena_addr) (kit_of_mukit (Ligo.nat_from_literal "1n")) in
            ledger_withdraw_kit (fa2_state_in, leena_addr, kit_amount)
         );

       (* Withdrawing less than entire amount should succeed. *)
       let fa2_state_in = fa2_state_out in
       let kit_amount = kit_sub (ask_kit_of fa2_state_in leena_addr) (kit_of_mukit (Ligo.nat_from_literal "1_234n")) in
       let fa2_state_out = ledger_withdraw_kit (fa2_state_in, leena_addr, kit_amount) in
       assert_kit_equal
         ~expected:(ask_kit_of fa2_state_in leena_addr)
         ~real:(kit_add (ask_kit_of fa2_state_out leena_addr) kit_amount);

       (* Withdrawing the entire amount should succeed. *)
       let fa2_state_in = fa2_state_out in
       let kit_amount = ask_kit_of fa2_state_in leena_addr in
       let fa2_state_out = ledger_withdraw_kit (fa2_state_in, leena_addr, kit_amount) in
       assert_kit_equal
         ~expected:kit_zero
         ~real:(ask_kit_of fa2_state_out leena_addr);

       ()
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"ledger_issue_then_withdraw_kit should behave like an issue and then a withdraw"
        ~count:property_test_count
        (QCheck.pair arb_kit arb_kit)
      @@ fun (kit_1, kit_2) ->
        let kit_to_issue = kit_max kit_1 kit_2 in
        let kit_to_withdraw = kit_min kit_1 kit_2 in
        (* TODO: Check the other two cases: when they are equal and when kit_to_issue < kit_to_withdraw. *)

        Ligo.Tezos.reset ();
        let fa2_state_in = initial_fa2_state in
        let fa2_state_out_1 =
          ledger_withdraw_kit (ledger_issue_kit (fa2_state_in, leena_addr, kit_to_issue), leena_addr, kit_to_withdraw) in
        let fa2_state_out_2 =
          ledger_issue_then_withdraw_kit (fa2_state_in, leena_addr, kit_to_issue, kit_to_withdraw) in
        (ask_kit_of fa2_state_out_1 leena_addr = ask_kit_of fa2_state_out_2 leena_addr)
    );

    (* ************************************************************************* *)
    (**                        ledger_*_liquidity tests                          *)
    (* ************************************************************************* *)
    (* TODO: ledger_*_liquidity functions do not perform any permission check.
     * This we can only check at a higher level, probably with e2e tests. *)

    ("ledger_issue_liquidity/ledger_withdraw_liquidity issues/withdraws expected liquidity" >::
     fun _ ->
       Ligo.Tezos.reset ();

       (* Leena's account initially empty. *)
       let fa2_state_in = initial_fa2_state in
       assert_nat_equal
         ~expected:(Ligo.nat_from_literal "0n")
         ~real:(ask_lqt_of fa2_state_in leena_addr);

       (* Withdrawing an empty amount from an empty account should succeed just fine. *)
       let lqt_amount = (Ligo.nat_from_literal "0n") in
       let fa2_state_out = ledger_withdraw_liquidity (fa2_state_in, leena_addr, lqt_amount) in
       assert_nat_equal
         ~expected:(ask_lqt_of fa2_state_in leena_addr)
         ~real:(ask_lqt_of fa2_state_out leena_addr);

       (* Withdrawing a non-empty amount from an empty account should fail. *)
       let fa2_state_in = fa2_state_out in
       assert_raises
         (Failure "FA2_INSUFFICIENT_BALANCE")
         (fun () ->
            let lqt_amount = (Ligo.nat_from_literal "1n") in
            ledger_withdraw_liquidity (fa2_state_in, leena_addr, lqt_amount)
         );

       (* Issue an amount, starting from zero. *)
       let fa2_state_in = fa2_state_out in
       let lqt_amount = (Ligo.nat_from_literal "123_456n") in
       let fa2_state_out = ledger_issue_liquidity (fa2_state_in, leena_addr, lqt_amount) in
       assert_nat_equal
         ~expected:(Ligo.add_nat_nat (ask_lqt_of fa2_state_in leena_addr) lqt_amount)
         ~real:(ask_lqt_of fa2_state_out leena_addr);

       (* Issue an amount, starting from non-zero. *)
       let fa2_state_in = fa2_state_out in
       let lqt_amount = (Ligo.nat_from_literal "789_012_345n") in
       let fa2_state_out = ledger_issue_liquidity (fa2_state_in, leena_addr, lqt_amount) in
       assert_nat_equal
         ~expected:(Ligo.add_nat_nat (ask_lqt_of fa2_state_in leena_addr) lqt_amount)
         ~real:(ask_lqt_of fa2_state_out leena_addr);

       (* Withdrawing more than the entire amount should fail. *)
       let fa2_state_in = fa2_state_out in
       assert_raises
         (Failure "FA2_INSUFFICIENT_BALANCE")
         (fun () ->
            let lqt_amount = Ligo.add_nat_nat (ask_lqt_of fa2_state_in leena_addr) ((Ligo.nat_from_literal "1n")) in
            ledger_withdraw_liquidity (fa2_state_in, leena_addr, lqt_amount)
         );

       (* Withdrawing less than entire amount should succeed. *)
       let fa2_state_in = fa2_state_out in
       let lqt_amount =
         match Ligo.is_nat (Ligo.sub_nat_nat (ask_lqt_of fa2_state_in leena_addr) ((Ligo.nat_from_literal "1_234n"))) with
         | None -> assert_failure "impossible"
         | Some lqt -> lqt in
       let fa2_state_out = ledger_withdraw_liquidity (fa2_state_in, leena_addr, lqt_amount) in
       assert_nat_equal
         ~expected:(ask_lqt_of fa2_state_in leena_addr)
         ~real:(Ligo.add_nat_nat (ask_lqt_of fa2_state_out leena_addr) lqt_amount);

       (* Withdrawing the entire amount should succeed. *)
       let fa2_state_in = fa2_state_out in
       let lqt_amount = ask_lqt_of fa2_state_in leena_addr in
       let fa2_state_out = ledger_withdraw_liquidity (fa2_state_in, leena_addr, lqt_amount) in
       assert_nat_equal
         ~expected:(Ligo.nat_from_literal "0n")
         ~real:(ask_lqt_of fa2_state_out leena_addr);

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
       let tx = mk_kit_tx ~from_:alice_addr ~to_:bob_addr ~amount:kit_zero in
       let _fa2_state = fa2_run_transfer (fa2_state, [tx]) in
       ()
    );

    ("fa2_run_transfer - fails from non-owner and not allowed operator" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let tx = mk_kit_tx ~from_:alice_addr ~to_:bob_addr ~amount:kit_zero in
       assert_raises
         (Failure "FA2_NOT_OPERATOR")
         (fun () -> fa2_run_transfer (fa2_state, [tx]))
    );

    ("fa2_run_transfer - succeeds from non-owner but allowed operator" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let fa2_state = initial_fa2_state in
       let update_op = add_kit_operator ~owner:alice_addr ~operator:bob_addr in

       let fa2_state = fa2_run_update_operators (fa2_state, [update_op]) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let tx = mk_kit_tx ~from_:alice_addr ~to_:leena_addr ~amount:kit_zero in
       let _fa2_state = fa2_run_transfer (fa2_state, [tx]) in
       ()
    );

    ("fa2_run_transfer - transfers (kit) funds as expected" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let fa2_state_in = initial_fa2_state in

       (* All interesting accounts initially empty. *)
       assert_kit_equal ~expected:kit_zero ~real:(ask_kit_of fa2_state_in leena_addr);
       assert_kit_equal ~expected:kit_zero ~real:(ask_kit_of fa2_state_in bob_addr);
       assert_kit_equal ~expected:kit_zero ~real:(ask_kit_of fa2_state_in alice_addr);

       (* Populate all accounts with different amounts. *)
       let fa2_state_in =
         let fa2_state_in = ledger_issue_kit (fa2_state_in, leena_addr, kit_of_mukit (Ligo.nat_from_literal "7_000_000n")) in
         let fa2_state_in = ledger_issue_kit (fa2_state_in, bob_addr,   kit_of_mukit (Ligo.nat_from_literal "8_000_000n")) in
         let fa2_state_in = ledger_issue_kit (fa2_state_in, alice_addr, kit_of_mukit (Ligo.nat_from_literal "9_000_000n")) in
         fa2_state_in in

       (* Ensure all accounts have the expected kit in them. *)
       assert_kit_equal ~expected:(kit_of_mukit (Ligo.nat_from_literal "7_000_000n")) ~real:(ask_kit_of fa2_state_in leena_addr);
       assert_kit_equal ~expected:(kit_of_mukit (Ligo.nat_from_literal "8_000_000n")) ~real:(ask_kit_of fa2_state_in bob_addr);
       assert_kit_equal ~expected:(kit_of_mukit (Ligo.nat_from_literal "9_000_000n")) ~real:(ask_kit_of fa2_state_in alice_addr);

       (* Scenario 1: Alice sends funds to leena directly. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let amount = kit_of_mukit (Ligo.nat_from_literal "123_456n") in
       let tx = mk_kit_tx ~from_:alice_addr ~to_:leena_addr ~amount:amount in
       let fa2_state_out = fa2_run_transfer (fa2_state_in, [tx]) in

       assert_kit_equal (* leena's amount goes up *)
         ~expected:(kit_add (ask_kit_of fa2_state_in leena_addr) amount)
         ~real:(ask_kit_of fa2_state_out leena_addr);
       assert_kit_equal (* alice's amount goes down *)
         ~expected:(ask_kit_of fa2_state_in alice_addr)
         ~real:(kit_add (ask_kit_of fa2_state_out alice_addr) amount);
       assert_nat_equal (* total amount of kit stays the same *)
         ~expected:(fa2_get_total_kit_balance fa2_state_in)
         ~real:(fa2_get_total_kit_balance fa2_state_out);

       (* Scenario 2: Leena authorizes alice to send funds; alice sends funds to bob. *)
       let fa2_state_in = fa2_state_out in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let update_op = add_kit_operator ~owner:leena_addr ~operator:alice_addr in
       let fa2_state_in = fa2_run_update_operators (fa2_state_in, [update_op]) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let amount = kit_of_mukit (Ligo.nat_from_literal "1_500_000n") in
       let tx = mk_kit_tx ~from_:leena_addr ~to_:bob_addr ~amount:amount in
       let fa2_state_out = fa2_run_transfer (fa2_state_in, [tx]) in

       assert_kit_equal (* leena's amount goes down *)
         ~expected:(ask_kit_of fa2_state_in leena_addr)
         ~real:(kit_add (ask_kit_of fa2_state_out leena_addr) amount);
       assert_kit_equal (* bob's amount goes up *)
         ~expected:(kit_add (ask_kit_of fa2_state_in bob_addr) amount)
         ~real:(ask_kit_of fa2_state_out bob_addr);
       assert_nat_equal (* total amount of kit stays the same *)
         ~expected:(fa2_get_total_kit_balance fa2_state_in)
         ~real:(fa2_get_total_kit_balance fa2_state_out);

       (* Scenario 3: Alice is authorized by leena to send funds; alice sends
        * funds first from leena to alice, and then from alice to bob. If the
        * order of execution is not correct, the numbers are high enough for
        * this operation to fail. *)
       let fa2_state_in = fa2_state_out in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let amount1 = kit_of_mukit (Ligo.nat_from_literal "5_623_456n") in
       let tx1 = mk_kit_tx ~from_:leena_addr ~to_:alice_addr ~amount:amount1 in
       let amount2 = kit_of_mukit (Ligo.nat_from_literal "14_500_000n") in
       let tx2 = mk_kit_tx ~from_:alice_addr ~to_:bob_addr ~amount:amount2 in

       (* The numbers are too high for the operations to be executed in the wrong order. *)
       assert_raises
         (Failure "FA2_INSUFFICIENT_BALANCE")
         (fun () -> fa2_run_transfer (fa2_state_in, [tx2; tx1]));

       let fa2_state_out = fa2_run_transfer (fa2_state_in, [tx1; tx2]) in

       assert_kit_equal (* leena's amount goes down by amount1 *)
         ~expected:(ask_kit_of fa2_state_in leena_addr)
         ~real:(kit_add (ask_kit_of fa2_state_out leena_addr) amount1);
       assert_kit_equal (* alice's amount goes up by amount1 by down by amount2 *)
         ~expected:(kit_add (ask_kit_of fa2_state_in alice_addr) amount1)
         ~real:(kit_add (ask_kit_of fa2_state_out alice_addr) amount2);
       assert_kit_equal (* bob's amount goes up by amount2 *)
         ~expected:(kit_add (ask_kit_of fa2_state_in bob_addr) amount2)
         ~real:(ask_kit_of fa2_state_out bob_addr);
       assert_nat_equal (* total amount of kit stays the same *)
         ~expected:(fa2_get_total_kit_balance fa2_state_in)
         ~real:(fa2_get_total_kit_balance fa2_state_out);

       ()
    );

    (* TODO: test that the order of execution of operations is as expected (need more than on tx for that). *)
    (* TODO: test all ownership variations. *)
    (* TODO: ensure all amounts change as expected as well. *)
    (* TODO: test the rest of the functions in fa2Interface.ml too. *)

    (* TODO: test fa2_get_balance *)
    (* TODO: test fa2_run_balance_of *)

    (* FIXME: Not sure how to always make sure that fa2_all_tokens lists all
     * valid tokens. Perhaps with an external script, like we do to check that
     * there are no duplicate error codes? *)
  ]
