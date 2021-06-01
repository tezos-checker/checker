open OUnit2
open TestCommon
open Fa2Interface

type just_boolean = bool
[@@deriving show]

let suite =
  "Fa2Interface tests" >::: [
    (* fa2_run_transfer tests *)

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
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez"); (* TODO: try with a different sender; should fail *)

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
