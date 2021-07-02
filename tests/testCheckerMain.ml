open OUnit2
open TestLib
open CheckerMain
open Error

(* NOTE: The tests in this file mainly try to cover all execution paths in
 * CheckerMain.main, but don't do much more. At this high a level most of the
 * actual code to be run is wrapped in {BEGIN/END}_LIGO, so I think e2e tests
 * are more appropriate for checking the actual outputs. *)

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

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


    (* Succeeding cases when checker is not sealed yet *)
    ("If checker is not sealed, the deployer should be able to call DeployFunction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let wrapper = CheckerMain.initial_wrapper leena_addr in (* unsealed *)

       let fn_id, fn_bytes = CheckerEntrypoints.(lazyParamsToLazyFunctionId (Create_burrow (Ligo.nat_from_literal "63n", Some charles_key_hash))) in
       let op = CheckerMain.DeployFunction (fn_id, fn_bytes) in

       (* This call should succeed (first time, no previous entry). *)
       let _ops, wrapper = CheckerMain.main (op, wrapper) in
       (* This call should also succeed (second time, concatenation). *)
       let _ops, _wrapper = CheckerMain.main (op, wrapper) in
       ()
    );

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
       let op = CheckerMain.SealContract (oracle_addr, ctez_addr) in
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

    (* Succeeding cases when checker is already sealed *)
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

    ("If checker is sealed, users should be able to call CheckerEntrypoint/LazyParams" >::
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
          let op = CheckerMain.SealContract (oracle_addr, ctez_addr) in
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

    (* Add tests here *)
  ]
