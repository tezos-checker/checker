open OUnit2
open TestLib
open CheckerMain
open Error

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let with_sealed_wrapper f =
  fun _ ->

  let checker_deployer = leena_addr in
  Ligo.Tezos.reset ();
  Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:checker_deployer ~amount:(Ligo.tez_from_literal "0mutez");

  let wrapper = CheckerMain.initial_wrapper checker_deployer in (* unsealed *)
  let ctez_addr = Ligo.address_of_string "ctez_addr" in
  let oracle_addr = Ligo.address_of_string "oracle_addr" in
  let op = CheckerMain.SealContract (oracle_addr, ctez_addr) in
  let _ops, wrapper = CheckerMain.main (op, wrapper) in (* sealed *)
  f wrapper

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

    (* Failing cases when checker is not sealed yet *)
    ("If checker is not sealed, only the deployer should be able to work with it" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let wrapper = CheckerMain.initial_wrapper leena_addr in (* unsealed *)
       let ctez_addr = Ligo.address_of_string "ctez_addr" in
       let oracle_addr = Ligo.address_of_string "oracle_addr" in

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
         let op =
           let ctez_addr = Ligo.address_of_string "ctez_addr" in
           let oracle_addr = Ligo.address_of_string "oracle_addr" in
           CheckerMain.SealContract (oracle_addr, ctez_addr) in
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
