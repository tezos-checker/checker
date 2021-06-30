open OUnit2
open TestLib
open CheckerMain

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


    ("SealContract - no deployments" >::
     fun _ ->
       let checker_deployer = leena_addr in
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let wrapper = CheckerMain.initial_wrapper checker_deployer in (* unsealed *)
       let ctez_addr = Ligo.address_of_string "ctez_addr" in
       let oracle_addr = Ligo.address_of_string "oracle_addr" in
       let op = CheckerMain.SealContract (oracle_addr, ctez_addr) in
       let _ops, _wrapper = CheckerMain.main (op, wrapper) in (* sealed *)
       assert_bool "yay!" true;

       ()
    );

    (* Add tests here *)
  ]
