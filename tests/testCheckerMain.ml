open OUnit2
open TestCommon
open CheckerMain

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let suite =
  "CheckerMainTests" >::: [

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

    (* Add tests here *)
  ]
