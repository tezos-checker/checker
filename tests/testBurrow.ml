open Error
open FixedPoint
open Kit
open Tok
open OUnit2
open TestLib

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let burrow_addr = Ligo.address_of_string "BURROW_ADDR"

(* Create a burrow from an amount of kit outstanding and tez collateral along with
 * an active flag. The rest of the parameters are fixed. *)
let make_test_burrow ~outstanding_kit ~collateral ~active = Burrow.make_burrow_for_test
    ~outstanding_kit:outstanding_kit
    ~active:active
    ~address:burrow_addr
    ~delegate:None
    ~collateral:collateral
    ~adjustment_index:fixedpoint_one
    ~collateral_at_auction:tok_zero
    ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0)

(* A burrow with fixed parameters which was last touched at 0s. Use for tests which check
 * that functions requiring a burrow to be recently touched fail as expected. *)
let burrow_for_needs_touch_tests = make_test_burrow
    ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "0n"))
    ~collateral:tok_zero
    ~active:true

let suite =
  "Burrow tests" >::: [
    ("burrow_burn_kit - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_burn_kit
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           burrow_for_needs_touch_tests
       in ()
    );

    ("burrow_burn_kit - burning exactly outstanding_kit returns burrow with expected outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:tok_zero in

       let kit_to_burn = kit_of_denomination (Ligo.nat_from_literal "1n") in
       let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn burrow0 in

       assert_kit_equal ~expected:kit_zero ~real:(Burrow.burrow_outstanding_kit burrow);
       assert_kit_equal ~expected:kit_to_burn ~real:actual_burned
    );

    ("burrow_burn_kit - burning greater than outstanding_kit returns burrow with expected outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:tok_zero in

       let kit_to_burn = kit_of_denomination (Ligo.nat_from_literal "2n") in
       let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn burrow0 in

       assert_kit_equal ~expected:kit_zero ~real:(Burrow.burrow_outstanding_kit burrow);
       assert_kit_equal ~expected:(kit_of_denomination (Ligo.nat_from_literal "1n")) ~real:actual_burned
    );

    ("burrow_burn_kit - burning less than outstanding_kit returns burrow with expected outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "2n"))
           ~active:true
           ~collateral:tok_zero in

       let kit_to_burn = kit_of_denomination (Ligo.nat_from_literal "1n") in
       let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn burrow0 in

       assert_kit_equal ~expected:(kit_of_denomination (Ligo.nat_from_literal "1n")) ~real:(Burrow.burrow_outstanding_kit burrow);
       assert_kit_equal ~expected:kit_to_burn ~real:actual_burned
    );

    ("burrow_burn_kit - burning zero kit returns burrow with expected outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:tok_zero in

       let kit_to_burn = kit_zero in
       let burrow, actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn burrow0 in

       assert_kit_equal ~expected:(kit_of_denomination (Ligo.nat_from_literal "1n")) ~real:(Burrow.burrow_outstanding_kit burrow);
       assert_kit_equal ~expected:kit_to_burn ~real:actual_burned
    );

    ("burrow_burn_kit - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:tok_zero in

       let burrow, _actual_burned = Burrow.burrow_burn_kit Parameters.initial_parameters (kit_of_denomination (Ligo.nat_from_literal "1n")) burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address burrow)
    );

    ("burrow_set_delegate - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_set_delegate
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           (Some charles_key_hash)
           burrow_for_needs_touch_tests
       in ()
    );

    ("burrow_set_delegate - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:tok_zero in

       let burrow = Burrow.burrow_set_delegate Parameters.initial_parameters (Some charles_key_hash) burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address burrow)
    );

    ("burrow_deposit_collateral - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_deposit_collateral
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           (tok_of_denomination (Ligo.nat_from_literal "1n"))
           burrow_for_needs_touch_tests
       in ()
    );

    ("burrow_deposit_collateral - burrow after successful deposit has expected collateral" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "100n")) in

       let burrow = Burrow.burrow_deposit_collateral
           Parameters.initial_parameters
           (tok_of_denomination (Ligo.nat_from_literal "1n"))
           burrow0 in

       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "101n"))
         ~real:(Burrow.burrow_collateral burrow)
    );

    ("burrow_deposit_collateral - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:tok_zero in

       let burrow = Burrow.burrow_deposit_collateral Parameters.initial_parameters (tok_of_denomination (Ligo.nat_from_literal "1n")) burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_withdraw_tez - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_withdraw_tez
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           tok_zero
           burrow_for_needs_touch_tests
       in ()
    );

    ("burrow_withdraw_tez - burrow after successful withdrawal has expected collateral" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "100n")) in

       let burrow = Burrow.burrow_withdraw_tez
           Parameters.initial_parameters
           (tok_of_denomination (Ligo.nat_from_literal "1n"))
           burrow0 in

       assert_tok_equal
         ~expected:(tok_of_denomination (Ligo.nat_from_literal "99n"))
         ~real:(Burrow.burrow_collateral burrow)
    );

    ("burrow_withdraw_tez - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n")) in

       let burrow = Burrow.burrow_withdraw_tez Parameters.initial_parameters (tok_of_denomination (Ligo.nat_from_literal "1n")) burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_withdraw_tez - fails if withdrawal would overburrow the burrow" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "4n"))
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "10n")) in

       assert_raises
         (Failure (Ligo.string_of_int error_WithdrawTezFailure))
         (fun () ->
            let _ =
              Burrow.burrow_withdraw_tez
                {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
                (tok_of_denomination (Ligo.nat_from_literal "2n"))
                burrow0
            in ()
         )
    );

    ("burrow_mint_kit - burrow after successful minting has expected collateral" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "100n")) in

       let burrow = Burrow.burrow_mint_kit
           Parameters.initial_parameters
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           burrow0 in

       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "2n"))
         ~real:(Burrow.burrow_outstanding_kit burrow)
    );

    ("burrow_mint_kit - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_mint_kit
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           (kit_of_denomination (Ligo.nat_from_literal "0n"))
           burrow_for_needs_touch_tests
       in ()
    );

    ("burrow_mint_kit - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "4n")) in

       let burrow = Burrow.burrow_mint_kit Parameters.initial_parameters (kit_of_denomination (Ligo.nat_from_literal "1n")) burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_mint_kit - minting burrow_max_mintable_kit succeeds" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "12_345_678_904n")) in

       let burrow_max_mintable_kit = Burrow.burrow_max_mintable_kit Parameters.initial_parameters burrow0 in
       let burrow = Burrow.burrow_mint_kit Parameters.initial_parameters burrow_max_mintable_kit burrow0 in

       assert_kit_equal
         ~expected:burrow_max_mintable_kit
         ~real:(Burrow.burrow_outstanding_kit burrow)
    );

    ("burrow_mint_kit - minting more than burrow_max_mintable_kit fails" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_MintKitFailure))
         (fun () ->
            let burrow0 = make_test_burrow
                ~outstanding_kit:kit_zero
                ~active:true
                ~collateral:(tok_of_denomination (Ligo.nat_from_literal "12_345_678_904n")) in

            let burrow_max_mintable_kit = Burrow.burrow_max_mintable_kit Parameters.initial_parameters burrow0 in
            let just_over_max_mintable_kit = Kit.kit_add burrow_max_mintable_kit (kit_of_denomination (Ligo.nat_from_literal "1n")) in
            Burrow.burrow_mint_kit Parameters.initial_parameters just_over_max_mintable_kit burrow0
         )
    );

    ("burrow_activate - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let burrow, _ =
         Burrow.burrow_deactivate Parameters.initial_parameters burrow_for_needs_touch_tests in
       let _ =
         Burrow.burrow_activate
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           Constants.creation_deposit
           burrow
       in ()
    );

    ("burrow_activate - fails for a burrow which is already active" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_BurrowIsAlreadyActive))
         (fun () ->
            Burrow.burrow_activate
              Parameters.initial_parameters
              Constants.creation_deposit
              (make_test_burrow
                 ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
                 ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n"))
                 ~active:true
              )
         )
    );

    ("burrow_activate - fails when one less tez than creation deposit provided" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_InsufficientFunds))
         (fun () ->
            Burrow.burrow_activate
              Parameters.initial_parameters
              (tok_sub Constants.creation_deposit (tok_of_denomination (Ligo.nat_from_literal "1n")))
              (make_test_burrow
                 ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
                 ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n"))
                 ~active:true
              )
         )
    );

    ("burrow_activate - passes with creation deposit tez for a burrow which is inactive" >::
     fun _ ->
       let burrow = Burrow.burrow_activate
           Parameters.initial_parameters
           Constants.creation_deposit
           (make_test_burrow
              ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
              ~collateral:tok_zero
              ~active:false
           )
       in
       assert_bool "burrow was not flagged as active after calling burrow_activate" (Burrow.burrow_active burrow)
    );

    ("burrow_activate - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:false
           ~collateral:tok_zero in

       let burrow = Burrow.burrow_activate Parameters.initial_parameters Constants.creation_deposit burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_deactivate - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_deactivate
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           burrow_for_needs_touch_tests
       in ()
    );

    ("burrow_deactivate - fails for a burrow which is already inactive" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_DeactivatingAnInactiveBurrow))
         (fun () ->
            Burrow.burrow_deactivate
              Parameters.initial_parameters
              (make_test_burrow
                 ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "0n"))
                 ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n"))
                 ~active:false
              )
         )
    );

    ("burrow_deactivate - fails for a burrow which has outstanding kit" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_DeactivatingWithOutstandingKit))
         (fun () ->
            Burrow.burrow_deactivate
              Parameters.initial_parameters
              (make_test_burrow
                 ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
                 ~collateral:(tok_of_denomination (Ligo.nat_from_literal "10n"))
                 ~active:true
              )
         )
    );

    ("burrow_deactivate - fails for a burrow which is overburrowed" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_DeactivatingAnOverburrowedBurrow))
         (fun () ->
            Burrow.burrow_deactivate
              Parameters.initial_parameters
              (make_test_burrow
                 ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "10n"))
                 ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n"))
                 ~active:true
              )
         )
    );

    ("burrow_deactivate - fails for a burrow which has collateral at auction" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_DeactivatingWithCollateralAtAuctions))
         (fun () ->
            Burrow.burrow_deactivate
              Parameters.initial_parameters
              (
                Burrow.make_burrow_for_test
                  ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "0n"))
                  ~active:true
                  ~address:burrow_addr
                  ~delegate:None
                  ~collateral:(tok_of_denomination (Ligo.nat_from_literal "10n"))
                  ~adjustment_index:fixedpoint_one
                  ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "1n"))
                  ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0)
              )
         )
    );

    ("burrow_deactivate - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:tok_zero in

       let burrow, _ = Burrow.burrow_deactivate Parameters.initial_parameters burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_request_liquidation - does not fail for a burrow which needs to be touched" >::
     fun _ ->
       let _ =
         Burrow.burrow_request_liquidation
           {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
           burrow_for_needs_touch_tests
       in ()
    );

    (* Note: this is a bit overkill but testing anyways since the address field is extremely important *)
    ("burrow_create - address matches provided address" >::
     fun _ ->
       let burrow = Burrow.burrow_create Parameters.initial_parameters burrow_addr Constants.creation_deposit None in

       assert_address_equal
         ~expected:burrow_addr
         ~real:(Burrow.burrow_address burrow)

    );

    (
      "burrow_create - created burrow has zero collateral at auction" >::
      fun _ ->
        let burrow = Burrow.burrow_create Parameters.initial_parameters burrow_addr Constants.creation_deposit None in

        assert_tok_equal
          ~expected:tok_zero
          ~real:(Burrow.burrow_collateral_at_auction burrow)
    );

    (
      "burrow_create - created burrow has zero outstanding kit" >::
      fun _ ->
        let burrow = Burrow.burrow_create Parameters.initial_parameters burrow_addr Constants.creation_deposit None in

        assert_kit_equal
          ~expected:kit_zero
          ~real:(Burrow.burrow_outstanding_kit burrow)
    );

    ("burrow_touch - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:tok_zero in
       let parameters = {
         Parameters.initial_parameters
         with last_touched=(Ligo.timestamp_from_seconds_literal 1)
       } in

       let burrow = Burrow.burrow_touch parameters burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_return_kit_from_auction - does not change burrow address" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:tok_zero
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "1n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
       let slice = LiquidationAuctionPrimitiveTypes.{
           burrow=(Ligo.address_from_literal "12345", Ligo.nat_from_literal "0n");
           tok=tok_of_denomination (Ligo.nat_from_literal "1n");
           min_kit_for_unwarranted = Some (kit_of_denomination (Ligo.nat_from_literal "1n"));
         } in

       let burrow, _returned_kit, _excess_kit = Burrow.burrow_return_kit_from_auction slice (kit_of_denomination (Ligo.nat_from_literal "1n")) burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("burrow_return_slice_from_auction - expected value" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "2n"))
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "2n"))
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
       let slice = let open LiquidationAuctionPrimitiveTypes in {
           burrow=(burrow_addr, Ligo.nat_from_literal "0n");
           tok=tok_of_denomination (Ligo.nat_from_literal "1n");
           min_kit_for_unwarranted = Some (kit_of_denomination (Ligo.nat_from_literal "1n"));
         } in

       let expected_burrow = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "2n"))
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "3n"))
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "2n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
       let burrow = Burrow.burrow_return_slice_from_auction slice burrow0 in

       assert_burrow_equal
         ~expected:expected_burrow
         ~real:burrow
    );

    ("burrow_return_slice_from_auction - does not change burrow address" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:tok_zero
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "1n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
       let slice = let open LiquidationAuctionPrimitiveTypes in {
           burrow=(Ligo.address_from_literal "12345", Ligo.nat_from_literal "0n");
           tok=tok_of_denomination (Ligo.nat_from_literal "1n");
           min_kit_for_unwarranted = Some (kit_of_denomination (Ligo.nat_from_literal "1n"));
         } in

       let burrow = Burrow.burrow_return_slice_from_auction slice burrow0 in

       assert_address_equal
         ~expected:(Burrow.burrow_address burrow0)
         ~real:(Burrow.burrow_address  burrow)
    );

    ("compute_min_kit_for_unwarranted - burrow with zero collateral and zero outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:tok_zero in
       let collateral_to_auction = tok_of_denomination (Ligo.nat_from_literal "1n") in
       let min_kit_for_unwarranted = Burrow.compute_min_kit_for_unwarranted Parameters.initial_parameters burrow0 collateral_to_auction in

       assert_kit_option_equal
         ~expected:(Some kit_zero)
         ~real:min_kit_for_unwarranted
    );

    ("compute_min_kit_for_unwarranted - burrow with zero collateral and positive outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:tok_zero in
       let collateral_to_auction = tok_of_denomination (Ligo.nat_from_literal "1n") in
       let min_kit_for_unwarranted = Burrow.compute_min_kit_for_unwarranted Parameters.initial_parameters burrow0 collateral_to_auction in

       assert_kit_option_equal
         ~expected:None
         ~real:min_kit_for_unwarranted
    );

    ("compute_min_kit_for_unwarranted - burrow with positive collateral and zero outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n")) in
       let collateral_to_auction = tok_of_denomination (Ligo.nat_from_literal "1n") in
       let min_kit_for_unwarranted = Burrow.compute_min_kit_for_unwarranted Parameters.initial_parameters burrow0 collateral_to_auction in

       assert_kit_option_equal
         ~expected:(Some kit_zero)
         ~real:min_kit_for_unwarranted
    );

    ("compute_min_kit_for_unwarranted - burrow with positive collateral and positive outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n")) in
       let collateral_to_auction = tok_of_denomination (Ligo.nat_from_literal "1n") in
       let min_kit_for_unwarranted = Burrow.compute_min_kit_for_unwarranted Parameters.initial_parameters burrow0 collateral_to_auction in

       assert_kit_option_equal
         ~expected:(Some (kit_of_denomination (Ligo.nat_from_literal "2n")))
         ~real:min_kit_for_unwarranted
    );

    ("compute_min_kit_for_unwarranted - burrow with positive collateral and collateral at auction and positive outstanding kit" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "5n"))
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "2n"))
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "2n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
       let collateral_to_auction = tok_of_denomination (Ligo.nat_from_literal "2n") in
       let min_kit_for_unwarranted = Burrow.compute_min_kit_for_unwarranted Parameters.initial_parameters burrow0 collateral_to_auction in

       assert_kit_option_equal
         ~expected:(Some (kit_of_denomination (Ligo.nat_from_literal "7n")))
         ~real:min_kit_for_unwarranted
    );

    (* This is a bit of an odd test but it ensures that the math in compute_collateral_to_auction
       won't throw an exception if the constants are ever reconfigured in this way.*)
    ("compute_collateral_to_auction - constants obey assumption in implementation" >::
     fun _ ->
       let open Common in
       let {num=f_num; den=f_den} = Constants.fminting in
       let {num=lp_num; den=lp_den} = Constants.liquidation_penalty in

       assert_bool
         ("fminting and liquidation_penalty must be configured such that" ^
          "((1 - liquidation_penalty) * fminting - 1) > 0 in order for the assumptions in " ^
          "compute_collateral_to_auction to hold")
         (Ligo.gt_int_int
            (Ligo.mul_int_int
               f_num
               (Ligo.sub_int_int lp_den lp_num))
            (Ligo.mul_int_int lp_den f_den))
    );

    ("compute_collateral_to_auction - expected value for an overburrowed burrow" >::
     fun _ ->
       let burrow = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "3n"))
           ~active:true
           ~address:alice_addr
           ~delegate:None
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "3n"))
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
       (* Note: cranking up the index to make test more sensitive to small changes
        *  potentially obscured by rounding. This high of a difference between the
        *  index and protected index is unlikely to occur in real-world scenarios.
       *)
       let parameters = Parameters.({initial_parameters with index=(Ligo.nat_from_literal "100_000_000_000n");}) in
       assert_int_equal
         ~expected:(Ligo.int_from_literal "707856")
         ~real:(Burrow.compute_collateral_to_auction parameters burrow)
    );

    ("compute_collateral_to_auction - expected value for an underburrowed burrow" >::
     fun _ ->
       let burrow = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "3n"))
           ~active:true
           ~address:alice_addr
           ~delegate:None
           ~collateral:(tok_of_denomination (Ligo.nat_from_literal "3n"))
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3n"))
           ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

       assert_int_equal
         ~expected:(Ligo.int_from_literal "-2")
         ~real:(Burrow.compute_collateral_to_auction Parameters.initial_parameters burrow)
    );

    (
      "burrow_return_kit_from_auction - expected value for burrow with outstanding kit" >::
      fun _ -> (
          let burrow = Burrow.make_burrow_for_test
              ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "10n"))
              ~active:true
              ~address:alice_addr
              ~delegate:None
              ~collateral:tok_zero
              ~adjustment_index:fixedpoint_one
              ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3n"))
              ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

          let slice = let open LiquidationAuctionPrimitiveTypes in
            {
              burrow=(alice_addr, Ligo.nat_from_literal "0n");
              tok=(tok_of_denomination (Ligo.nat_from_literal "2n"));
              min_kit_for_unwarranted=None;
            }
          in

          let burrow_out, _returned_kit, _excess_kit = Burrow.burrow_return_kit_from_auction slice (kit_of_denomination (Ligo.nat_from_literal "9n")) burrow in
          let burrow_expected = Burrow.make_burrow_for_test
              ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
              ~active:true
              ~address:alice_addr
              ~delegate:None
              ~collateral:tok_zero
              ~adjustment_index:fixedpoint_one
              ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "1n"))
              ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

          assert_burrow_equal
            ~expected:burrow_expected
            ~real:burrow_out
        ));

    (
      "burrow_is_cancellation_warranted - warranted cancellation" >::
      fun _ -> (
          let burrow = Burrow.make_burrow_for_test
              ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "4_657_142n"))
              ~active:true
              ~address:alice_addr
              ~delegate:None
              ~collateral:(tok_of_denomination (Ligo.nat_from_literal "5_000_000n"))
              ~adjustment_index:fixedpoint_one
              ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))
              ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
          let cancelled_slice_tok = tok_of_denomination (Ligo.nat_from_literal "1_000_000n") in

          assert_bool
            "burrow_is_cancellation_warranted returned false, but the cancellation is expected to be warranted"
            (Burrow.burrow_is_cancellation_warranted Parameters.initial_parameters burrow cancelled_slice_tok))
    );

    (
      "burrow_is_cancellation_warranted - unwarranted cancellation" >::
      fun _ ->
        let burrow = Burrow.make_burrow_for_test
            ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "4_657_143n"))
            ~active:true
            ~address:alice_addr
            ~delegate:None
            ~collateral:(tok_of_denomination (Ligo.nat_from_literal "5_000_000n"))
            ~adjustment_index:fixedpoint_one
            ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))
            ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
        let cancelled_slice_tok = tok_of_denomination (Ligo.nat_from_literal "1_000_000n") in

        assert_bool
          "burrow_is_cancellation_warranted returned true, but the cancellation is expected to be unwarranted"
          (not (Burrow.burrow_is_cancellation_warranted Parameters.initial_parameters burrow cancelled_slice_tok))
    );

    (
      "burrow_is_liquidatable - liquidatable burrow" >::
      fun _ ->
        let burrow = Burrow.make_burrow_for_test
            ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "4_000_000n"))
            ~active:true
            ~address:alice_addr
            ~delegate:None
            ~collateral:(tok_of_denomination (Ligo.nat_from_literal "2_469_999n"))
            ~adjustment_index:fixedpoint_one
            ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))
            ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

        assert_bool
          "burrow_is_liquidatable returned false, but the burrow is expected to be liquidatable"
          (Burrow.burrow_is_liquidatable Parameters.initial_parameters burrow)
    );

    (
      "burrow_is_liquidatable - non-liquidatable burrow" >::
      fun _ ->
        let burrow = Burrow.make_burrow_for_test
            ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "4_000_000n"))
            ~active:true
            ~address:alice_addr
            ~delegate:None
            ~collateral:(tok_of_denomination (Ligo.nat_from_literal "2_470_000n"))
            ~adjustment_index:fixedpoint_one
            ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))
            ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

        assert_bool
          "burrow_is_liquidatable returned true, but the burrow is expected to be non-liquidatable"
          (not (Burrow.burrow_is_liquidatable Parameters.initial_parameters burrow))
    );

    (
      "burrow_total_associated_tok - active burrow" >::
      fun _ ->
        let burrow = Burrow.make_burrow_for_test
            ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1_000_000n"))
            ~active:true
            ~address:alice_addr
            ~delegate:None
            ~collateral:(tok_of_denomination (Ligo.nat_from_literal "2_000_000n"))
            ~adjustment_index:fixedpoint_one
            ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))
            ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

        assert_tok_equal
          ~expected:(tok_of_denomination (Ligo.nat_from_literal "6_000_000n"))
          ~real:(Burrow.burrow_total_associated_tok burrow)
    );

    (
      "burrow_total_associated_tok - inactive burrow" >::
      fun _ ->
        let burrow = Burrow.make_burrow_for_test
            ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1_000_000n"))
            ~active:false
            ~address:alice_addr
            ~delegate:None
            ~collateral:(tok_of_denomination (Ligo.nat_from_literal "2_000_000n"))
            ~adjustment_index:fixedpoint_one
            ~collateral_at_auction:(tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))
            ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

        assert_tok_equal
          ~expected:(tok_of_denomination (Ligo.nat_from_literal "5_000_000n"))
          ~real:(Burrow.burrow_total_associated_tok burrow)
    );
    (* =========================================================================================== *)
    (* Property tests for ensuring methods don't allow a burrow to become overburrowed *)
    (* =========================================================================================== *)

    (
      let collateral = 100 in
      let outstanding_kit = 1 in
      (* As of writing, the below calculation simplifies to floor(collateral * 10/21) *)
      let burrowing_limit_kit = 47 in
      let min_mint_to_overburrow = burrowing_limit_kit - outstanding_kit + 1 in
      let arb_kit = QCheck.map (fun x -> kit_of_denomination (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(min_mint_to_overburrow -- max_int) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_mint_kit - fails when minting would cause the burrow to be overburrowed"
        ~count:property_test_count
        arb_kit
      @@ fun mint_kit ->
      let burrow = make_test_burrow
          ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal (string_of_int outstanding_kit ^ "n")))
          ~active:true
          ~collateral:(tok_of_denomination (Ligo.nat_from_literal ((string_of_int collateral) ^ "n"))) in

      assert_raises
        (Failure (Ligo.string_of_int error_MintKitFailure))
        (fun () -> Burrow.burrow_mint_kit Parameters.initial_parameters mint_kit burrow);
      true
    );

    (
      let collateral = 100 in
      let outstanding_kit = kit_of_denomination (Ligo.nat_from_literal ("1n")) in
      (* As of writing, the below calculation simplifies to ceil(outstanding_kit * 21/10) *)
      let burrowing_limit_tez = 3 in
      let min_withdrawal_to_overburrow = (collateral - burrowing_limit_tez) + 1 in
      let arb_tez = QCheck.map (fun x -> tok_of_denomination (Ligo.nat_from_literal ((string_of_int x) ^ "n"))) QCheck.(min_withdrawal_to_overburrow -- collateral) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_withdraw_tez - fails when the withdrawal would cause the burrow to be overburrowed"
        ~count:property_test_count
        arb_tez
      @@ fun tez_to_withdraw ->
      let burrow = make_test_burrow
          ~outstanding_kit:outstanding_kit
          ~active:true
          ~collateral:(tok_of_denomination (Ligo.nat_from_literal ((string_of_int collateral) ^ "n"))) in

      assert_raises
        (Failure (Ligo.string_of_int error_WithdrawTezFailure))
        (fun () -> Burrow.burrow_withdraw_tez Parameters.initial_parameters tez_to_withdraw burrow);
      true
    );

    (* =========================================================================================== *)
    (* Other property tests *)
    (* =========================================================================================== *)
    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_deposit_collateral - increases burrow collateral by exactly deposit amount"
        ~count:property_test_count
        TestArbitrary.arb_tok
      @@ fun tok_to_deposit ->
      let burrow0 = make_test_burrow
          ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "1n"))
          ~active:true
          ~collateral:(tok_of_denomination (Ligo.nat_from_literal "100n")) in

      let burrow = Burrow.burrow_deposit_collateral
          Parameters.initial_parameters
          tok_to_deposit
          burrow0 in

      assert_tok_equal
        ~expected:(tok_add (Burrow.burrow_collateral burrow0) tok_to_deposit)
        ~real:(Burrow.burrow_collateral burrow);
      true
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_deactivate - all collateral is extracted from burrow"
        ~count:property_test_count
        TestArbitrary.arb_tok
      @@ fun collateral_balance_tok ->
      let burrow0 = make_test_burrow
          ~outstanding_kit:(kit_of_denomination (Ligo.nat_from_literal "0n"))
          ~active:true
          ~collateral:collateral_balance_tok in

      let burrow, returned_tez = Burrow.burrow_deactivate
          Parameters.initial_parameters
          burrow0 in

      assert_tok_equal
        ~expected:tok_zero
        ~real:(Burrow.burrow_collateral burrow);
      assert_tok_equal
        ~expected:(tok_add Constants.creation_deposit collateral_balance_tok)
        ~real:returned_tez;
      true
    );

    (
      let arb_tok = QCheck.map (fun x -> tok_of_denomination (Ligo.nat_from_literal ((string_of_int x) ^ "n"))) QCheck.(1_000_000 -- max_int) in
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_deactivate / burrow_activate - no collateral lost in re-activation round-trip"
        ~count:property_test_count
        arb_tok
      @@ fun starting_collateral ->
      (* Start with an active burrow with some tez collateral *)
      let burrow0 = make_test_burrow
          ~outstanding_kit:kit_zero
          ~active:true
          ~collateral:starting_collateral in

      (* Deactivate the burrow *)
      let deactivated_burrow, tez = Burrow.burrow_deactivate Parameters.initial_parameters burrow0 in
      (* Reactivate it with the tez returned from deactivating it *)
      let burrow = Burrow.burrow_activate Parameters.initial_parameters tez deactivated_burrow in

      assert_tok_equal
        ~expected:starting_collateral
        ~real:(Burrow.burrow_collateral burrow);
      true
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_touch - net kit associated with burrow does not change when no adjustment is required"
        ~count:property_test_count
        (TestArbitrary.arb_kit)
      @@ fun (kit) ->

      (* Random kit balances which obey the burrow invariants *)
      let outstanding = kit in
      (* Note: this combination of burrow and parameters cause the adjustment to be just the identity *)
      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:outstanding
          ~active:true
          ~address:burrow_addr
          ~delegate:None
          ~collateral:(tok_of_denomination (Ligo.nat_from_literal "1n"))
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:tok_zero
          ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in
      let parameters = {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)} in

      let burrow = Burrow.burrow_touch parameters burrow0 in
      assert_bool
        "Net kit changed during touch operation even though no adjustment should have been performed."
        (Kit.eq_kit_kit (Burrow.burrow_outstanding_kit burrow) (Burrow.burrow_outstanding_kit burrow0));
      true
    );

    (
      (* Note: this test was written to catch cases in an implemention of compute_collateral_to_auction in which
         errors would be thrown for negative values. *)
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"compute_collateral_to_auction - does not throw exception for arbitrary inputs"
        ~count:property_test_count
        (QCheck.triple TestArbitrary.arb_tok TestArbitrary.arb_tok TestArbitrary.arb_kit)
      @@ fun (collateral, collateral_at_auction, outstanding_kit) ->

      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:outstanding_kit
          ~active:true
          ~address:burrow_addr
          ~delegate:None
          ~collateral:collateral
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:collateral_at_auction
          ~last_checker_timestamp:(Ligo.timestamp_from_seconds_literal 0) in

      let _ = Burrow.compute_collateral_to_auction Parameters.initial_parameters burrow0 in
      true

    );

  ]

let () =
  run_test_tt_main
    suite
