open Error
open FixedPoint
open Kit
open OUnit2
open TestCommon

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

(* Create a burrow from an amount of kit outstanding and tez collateral along with
 * an active flag. The rest of the parameters are fixed. *)
let make_test_burrow ~outstanding_kit ~collateral ~active = Burrow.make_burrow_for_test
    ~outstanding_kit:outstanding_kit
    ~excess_kit:kit_zero
    ~active:active
    ~permission_version:(Ligo.nat_from_literal "0n")
    ~allow_all_tez_deposits:false
    ~allow_all_kit_burnings:false
    ~delegate:None
    ~collateral:collateral
    ~adjustment_index:fixedpoint_one
    ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
    ~liquidation_slices:None
    ~last_touched:(Ligo.timestamp_from_seconds_literal 0)

(* A burrow with fixed parameters which was last touched at 0s. Use for tests which check
 * that functions requiring a burrow to be recently touched fail as expected. *)
let burrow_for_needs_touch_tests = make_test_burrow
    ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "0n"))
    ~collateral:(Ligo.tez_from_literal "0mutez")
    ~active:true

let suite =
  "Burrow tests" >::: [
    ("burrow_burn_kit - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_burn_kit
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (kit_of_mukit (Ligo.nat_from_literal "1n"))
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_burn_kit - burning exactly outstanding_kit returns burrow with expected excess and outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           burrow0 in

       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_burn_kit - burning greater than outstanding_kit returns burrow with expected excess and outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "2n"))
           burrow0 in

       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "1n")) (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_burn_kit - burning less than outstanding_kit returns burrow with expected excess and outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "2n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           burrow0 in

       assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "1n")) (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_burn_kit - burning zero kit returns burrow with expected excess and outstanding kit" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           kit_zero
           burrow0 in

       assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "1n")) (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_set_delegate - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_set_delegate
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (Some charles_key_hash)
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_set_allow_all_tez_deposits - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_set_allow_all_tez_deposits
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
              true
         )
    );

    ("burrow_set_allow_all_kit_burns - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_set_allow_all_kit_burns
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
              true
         )
    );

    ("burrow_is_overburrowed - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_is_overburrowed
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_deposit_tez - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_deposit_tez
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (Ligo.tez_from_literal "1mutez")
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_deposit_tez - burrow after successful deposit has expected collateral" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "100mutez") in

       let burrow = Burrow.burrow_deposit_tez
           Parameters.initial_parameters
           (Ligo.tez_from_literal "1mutez")
           burrow0 in

       assert_equal
         ~printer:Ligo.string_of_tez
         (Ligo.tez_from_literal "101mutez")
         (Burrow.burrow_collateral burrow)
    );

    ("burrow_withdraw_tez - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_withdraw_tez
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (Ligo.tez_from_literal "1mutez")
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_withdraw_tez - burrow after successful withdrawal has expected collateral" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "100mutez") in

       let burrow = Burrow.burrow_withdraw_tez
           Parameters.initial_parameters
           (Ligo.tez_from_literal "1mutez")
           burrow0 in

       assert_equal
         ~printer:Ligo.string_of_tez
         (Ligo.tez_from_literal "99mutez")
         (Burrow.burrow_collateral burrow)
    );

    ("burrow_mint_kit - burrow after successful minting has expected collateral" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "100mutez") in

       let burrow = Burrow.burrow_mint_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           burrow0 in

       assert_equal
         ~printer:show_kit
         (kit_of_mukit (Ligo.nat_from_literal "2n"))
         (Burrow.burrow_outstanding_kit burrow)
    );

    ("burrow_mint_kit - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_mint_kit
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (kit_of_mukit (Ligo.nat_from_literal "1n"))
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_activate - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_activate
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              Constants.creation_deposit
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_deactivate - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_deactivate
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_increase_permission_version - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_increase_permission_version
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
         )
    );

    ("burrow_is_liquidatable - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_is_liquidatable
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
         )
    );

    ("compute_min_kit_for_unwarranted - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.compute_min_kit_for_unwarranted
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
              (Ligo.tez_from_literal "1mutez")
         )
    );

    ("burrow_request_liquidation - fails for a burrow which needs to be touched" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_request_liquidation
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow_for_needs_touch_tests
         )
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
      let arb_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(min_mint_to_overburrow -- max_int) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_mint_kit - fails when minting would cause the burrow to be overburrowed"
        ~count:property_test_count
        arb_kit
      @@ fun mint_kit ->
      let burrow = make_test_burrow
          ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal (string_of_int outstanding_kit ^ "n")))
          ~active:true
          ~collateral:(Ligo.tez_from_literal ((string_of_int collateral) ^ "mutez")) in

      assert_raises
        (Failure (Ligo.string_of_int error_MintKitFailure))
        (fun () -> Burrow.burrow_mint_kit Parameters.initial_parameters mint_kit burrow);
      true
    );

    (
      let collateral = 100 in
      let outstanding_kit = kit_of_mukit (Ligo.nat_from_literal ("1n")) in
      (* As of writing, the below calculation simplifies to ceil(outstanding_kit * 21/10) *)
      let burrowing_limit_tez = 3 in
      let min_withdrawal_to_overburrow = (collateral - burrowing_limit_tez) + 1 in
      let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(min_withdrawal_to_overburrow -- collateral) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_withdraw_tez - fails when the withdrawal would cause the burrow to be overburrowed"
        ~count:property_test_count
        arb_tez
      @@ fun tez_to_withdraw ->
      let burrow = make_test_burrow
          ~outstanding_kit:outstanding_kit
          ~active:true
          ~collateral:(Ligo.tez_from_literal ((string_of_int collateral) ^ "mutez")) in

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
        ~name:"burrow_deposit_tez - increases burrow collateral by exactly deposit amount"
        ~count:property_test_count
        TestArbitrary.arb_tez
      @@ fun tez_to_deposit ->
      let burrow0 = make_test_burrow
          ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
          ~active:true
          ~collateral:(Ligo.tez_from_literal "100mutez") in

      let burrow = Burrow.burrow_deposit_tez
          Parameters.initial_parameters
          tez_to_deposit
          burrow0 in

      assert_equal
        ~printer:Ligo.string_of_tez
        (Ligo.add_tez_tez (Burrow.burrow_collateral burrow0) tez_to_deposit)
        (Burrow.burrow_collateral burrow);
      true
    );

  ]
