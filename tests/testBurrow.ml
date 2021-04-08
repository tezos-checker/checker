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

    ("burrow_deposit_tez - burrow after successful deposit has expected collateral and excess" >::
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
         (Burrow.burrow_collateral burrow);
       assert_equal
         ~printer:show_kit
         kit_zero
         (Burrow.burrow_excess_kit burrow)
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

    ("burrow_activate - fails for a burrow which is already active" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_BurrowIsAlreadyActive))
         (fun () ->
            Burrow.burrow_activate
              Parameters.initial_parameters
              Constants.creation_deposit
              (make_test_burrow
                 ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
                 ~collateral:(Ligo.tez_from_literal "1mutez")
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
              (Ligo.sub_tez_tez Constants.creation_deposit (Ligo.tez_from_literal "1mutez"))
              (make_test_burrow
                 ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
                 ~collateral:(Ligo.tez_from_literal "1mutez")
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
              ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
              ~collateral:(Ligo.tez_from_literal "0mutez")
              ~active:false
           )
       in
       assert_bool "burrow was not flagged as active after calling burrow_activate" (Burrow.burrow_active burrow)
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

    ("burrow_deactivate - fails for a burrow which is already inactive" >::
     fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int error_DeactivatingAnInactiveBurrow))
         (fun () ->
            Burrow.burrow_deactivate
              Parameters.initial_parameters
              (make_test_burrow
                 ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "0n"))
                 ~collateral:(Ligo.tez_from_literal "1mutez")
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
                 ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
                 ~collateral:(Ligo.tez_from_literal "10mutez")
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
                 ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10n"))
                 ~collateral:(Ligo.tez_from_literal "1mutez")
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
                  ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "0n"))
                  ~excess_kit:kit_zero
                  ~active:true
                  ~permission_version:(Ligo.nat_from_literal "0n")
                  ~allow_all_tez_deposits:false
                  ~allow_all_kit_burnings:false
                  ~delegate:None
                  ~collateral:(Ligo.tez_from_literal "10mutez")
                  ~adjustment_index:fixedpoint_one
                  ~collateral_at_auction:(Ligo.tez_from_literal "1mutez")
                  ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
              )
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

    ("burrow_increase_permission_version - permission version in burrow state matches returned permission version" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "0n"))
           ~active:true
           ~collateral:(Ligo.tez_from_literal "1mutez") in

       let new_version, burrow = Burrow.burrow_increase_permission_version Parameters.initial_parameters burrow0 in

       assert_equal
         ~printer:Ligo.string_of_nat
         new_version
         (Burrow.burrow_permission_version burrow);
       assert_bool
         "New permission version was equal to the original permission version"
         (not (new_version = (Burrow.burrow_permission_version burrow0)))
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
    (* Property tests for ensuring burrow invariants are obeyed *)
    (* =========================================================================================== *)
    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_mint_kit - returned burrow obeys burrow invariants"
        ~count:property_test_count
        (QCheck.pair TestArbitrary.arb_kit QCheck.(0 -- max_int))
      @@ fun (burrow_kit, arbitrary_int) ->

      let kit_to_mint = kit_of_mukit (Ligo.nat_from_literal "10n") in
      (* Random kit balances which obey the burrow invariants and allow minting kit_to_mint without overburrowing *)
      let outstanding, excess, collateral = if (arbitrary_int mod 2) = 0 then
          (burrow_kit, kit_zero, Ligo.mul_tez_nat (Ligo.tez_from_literal "10mutez") (kit_to_mukit_nat burrow_kit))
        else
          (kit_zero, burrow_kit, Ligo.mul_tez_nat (Ligo.tez_from_literal "10mutez") (kit_to_mukit_nat kit_to_mint))
      in
      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:outstanding
          ~excess_kit:excess
          ~active:true
          ~permission_version:(Ligo.nat_from_literal "0n")
          ~allow_all_tez_deposits:false
          ~allow_all_kit_burnings:false
          ~delegate:None
          ~collateral:collateral
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
          ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

      let _ = Burrow.assert_burrow_invariants (Burrow.burrow_mint_kit Parameters.initial_parameters kit_to_mint burrow0) in
      true
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_burn_kit - returned burrow obeys burrow invariants"
        ~count:property_test_count
        (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit QCheck.(0 -- max_int))
      @@ fun (burrow_kit, kit_to_burn, arbitrary_int) ->

      (* Random kit balances which obey the burrow invariants and allow minting kit_to_mint without overburrowing *)
      let outstanding, excess = if (arbitrary_int mod 2) = 0 then
          (burrow_kit, kit_zero)
        else
          (kit_zero, burrow_kit)
      in
      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:outstanding
          ~excess_kit:excess
          ~active:true
          ~permission_version:(Ligo.nat_from_literal "0n")
          ~allow_all_tez_deposits:false
          ~allow_all_kit_burnings:false
          ~delegate:None
          ~collateral:(Ligo.tez_from_literal "1mutez")
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
          ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

      let _ = Burrow.assert_burrow_invariants (Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn burrow0) in
      true
    );

    (
      let upper_collat_bound_for_test = 1_001_001 in
      let kit_to_allow_liquidation = kit_of_mukit (Ligo.nat_from_literal "1901901n") in
      let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(0 -- upper_collat_bound_for_test) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_request_liquidation - burrow returned in case 2a (close burrow) obeys burrow invariants"
        ~count:property_test_count
        arb_tez
      @@ fun collateral ->

      let burrow0 = make_test_burrow
          ~outstanding_kit:kit_to_allow_liquidation
          ~active:true
          ~collateral:collateral in

      let burrow = match Burrow.burrow_request_liquidation Parameters.initial_parameters burrow0 with
        | Some (Burrow.Close, liquidation_details) -> liquidation_details.burrow_state
        | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
        | _ -> failwith "liquidation_type returned by burrow_request_liquidation was not Close as is expected by this test"
      in
      Burrow.assert_burrow_invariants burrow;
      true
    );

    (
      (* 1 / liquidation_reward * creation_deposit *)
      let lower_collat_bound_for_test = 1_001_001 in
      let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(lower_collat_bound_for_test -- max_int) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_request_liquidation - burrow returned in case 2b (liquidate all collateral) obeys burrow invariants"
        ~count:property_test_count
        (QCheck.pair arb_tez TestArbitrary.arb_kit)
      @@ fun (collateral, extra_kit)->

      (* Want state where:  *)
      (* collat - reward - tez_to_auction <= creation_deposit *)
      (* 999/1000 * collat - tez_to_auction <= creation_deposit *)
      (*   tez_to_auction = outstanding_kit - collat / 1.9 *)
      (* 2899 / 1000 collat + creation_deposit <= outstanding_kit *)
      let min_kit_to_trigger_case = Common.fdiv_int_int
          (Ligo.mul_int_int (Ligo.int_from_literal "28981") (Common.tez_to_mutez collateral))
          (Ligo.int_from_literal "19000") in
      let outstanding_kit = match Ligo.is_nat min_kit_to_trigger_case with
        | Some n -> kit_add (kit_of_mukit n) extra_kit
        | None -> failwith "The calculated outstanding_kit for the test case was not a nat"
      in
      let burrow0 = make_test_burrow
          ~outstanding_kit:outstanding_kit
          ~active:true
          ~collateral:collateral in

      let burrow = match Burrow.burrow_request_liquidation Parameters.initial_parameters burrow0 with
        | Some (Burrow.Complete, liquidation_details) -> liquidation_details.burrow_state
        | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
        | _ -> failwith "liquidation_type returned by burrow_request_liquidation was not Complete as is expected by this test"
      in
      Burrow.assert_burrow_invariants burrow;
      true
    );

    (
      (* Holding collateral constant and varying kit since the range of outstanding_kit to trigger this case
       * depends on the collateral. *)
      let collateral = 10_000_000 in
      (* liquidation limit + 1 *)
      let min_kit_for_case = 5_263_158  in
      (* (999 / 1000 collat - creation_deposit) - 1/10 * (999 / 1000 collat - creation_deposit) *)
      let max_kit_for_case = 8_091_000 in
      let arb_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(min_kit_for_case -- max_kit_for_case) in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_request_liquidation - burrow returned in case 2c (partially liquidate collateral) obeys burrow invariants"
        ~count:property_test_count
        arb_kit
      @@ fun outstanding_kit ->

      let burrow0 = make_test_burrow
          ~outstanding_kit:outstanding_kit
          ~active:true
          ~collateral:(Ligo.tez_from_literal (string_of_int collateral ^ "mutez")) in

      let burrow = match Burrow.burrow_request_liquidation Parameters.initial_parameters burrow0 with
        | Some (Burrow.Partial, liquidation_details) -> liquidation_details.burrow_state
        | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
        | _ -> failwith "liquidation_type returned by burrow_request_liquidation was not Partial as is expected by this test"
      in
      Burrow.assert_burrow_invariants burrow;
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

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_deactivate - all collateral is extracted from burrow"
        ~count:property_test_count
        TestArbitrary.arb_tez
      @@ fun collateral_balance_tez ->
      let burrow0 = make_test_burrow
          ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "0n"))
          ~active:true
          ~collateral:collateral_balance_tez in

      let burrow, returned_tez = Burrow.burrow_deactivate
          Parameters.initial_parameters
          burrow0 in

      assert_equal
        ~printer:Ligo.string_of_tez
        (Ligo.tez_from_literal ("0mutez"))
        (Burrow.burrow_collateral burrow);
      assert_equal
        ~printer:Ligo.string_of_tez
        (Ligo.add_tez_tez Constants.creation_deposit collateral_balance_tez)
        returned_tez;
      true
    );

    (
      let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(1_000_000 -- max_int) in
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_deactivate / burrow_activate - no collateral lost in re-activation round-trip"
        ~count:property_test_count
        arb_tez
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

      assert_equal
        ~printer:Ligo.string_of_tez
        starting_collateral
        (Burrow.burrow_collateral burrow);
      true
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_increase_permission_version - increases burrow permission version by exactly one"
        ~count:property_test_count
        TestArbitrary.arb_nat
      @@ fun initial_permission_version ->

      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:kit_zero
          ~excess_kit:kit_zero
          ~active:true
          ~permission_version:initial_permission_version
          ~allow_all_tez_deposits:false
          ~allow_all_kit_burnings:false
          ~delegate:None
          ~collateral:(Ligo.tez_from_literal "1mutez")
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
          ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

      let new_permission_version, _ = Burrow.burrow_increase_permission_version Parameters.initial_parameters burrow0 in

      assert_equal
        ~printer:Ligo.string_of_int
        (Ligo.int_from_literal "1")
        (Ligo.sub_nat_nat new_permission_version initial_permission_version);
      true
    );

    (
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"burrow_touch - net kit associated with burrow does not change when no adjustment is required"
        ~count:property_test_count
        (QCheck.pair TestArbitrary.arb_kit QCheck.(0 -- max_int))
      @@ fun (kit, arbitrary_int) ->

      (* Random kit balances which obey the burrow invariants *)
      let outstanding, excess = if (arbitrary_int mod 2) = 0 then
          (kit, kit_zero)
        else
          (kit_zero, kit)
      in
      (* Helper for computing the net kit balance of the burrow *)
      let net_kit_int b = (Ligo.sub_int_int (kit_to_mukit_int (Burrow.burrow_excess_kit b)) (kit_to_mukit_int (Burrow.burrow_outstanding_kit b))) in
      (* Note: this combination of burrow and parameters cause the adjustment to be just the identity *)
      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:outstanding
          ~excess_kit:excess
          ~active:true
          ~permission_version:(Ligo.nat_from_literal "0n")
          ~allow_all_tez_deposits:false
          ~allow_all_kit_burnings:false
          ~delegate:None
          ~collateral:(Ligo.tez_from_literal "1mutez")
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
          ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in
      let parameters = {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)} in

      let burrow = Burrow.burrow_touch parameters burrow0 in
      assert_bool
        "Net kit changed during touch operation even though no adjustment should have been performed."
        (net_kit_int burrow = net_kit_int burrow0);
      true
    );


  ]
