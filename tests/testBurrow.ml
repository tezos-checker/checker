open Error
open FixedPoint
open Kit
open OUnit2
open TestCommon

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let burrow_addr = Ligo.address_of_string "BURROW_ADDR"

(* Create a burrow from an amount of kit outstanding and tez collateral along with
 * an active flag. The rest of the parameters are fixed. *)
let make_test_burrow ~outstanding_kit ~collateral ~active = Burrow.make_burrow_for_test
    ~outstanding_kit:outstanding_kit
    ~excess_kit:kit_zero
    ~active:active
    ~address:burrow_addr
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

    ("burrow_burn_kit - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_burn_kit Parameters.initial_parameters (kit_of_mukit (Ligo.nat_from_literal "1n")) burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address burrow)
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

    ("burrow_set_delegate - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_set_delegate Parameters.initial_parameters (Some charles_key_hash) burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address burrow)
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

    ("burrow_deposit_tez - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_deposit_tez Parameters.initial_parameters (Ligo.tez_from_literal "1mutez") burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
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

    ("burrow_withdraw_tez - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "1mutez") in

       let burrow = Burrow.burrow_withdraw_tez Parameters.initial_parameters (Ligo.tez_from_literal "1mutez") burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
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

    ("burrow_mint_kit - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "4mutez") in

       let burrow = Burrow.burrow_mint_kit Parameters.initial_parameters (kit_of_mukit (Ligo.nat_from_literal "1n")) burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
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

    ("burrow_activate - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:false
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow = Burrow.burrow_activate Parameters.initial_parameters Constants.creation_deposit burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
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
                  ~address:burrow_addr
                  ~delegate:None
                  ~collateral:(Ligo.tez_from_literal "10mutez")
                  ~adjustment_index:fixedpoint_one
                  ~collateral_at_auction:(Ligo.tez_from_literal "1mutez")
                  ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
              )
         )
    );

    ("burrow_deactivate - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in

       let burrow, _ = Burrow.burrow_deactivate Parameters.initial_parameters burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
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

    (* Note: this is a bit overkill but testing anyways since the address field is extremely important *)
    ("burrow_create - address matches provided address" >::
     fun _ ->
       let burrow = Burrow.burrow_create Parameters.initial_parameters burrow_addr Constants.creation_deposit None in

       assert_equal
         ~printer:Ligo.string_of_address
         burrow_addr
         (Burrow.burrow_address burrow)

    );

    ("burrow_touch - does not change burrow address" >::
     fun _ ->
       let burrow0 = make_test_burrow
           ~outstanding_kit:kit_zero
           ~active:true
           ~collateral:(Ligo.tez_from_literal "0mutez") in
       let parameters = {
         Parameters.initial_parameters
         with last_touched=(Ligo.timestamp_from_seconds_literal 1)
       } in

       let burrow = Burrow.burrow_touch parameters burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
    );

    ("burrow_return_kit_from_auction - does not change burrow address" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "1mutez")
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in
       let slice = LiquidationAuctionPrimitiveTypes.{
           burrow=(Ligo.address_from_literal "12345", Ligo.nat_from_literal "0n");
           tez=Ligo.tez_from_literal "1mutez";
           min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "1n"));
         } in

       let burrow = Burrow.burrow_return_kit_from_auction slice (kit_of_mukit (Ligo.nat_from_literal "1n")) burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
    );

    ("burrow_return_slice_from_auction - does not change burrow address" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~address:burrow_addr
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "1mutez")
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in
       let slice = let open LiquidationAuctionPrimitiveTypes in {
           burrow=(Ligo.address_from_literal "12345", Ligo.nat_from_literal "0n");
           tez=Ligo.tez_from_literal "1mutez";
           min_kit_for_unwarranted = Some (kit_of_mukit (Ligo.nat_from_literal "1n"));
         } in

       let burrow = Burrow.burrow_return_slice_from_auction slice burrow0 in

       assert_equal
         ~printer:Ligo.string_of_address
         (Burrow.burrow_address burrow0)
         (Burrow.burrow_address  burrow)
    );

    (* This is a bit of an odd test but it ensures that the math in compute_tez_to_auction
       won't throw an exception if the constants are ever reconfigured in this way.*)
    ("compute_tez_to_auction - constants obey assumption in implementation" >::
     fun _ ->
       let open Ratio in
       let {num=f_num; den=f_den} = Constants.fminting in
       let {num=lp_num; den=lp_den} = Constants.liquidation_penalty in

       assert_bool
         ("fminting and liquidation_penalty must be configured such that" ^
          "((1 - liquidation_penalty) * fminting - 1) > 0 in order for the assumptions in " ^
          "compute_tez_to_auction to hold")
         (Ligo.gt_int_int
            (Ligo.mul_int_int
               f_num
               (Ligo.sub_int_int lp_den lp_num))
            (Ligo.mul_int_int lp_den f_den))
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
          ~address:burrow_addr
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
          ~address:burrow_addr
          ~delegate:None
          ~collateral:(Ligo.tez_from_literal "1mutez")
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
          ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

      let _ = Burrow.assert_burrow_invariants (Burrow.burrow_burn_kit Parameters.initial_parameters kit_to_burn burrow0) in
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
          ~address:burrow_addr
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

    (
      (* Note: this test was written to catch cases in an implemention of compute_tez_to_auction in which
         errors would be thrown for negative values. *)
      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"compute_tez_to_auction - does not throw exception for arbitrary inputs"
        ~count:property_test_count
        (QCheck.triple TestArbitrary.arb_tez TestArbitrary.arb_tez TestArbitrary.arb_kit)
      @@ fun (collateral, collateral_at_auction, outstanding_kit) ->

      let burrow0 = Burrow.make_burrow_for_test
          ~outstanding_kit:outstanding_kit
          ~excess_kit:kit_zero
          ~active:true
          ~address:burrow_addr
          ~delegate:None
          ~collateral:collateral
          ~adjustment_index:fixedpoint_one
          ~collateral_at_auction:collateral_at_auction
          ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

      let _ = Burrow.compute_tez_to_auction Parameters.initial_parameters burrow0 in
      true

    );

  ]
