open OUnit2
open TestCommon
open Kit
open FixedPoint
open Error

let suite =
  "Burrow tests" >::: [
    ("burrow_burn_kit - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_burn_kit
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (kit_of_mukit (Ligo.nat_from_literal "1n"))
              burrow0
         )
    );

    ("burrow_burn_kit - burning exactly outstanding_kit returns expected burrow state" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           burrow0
       in

       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_burn_kit - burning greater than outstanding_kit returns expected burrow state" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "2n"))
           burrow0
       in

       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "1n")) (Burrow.burrow_excess_kit burrow)
    );


    ("burrow_burn_kit - burning less than outstanding_kit returns expected burrow state" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "2n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           burrow0
       in

       assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "1n")) (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_burn_kit - burning zero kit returns expected burrow state" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       let burrow = Burrow.burrow_burn_kit
           Parameters.initial_parameters
           kit_zero
           burrow0
       in

       assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "1n")) (Burrow.burrow_outstanding_kit burrow);
       assert_equal ~printer:show_kit kit_zero (Burrow.burrow_excess_kit burrow)
    );

    ("burrow_set_delegate - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_set_delegate
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (Some charles_key_hash)
              burrow0
         )
    );

    ("burrow_set_allow_all_tez_deposits - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_set_allow_all_tez_deposits
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
              true
         )
    );

    ("burrow_set_allow_all_kit_burns - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_set_allow_all_kit_burns
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
              true
         )
    );

    ("burrow_is_overburrowed - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_is_overburrowed
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
         )
    );

    ("burrow_deposit_tez - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_deposit_tez
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (Ligo.tez_from_literal "1mutez")
              burrow0
         )
    );

    ("burrow_withdraw_tez - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_withdraw_tez
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (Ligo.tez_from_literal "1mutez")
              burrow0
         )
    );

    ("burrow_mint_kit - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_mint_kit
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              (kit_of_mukit (Ligo.nat_from_literal "1n"))
              burrow0
         )
    );

    ("burrow_activate - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_activate
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              Constants.creation_deposit
              burrow0
         )
    );

    ("burrow_deactivate - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_deactivate
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
         )
    );

    ("burrow_increase_permission_version - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_increase_permission_version
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
         )
    );

    ("burrow_is_liquidatable - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_is_liquidatable
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
         )
    );

    ("compute_min_kit_for_unwarranted - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.compute_min_kit_for_unwarranted
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
              (Ligo.tez_from_literal "1mutez")
         )
    );

    ("burrow_request_liquidation - fails for a burrow which needs to be touched" >::
     fun _ ->
       let burrow0 = Burrow.make_burrow_for_test
           ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "1n"))
           ~excess_kit:kit_zero
           ~active:true
           ~permission_version:(Ligo.nat_from_literal "0n")
           ~allow_all_tez_deposits:false
           ~allow_all_kit_burnings:false
           ~delegate:None
           ~collateral:(Ligo.tez_from_literal "0mutez")
           ~adjustment_index:fixedpoint_one
           ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
           ~liquidation_slices:None
           ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

       assert_raises
         (Failure (Ligo.string_of_int error_OperationOnUntouchedBurrow))
         (fun () ->
            Burrow.burrow_request_liquidation
              {Parameters.initial_parameters with last_touched=(Ligo.timestamp_from_seconds_literal 1)}
              burrow0
         )
    );
  ]
