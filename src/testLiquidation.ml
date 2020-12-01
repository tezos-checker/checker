open Burrow
open OUnit2

(*
Properties we expect to hold
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
General
* is_liquidatable ==> is_overburrowed (not the other way around)
* No interaction with the burrow has any effect if it's inactive.
* Liquidation of an active burrow with collateral < creation_deposit should "close" it

If a liquidation was deemed Unnecessary:
* is_liquidatable is false for the given burrow

If a liquidation was deemed Partial:
* is_liquidatable is true for the given burrow
* is_optimistically_overburrowed is false for the resulting burrow

If a liquidation was deemed Complete:
* is_liquidatable is true for the given burrow
* is_optimistically_overburrowed is true for the resulting burrow
* the resulting burrow has no collateral

If a liquidation was deemed Close:
* is_liquidatable is true for the given burrow
* the resulting burrow is zeroed and inactive
*)

let params : Parameters.t =
  { q = FixedPoint.of_q_floor (Q.of_string "1015/1000");
    index = Tez.of_mutez 320_000;
    protected_index = Tez.of_mutez 360_000;
    target = FixedPoint.of_q_floor (Q.of_string "108/100");
    drift = FixedPoint.zero;
    drift' = FixedPoint.zero;
    burrow_fee_index = FixedPoint.one;
    imbalance_index = FixedPoint.one;
    outstanding_kit = Kit.one; (* TODO: What should that be? *)
    circulating_kit = Kit.zero; (* TODO: What should that be? *)
    last_touched = Timestamp.of_seconds 0;
  }

let initial_burrow =
  Burrow.make_for_test
    ~permission_version:0
    ~allow_all_tez_deposits:false
    ~allow_all_kit_burnings:false
    ~delegate:None
    ~active:true
    ~collateral:(Tez.of_mutez 10_000_000)
    ~outstanding_kit:(Kit.of_mukit 20_000_000)
    ~excess_kit:Kit.zero
    ~adjustment_index:(Parameters.compute_adjustment_index params)
    ~collateral_at_auction:Tez.zero
    ~last_touched:(Timestamp.of_seconds 0)
    ~liquidation_slices:None

let suite =
  "LiquidationTests" >::: [
    ("partial liquidation test" >:: fun _ ->
        let burrow = initial_burrow in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = Burrow.request_liquidation params burrow in

        assert_equal
          (Partial
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 9_999);
               tez_to_auction = Tez.of_mutez 7_142_471;
               expected_kit = Kit.of_mukit 17_592_294;
               min_kit_for_unwarranted = Kit.of_mukit 27_141_390;
               burrow_state =
                 Burrow.make_for_test
                   ~permission_version:0
                   ~allow_all_tez_deposits:false
                   ~allow_all_kit_burnings:false
                   ~delegate:None
                   ~active:true
                   ~collateral:(Tez.of_mutez 1_847_530)
                   ~outstanding_kit:(Kit.of_mukit 20_000_000)
                   ~excess_kit:Kit.zero
                   ~adjustment_index:(Parameters.compute_adjustment_index params)
                   ~collateral_at_auction:(Tez.of_mutez 7_142_471)
                   ~last_touched:(Timestamp.of_seconds 0)
                   ~liquidation_slices:None
             }
          )
          liquidation_result
          ~printer:Burrow.show_liquidation_result;

        match liquidation_result with
        | Unnecessary | Complete _ | Close _ -> failwith "impossible"
        | Partial details -> (
            assert_bool "is overburrowed" (Burrow.is_overburrowed params details.burrow_state);
            assert_bool "is not optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params details.burrow_state));
            assert_bool "is not liquidatable" (not (Burrow.is_liquidatable params details.burrow_state));
            assert_bool "is active" (Burrow.active details.burrow_state);
          )
    );

    ("unwarranted liquidation test" >:: fun _ ->
        let burrow =
          Burrow.make_for_test
            ~permission_version:0
            ~allow_all_tez_deposits:false
            ~allow_all_kit_burnings:false
            ~delegate:None
            ~active:true
            ~collateral:(Tez.of_mutez 10_000_000)
            ~outstanding_kit:(Kit.of_mukit 10_000_000)
            ~excess_kit:Kit.zero
            ~adjustment_index:(Parameters.compute_adjustment_index params)
            ~collateral_at_auction:Tez.zero
            ~last_touched:(Timestamp.of_seconds 0)
            ~liquidation_slices:None
        in

        assert_bool "is not overburrowed" (not (Burrow.is_overburrowed params burrow));
        assert_bool "is not optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params burrow));
        assert_bool "is not liquidatable" (not (Burrow.is_liquidatable params burrow));

        let liquidation_result = Burrow.request_liquidation params burrow in

        assert_equal Unnecessary liquidation_result ~printer:Burrow.show_liquidation_result;
    );

    ("complete liquidation test" >:: fun _ ->
        let burrow =
          Burrow.make_for_test
            ~permission_version:0
            ~allow_all_tez_deposits:false
            ~allow_all_kit_burnings:false
            ~delegate:None
            ~active:true
            ~collateral:(Tez.of_mutez 10_000_000)
            ~outstanding_kit:(Kit.of_mukit 100_000_000)
            ~excess_kit:Kit.zero
            ~adjustment_index:(Parameters.compute_adjustment_index params)
            ~collateral_at_auction:Tez.zero
            ~last_touched:(Timestamp.of_seconds 0)
            ~liquidation_slices:None
        in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = Burrow.request_liquidation params burrow in

        assert_equal
          (Complete
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 9_999);
               tez_to_auction = Tez.of_mutez 8_990_001;
               expected_kit = Kit.of_mukit 22_142_860;
               min_kit_for_unwarranted = Kit.of_mukit 170_810_019;
               burrow_state =
                 Burrow.make_for_test
                   ~permission_version:0
                   ~allow_all_tez_deposits:false
                   ~allow_all_kit_burnings:false
                   ~delegate:None
                   ~active:true
                   ~collateral:Tez.zero
                   ~outstanding_kit:(Kit.of_mukit 100_000_000)
                   ~excess_kit:Kit.zero
                   ~adjustment_index:(Parameters.compute_adjustment_index params)
                   ~collateral_at_auction:(Tez.of_mutez 8_990_001)
                   ~last_touched:(Timestamp.of_seconds 0)
                   ~liquidation_slices:None
             }
          )
          liquidation_result
          ~printer:Burrow.show_liquidation_result;

        match liquidation_result with
        | Unnecessary | Partial _ | Close _ -> failwith "impossible"
        | Complete details -> (
            assert_bool "is overburrowed" (Burrow.is_overburrowed params details.burrow_state);
            assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params details.burrow_state);
            assert_bool "is liquidatable" (Burrow.is_liquidatable params details.burrow_state);
            assert_bool "is active" (Burrow.active details.burrow_state);
          );
    );

    ("complete and close liquidation test" >:: fun _ ->
        let burrow =
          Burrow.make_for_test
            ~permission_version:0
            ~allow_all_tez_deposits:false
            ~allow_all_kit_burnings:false
            ~delegate:None
            ~active:true
            ~collateral:(Tez.of_mutez 1_000_000)
            ~outstanding_kit:(Kit.of_mukit 100_000_000)
            ~excess_kit:Kit.zero
            ~adjustment_index:(Parameters.compute_adjustment_index params)
            ~collateral_at_auction:Tez.zero
            ~last_touched:(Timestamp.of_seconds 0)
            ~liquidation_slices:None
        in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = Burrow.request_liquidation params burrow in

        assert_equal
          (Close
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 999);
               tez_to_auction = Tez.of_mutez 999_001;
               expected_kit = Kit.of_mukit 2_460_594;
               min_kit_for_unwarranted = Kit.of_mukit 189_810_190;
               burrow_state =
                 Burrow.make_for_test
                   ~permission_version:0
                   ~allow_all_tez_deposits:false
                   ~allow_all_kit_burnings:false
                   ~delegate:None
                   ~active:false
                   ~collateral:Tez.zero
                   ~outstanding_kit:(Kit.of_mukit 100_000_000)
                   ~excess_kit:Kit.zero
                   ~adjustment_index:(Parameters.compute_adjustment_index params)
                   ~collateral_at_auction:(Tez.of_mutez 999_001)
                   ~last_touched:(Timestamp.of_seconds 0)
                   ~liquidation_slices:None
             }
          )
          liquidation_result
          ~printer:Burrow.show_liquidation_result;

        match liquidation_result with
        | Unnecessary | Partial _ | Complete _ -> failwith "impossible"
        | Close details -> (
            assert_bool "is overburrowed" (Burrow.is_overburrowed params details.burrow_state);
            assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params details.burrow_state);
            assert_bool "is not liquidatable" (not (Burrow.is_liquidatable params details.burrow_state));
            assert_bool "is inactive" (not (Burrow.active details.burrow_state));
          );
    );
  ]
