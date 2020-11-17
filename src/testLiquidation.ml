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
  { q = FixedPoint.of_string "1.015";
    index = Tez.of_mutez 320_000;
    protected_index = Tez.of_mutez 360_000;
    target = FixedPoint.of_string "1.08";
    drift = FixedPoint.of_string "0.0";
    drift' = FixedPoint.of_string "0.0";
    burrow_fee_index = FixedPoint.of_string "1.0";
    imbalance_index = FixedPoint.of_string "1.0";
    outstanding_kit = Kit.one; (* TODO: What should that be? *)
    circulating_kit = Kit.zero; (* TODO: What should that be? *)
    last_touched = Timestamp.of_seconds 0;
  }

let initial_burrow =
  Burrow.make_for_test
    ~owner:(Address.of_string "192837")
    ~delegate:None
    ~active:true
    ~collateral:(Tez.of_mutez 10_000_000)
    ~outstanding_kit:(Kit.of_mukit 20_000_000)
    ~excess_kit:Kit.zero
    ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
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
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 10_000);
               tez_to_auction = Tez.of_mutez 7_142_472;
               expected_kit = Kit.of_mukit 17_592_296;
               min_kit_for_unwarranted = Kit.of_mukit 27_141_394;
               burrow_state =
                 Burrow.make_for_test
                   ~owner:(Address.of_string "192837")
                   ~delegate:None
                   ~active:true
                   ~collateral:(Tez.of_mutez 1_847_528)
                   ~outstanding_kit:(Kit.of_mukit 20_000_000)
                   ~excess_kit:Kit.zero
                   ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
                   ~collateral_at_auction:(Tez.of_mutez 7_142_472)
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
            ~owner:(Address.of_string "192837")
            ~delegate:None
            ~active:true
            ~collateral:(Tez.of_mutez 10_000_000)
            ~outstanding_kit:(Kit.of_mukit 10_000_000)
            ~excess_kit:Kit.zero
            ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
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
            ~owner:(Address.of_string "192837")
            ~delegate:None
            ~active:true
            ~collateral:(Tez.of_mutez 10_000_000)
            ~outstanding_kit:(Kit.of_mukit 100_000_000)
            ~excess_kit:Kit.zero
            ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
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
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 10_000);
               tez_to_auction = Tez.of_mutez 8_990_000;
               expected_kit = Kit.of_mukit 22_142_858;
               min_kit_for_unwarranted = Kit.of_mukit 170_810_000;
               burrow_state =
                 Burrow.make_for_test
                   ~owner:(Address.of_string "192837")
                   ~delegate:None
                   ~active:true
                   ~collateral:Tez.zero
                   ~outstanding_kit:(Kit.of_mukit 100_000_000)
                   ~excess_kit:Kit.zero
                   ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
                   ~collateral_at_auction:(Tez.of_mutez 8_990_000)
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
            ~owner:(Address.of_string "192837")
            ~delegate:None
            ~active:true
            ~collateral:(Tez.of_mutez 1_000_000)
            ~outstanding_kit:(Kit.of_mukit 100_000_000)
            ~excess_kit:Kit.zero
            ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
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
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 1000);
               tez_to_auction = Tez.of_mutez 999_000;
               expected_kit = Kit.of_mukit 2_460_592;
               min_kit_for_unwarranted = Kit.of_mukit 189_810_000;
               burrow_state =
                 Burrow.make_for_test
                   ~owner:(Address.of_string "192837")
                   ~delegate:None
                   ~active:false
                   ~collateral:Tez.zero
                   ~outstanding_kit:(Kit.of_mukit 100_000_000)
                   ~excess_kit:Kit.zero
                   ~adjustment_index:(FixedPoint.of_q_floor (Parameters.compute_adjustment_index params)) (* TODO: round up or down here? *)
                   ~collateral_at_auction:(Tez.of_mutez 999_000)
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
