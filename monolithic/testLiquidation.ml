open Parameters
open FixedPoint
open Tez
open Kit
open Timestamp
open Burrow
open Address
open Liquidation
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

let initial_burrow : Burrow.t =
  { owner = Address.of_string "192837";
    delegate = None;
    active = true;
    collateral = Tez.of_mutez 10_000_000;
    outstanding_kit = Kit.of_mukit 20_000_000;
    excess_kit = Kit.zero;
    adjustment_index = Parameters.compute_adjustment_index params;
    collateral_at_auction = Tez.zero;
    last_touched = Timestamp.of_seconds 0;
  }

let suite =
  "LiquidationTests" >::: [
    ("partial liquidation test" >:: fun _ ->
        let burrow = initial_burrow in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = request_liquidation params burrow in

        assert_equal
          (Partial
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 10_000);
               tez_to_auction = Tez.of_mutez 7_142_471;
               expected_kit = Kit.of_mukit 17_592_293;
               min_received_kit_for_unwarranted = Kit.of_mukit 27_141_389;
               burrow_state =
                 { burrow with
                   collateral = Tez.of_mutez 1_847_529;
                   outstanding_kit = Kit.of_mukit 20_000_000;
                   excess_kit = Kit.zero;
                   collateral_at_auction = Tez.of_mutez 7_142_471; };
             }
          )
          liquidation_result
          ~printer:Liquidation.show_liquidation_result;
    );

    ("unwarranted liquidation test" >:: fun _ ->
        let burrow = { initial_burrow with
                       collateral = Tez.of_mutez 10_000_000;
                       outstanding_kit = Kit.of_mukit 10_000_000;
                       excess_kit = Kit.zero; } in

        assert_bool "is not overburrowed" (not (Burrow.is_overburrowed params burrow));
        assert_bool "is optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params burrow));
        assert_bool "is liquidatable" (not (Burrow.is_liquidatable params burrow));

        let liquidation_result = request_liquidation params burrow in

        assert_equal Unnecessary liquidation_result ~printer:Liquidation.show_liquidation_result;
    );

    ("complete liquidation test" >:: fun _ ->
        let burrow = { initial_burrow with
                       collateral = Tez.of_mutez 10_000_000;
                       outstanding_kit = Kit.of_mukit 100_000_000;
                       excess_kit = Kit.zero; } in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = request_liquidation params burrow in

        assert_equal
          (Complete
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 10_000);
               tez_to_auction = Tez.of_mutez 8_990_000;
               expected_kit = Kit.of_mukit 22_142_857;
               min_received_kit_for_unwarranted = Kit.of_mukit 170_810_000;
               burrow_state =
                 { burrow with
                   collateral = Tez.zero;
                   outstanding_kit = Kit.of_mukit 100_000_000;
                   excess_kit = Kit.zero;
                   collateral_at_auction = Tez.of_mutez 8_990_000; };
             }
          )
          liquidation_result
          ~printer:Liquidation.show_liquidation_result;
    );

    ("complete and close liquidation test" >:: fun _ ->
        let burrow = { initial_burrow with
                       collateral = Tez.of_mutez 1_000_000;
                       outstanding_kit = Kit.of_mukit 100_000_000;
                       excess_kit = Kit.zero; } in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = request_liquidation params burrow in

        assert_equal
          (Close
             { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 1000);
               tez_to_auction = Tez.of_mutez 999_000;
               expected_kit = Kit.of_mukit 2_460_591;
               min_received_kit_for_unwarranted = Kit.of_mukit 189_810_000;
               burrow_state =
                 { burrow with
                   active = false;
                   collateral = Tez.zero;
                   collateral_at_auction = Tez.of_mutez 999_000; };
             }
          )
          liquidation_result
          ~printer:Liquidation.show_liquidation_result;
    );
  ]
