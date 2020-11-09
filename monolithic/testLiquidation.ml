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
    last_touched = Timestamp.of_seconds 1;
  }

let initial_burrow : Burrow.t =
  { owner = Address.of_string "192837";
    delegate = None;
    active = true;
    collateral = Tez.of_mutez 10_000_000;
    outstanding_kit = Kit.of_string "20.0";
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

        assert_equal Partial liquidation_result.outcome ~printer:Liquidation.show_liquidation_outcome;

        let new_burrow = liquidation_result.burrow_state in
        assert_equal { burrow with
                       collateral = Tez.of_mutez 2_633_201;
                       outstanding_kit = Kit.of_string "20";
                       excess_kit = Kit.zero;
                       collateral_at_auction = Tez.of_mutez 6_356_799; }
          new_burrow ~printer:Burrow.show;

        assert_equal Tez.(Constants.creation_deposit + Tez.of_mutez 10_000) liquidation_result.liquidation_reward ~printer:Tez.show;
        assert_equal
          Tez.(new_burrow.collateral + new_burrow.collateral_at_auction + liquidation_result.liquidation_reward)
          Tez.(burrow.collateral + burrow.collateral_at_auction)
          ~printer:Tez.show;
        assert_bool "not now optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params new_burrow));
        assert_bool "not now liquidatable" (not (Burrow.is_liquidatable params new_burrow));
        assert_bool "still overburrowed" (Burrow.is_overburrowed params new_burrow));

    ("unwarranted liquidation test" >:: fun _ ->
        let burrow = { initial_burrow with
                       collateral = Tez.of_mutez 10_000_000;
                       outstanding_kit = Kit.of_string "10";
                       excess_kit = Kit.zero; } in

        assert_bool "is not overburrowed" (not (Burrow.is_overburrowed params burrow));
        assert_bool "is optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params burrow));
        assert_bool "is liquidatable" (not (Burrow.is_liquidatable params burrow));

        let liquidation_result = request_liquidation params burrow in

        assert_equal Unnecessary liquidation_result.outcome ~printer:Liquidation.show_liquidation_outcome;

        let new_burrow = liquidation_result.burrow_state in
        assert_equal burrow new_burrow ~printer:Burrow.show;
        assert_equal Tez.zero liquidation_result.liquidation_reward ~printer:Tez.show;
    );

    ("complete liquidation test" >:: fun _ ->
        let burrow = { initial_burrow with
                       collateral = Tez.of_mutez 10_000_000;
                       outstanding_kit = Kit.of_string "100";
                       excess_kit = Kit.zero; } in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = request_liquidation params burrow in

        assert_equal Complete liquidation_result.outcome ~printer:Liquidation.show_liquidation_outcome;

        let new_burrow = liquidation_result.burrow_state in
        assert_equal { burrow with
                       collateral = Tez.zero;
                       outstanding_kit = Kit.of_string "100";
                       excess_kit = Kit.zero;
                       collateral_at_auction = Tez.of_mutez 8_990_000; }
          new_burrow ~printer:Burrow.show;

        assert_equal Tez.(Constants.creation_deposit + Tez.of_mutez 10_000) liquidation_result.liquidation_reward ~printer:Tez.show;
        assert_equal
          Tez.(new_burrow.collateral + new_burrow.collateral_at_auction + liquidation_result.liquidation_reward)
          Tez.(burrow.collateral + burrow.collateral_at_auction)
          ~printer:Tez.show;
        assert_bool "optimistically overburrowed" (Burrow.is_optimistically_overburrowed params new_burrow);
        assert_bool "liquidatable" (Burrow.is_liquidatable params new_burrow);
        assert_bool "still overburrowed" (Burrow.is_overburrowed params new_burrow));

    ("complete and close liquidation test" >:: fun _ ->
        let burrow = { initial_burrow with
                       collateral = Tez.of_mutez 1_000_000;
                       outstanding_kit = Kit.of_string "100";
                       excess_kit = Kit.zero; } in

        assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
        assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
        assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

        let liquidation_result = request_liquidation params burrow in

        assert_equal Close liquidation_result.outcome ~printer:Liquidation.show_liquidation_outcome;

        let new_burrow = liquidation_result.burrow_state in
        assert_equal { burrow with
                       active = false;
                       collateral = Tez.zero;
                       outstanding_kit = Kit.zero;
                       excess_kit = Kit.zero;
                       collateral_at_auction = Tez.zero; }
          new_burrow ~printer:Burrow.show;

        (* TODO: reward exceeds initial collateral in burrow - is that right? *)
        assert_equal Tez.(Constants.creation_deposit + Tez.of_mutez 1000)  liquidation_result.liquidation_reward ~printer:Tez.show;
        assert_bool "not optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params new_burrow));
        assert_bool "not liquidatable" (not (Burrow.is_liquidatable params new_burrow));
        assert_bool "not overburrowed" (not (Burrow.is_overburrowed params new_burrow)));
  ]
