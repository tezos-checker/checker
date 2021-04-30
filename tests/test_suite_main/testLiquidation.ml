open Kit
open Burrow
open Ratio
open OUnit2
open FixedPoint
open Parameters
open Test_lib
open Test_lib.TestCommon

let property_test_count = 1000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

type kit_option = kit option
[@@deriving show]

(* Create an arbitrary burrow state, given the set of checker's parameters (NB:
 * most values are fixed). *)
let arbitrary_burrow (params: parameters) =
  (* More likely to give Close/Unnecessary ones *)
  let arb_smart_tez_kit_1 =
    let positive_int = QCheck.(1 -- max_int) in
    QCheck.map
      (fun (t, k, factor) ->
         let tez =
           let x =
             div_ratio
               (ratio_of_int (Ligo.int_from_literal (string_of_int t)))
               (mul_ratio (ratio_of_int (Ligo.int_from_literal "2")) (ratio_of_int (Ligo.int_from_literal (string_of_int factor))))
           in
           fraction_to_tez_floor x.num x.den in
         let kit =
           let { num = x_num; den = x_den; } =
             (div_ratio
                (ratio_of_int (Ligo.int_from_literal (string_of_int k)))
                (ratio_of_int (Ligo.int_from_literal (string_of_int factor)))
             ) in
           kit_of_fraction_floor x_num x_den
         in
         (tez, kit)
      )
      (QCheck.triple positive_int positive_int positive_int) in
  (* More likely to give Complete/Partial/Unnecessary ones *)
  let arb_smart_tez_kit_2 =
    QCheck.map
      (fun (tez, kit) ->
         let tez =
           let x = div_ratio (ratio_of_tez tez) (ratio_of_int (Ligo.int_from_literal "2")) in
           fraction_to_tez_floor x.num x.den in
         (tez, kit)
      )
      (QCheck.pair TestArbitrary.arb_tez TestArbitrary.arb_kit) in
  (* Chose one of the two. Not perfect, I know, but improves coverage *)
  let arb_smart_tez_kit =
    QCheck.map
      (fun (x, y, num) -> if num mod 2 = 0 then x else y)
      (QCheck.triple arb_smart_tez_kit_1 arb_smart_tez_kit_2 QCheck.int) in
  QCheck.map
    (fun (tez, kit) ->
       make_burrow_for_test
         ~owner:bob_addr
         ~delegate:None
         ~active:true
         ~collateral:tez
         ~outstanding_kit:kit
         ~excess_kit:kit_zero
         ~adjustment_index:(compute_adjustment_index params)
         ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
         ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    )
    arb_smart_tez_kit

(*
Other properties
~~~~~~~~~~~~~~~~
* What about the relation between liquidatable and optimistically overburrowed?
* No interaction with the burrow has any effect if it's inactive. Actually, we
  have to discuss exactly which operations we wish to allow when the burrow is
  inactive.
*)

let params : parameters =
  { q = fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "1015") (Ligo.int_from_literal "1000"));
    index = Ligo.tez_from_literal "320_000mutez";
    protected_index = Ligo.tez_from_literal "360_000mutez";
    target = fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "108") (Ligo.int_from_literal "100"));
    drift = fixedpoint_zero;
    drift_derivative = fixedpoint_zero;
    burrow_fee_index = fixedpoint_one;
    imbalance_index = fixedpoint_one;
    outstanding_kit = kit_one;
    circulating_kit = kit_one;
    last_touched = Ligo.timestamp_from_seconds_literal 0;
  }

(* If a burrow is liquidatable, then it is also overburrowed. *)
let liquidatable_implies_overburrowed =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"liquidatable_implies_overburrowed"
    ~count:property_test_count
    (arbitrary_burrow params)
  @@ fun burrow ->
  (* several cases fail the premise but we we have quite some cases
   * succeeding as well, so it should be okay. *)
  QCheck.(
    burrow_is_liquidatable params burrow
    ==> burrow_is_overburrowed params burrow
  )

(* If a burrow is optimistically_overburrowed, then it is also overburrowed. *)
let optimistically_overburrowed_implies_overburrowed =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"optimistically_overburrowed_implies_overburrowed"
    ~count:property_test_count
    (arbitrary_burrow params)
  @@ fun burrow ->
  QCheck.(
    burrow_is_optimistically_overburrowed params burrow
    ==> burrow_is_overburrowed params burrow
  )

(* If a liquidation was deemed Partial:
 * - is_liquidatable is true for the given burrow
 * - is_overburrowed is true for the given burrow
 * - is_liquidatable is false for the resulting burrow
 * - is_overburrowed is true for the resulting burrow
 * - is_optimistically_overburrowed is false for the resulting burrow
 * - old_collateral = new_collateral + tez_to_auction + liquidation_reward
 * - old_collateral_at_auction = new_collateral_at_auction - tez_to_auction
 * - the resulting burrow is active
 * - TODO: If the auctioned tez gets sold with the current expected price, the
 *         burrow would be non-overburrowed, non-liquidatable,
 *         non-optimistically-overburrowed.
*)
let assert_properties_of_partial_liquidation params burrow_in details =
  let burrow_out = details.burrow_state in
  assert_bool
    "partial liquidation means overburrowed input burrow"
    (burrow_is_overburrowed params burrow_in);
  assert_bool
    "partial liquidation means liquidatable input burrow"
    (burrow_is_liquidatable params burrow_in);
  assert_bool
    "partial liquidation means non-liquidatable output burrow"
    (not (burrow_is_liquidatable params burrow_out));
  assert_bool
    "partial liquidation means overburrowed output burrow"
    (burrow_is_overburrowed params burrow_out);
  assert_bool
    "partial liquidation means non-optimistically-overburrowed output burrow"
    (not (burrow_is_optimistically_overburrowed params burrow_out));
  assert_equal
    (burrow_collateral burrow_in)
    (Ligo.add_tez_tez (Ligo.add_tez_tez (burrow_collateral burrow_out) details.tez_to_auction) details.liquidation_reward)
    ~printer:Ligo.string_of_tez;
  assert_equal
    (burrow_collateral_at_auction burrow_in)
    (Ligo.sub_tez_tez (burrow_collateral_at_auction burrow_out) details.tez_to_auction)
    ~printer:Ligo.string_of_tez;
  assert_bool
    "partial liquidation does not deactivate burrows"
    (burrow_active burrow_out)

(* If a liquidation was deemed Complete:
 * - is_liquidatable is true for the given burrow
 * - is_overburrowed is true for the given burrow
 * - is_liquidatable is true for the resulting burrow
 * - is_overburrowed is true for the resulting burrow
 * - is_optimistically_overburrowed is true for the resulting burrow
 * - old_collateral = new_collateral + tez_to_auction + liquidation_reward
 * - old_collateral_at_auction = new_collateral_at_auction - tez_to_auction
 * - the resulting burrow has no collateral
 * - the resulting burrow is active
 * - TODO: If the auctioned tez gets sold with the current expected price, the
 *         burrow would be overburrowed. Would it be liquidatable and
 *         optimistically-overburrowed though???
*)
let assert_properties_of_complete_liquidation params burrow_in details =
  let burrow_out = details.burrow_state in
  assert_bool
    "complete liquidation means liquidatable input burrow"
    (burrow_is_liquidatable params burrow_in);
  assert_bool
    "complete liquidation means overburrowed input burrow"
    (burrow_is_overburrowed params burrow_in);
  assert_bool
    "complete liquidation means liquidatable output burrow"
    (burrow_is_liquidatable params burrow_out);
  assert_bool
    "complete liquidation means overburrowed output burrow"
    (burrow_is_overburrowed params burrow_out);
  assert_bool
    "complete liquidation means optimistically-overburrowed output burrow"
    (burrow_is_optimistically_overburrowed params burrow_out);
  assert_bool
    "complete liquidation means no collateral in the output burrow"
    (burrow_collateral burrow_out = (Ligo.tez_from_literal "0mutez"));
  assert_equal
    (burrow_collateral burrow_in)
    (Ligo.add_tez_tez (Ligo.add_tez_tez (burrow_collateral burrow_out) details.tez_to_auction) details.liquidation_reward)
    ~printer:Ligo.string_of_tez;
  assert_equal
    (burrow_collateral_at_auction burrow_in)
    (Ligo.sub_tez_tez (burrow_collateral_at_auction burrow_out) details.tez_to_auction)
    ~printer:Ligo.string_of_tez;
  assert_bool
    "complete liquidation does not deactivate burrows"
    (burrow_active burrow_out)

(* If a liquidation was deemed Close:
 * - is_overburrowed is true for the given burrow
 * - is_liquidatable is true for the given burrow
 * - the resulting burrow is overburrowed
 * - the resulting burrow is not liquidatable (is inactive; no more rewards)
 * - the resulting burrow has no collateral
 * - the resulting burrow is inactive
 * - old_collateral + creation_deposit = new_collateral + tez_to_auction + liquidation_reward
 * - old_collateral_at_auction = new_collateral_at_auction - tez_to_auction
 * - TODO: What would happen if the auctioned tez got sold with the current
 *         expected price? Would it be overburrowed?
 *         optimistically-overburrowed? liquidatable?
*)
let assert_properties_of_close_liquidation params burrow_in details =
  let burrow_out = details.burrow_state in
  assert_bool
    "close liquidation means overburrowed input burrow"
    (burrow_is_overburrowed params burrow_in);
  assert_bool
    "close liquidation means liquidatable input burrow"
    (burrow_is_liquidatable params burrow_in);
  assert_bool
    "close liquidation means overburrowed output burrow"
    (burrow_is_overburrowed params burrow_out);
  assert_bool
    "close liquidation means non-liquidatable output burrow"
    (not (burrow_is_liquidatable params burrow_out));
  assert_bool
    "close liquidation means no collateral in the output burrow"
    (burrow_collateral burrow_out = (Ligo.tez_from_literal "0mutez"));
  assert_bool
    "close liquidation means inactive output burrow"
    (not (burrow_active burrow_out));
  assert_equal
    (Ligo.add_tez_tez (burrow_collateral burrow_in) Constants.creation_deposit)
    (Ligo.add_tez_tez (Ligo.add_tez_tez (burrow_collateral burrow_out) details.tez_to_auction) details.liquidation_reward)
    ~printer:Ligo.string_of_tez;
  assert_equal
    (burrow_collateral_at_auction burrow_in)
    (Ligo.sub_tez_tez (burrow_collateral_at_auction burrow_out) details.tez_to_auction)
    ~printer:Ligo.string_of_tez

let test_general_liquidation_properties =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_general_liquidation_properties"
    ~count:property_test_count
    (arbitrary_burrow params)
  @@ fun burrow ->
  match burrow_request_liquidation params burrow with
  (* If a liquidation was deemed Unnecessary then is_liquidatable
   * must be false for the input burrow. *)
  | None ->
    assert_bool
      "unnecessary liquidation means non-liquidatable input burrow"
      (not (burrow_is_liquidatable params burrow));
    true
  | Some (Partial, details) ->
    assert_properties_of_partial_liquidation params burrow details; true
  | Some (Complete, details) ->
    assert_properties_of_complete_liquidation params burrow details; true
  | Some (Close, details) ->
    assert_properties_of_close_liquidation params burrow details; true

let initial_burrow =
  make_burrow_for_test
    ~owner:bob_addr
    ~delegate:None
    ~active:true
    ~collateral:(Ligo.tez_from_literal "10_000_000mutez")
    ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "20_000_000n"))
    ~excess_kit:kit_zero
    ~adjustment_index:(compute_adjustment_index params)
    ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
    ~last_touched:(Ligo.timestamp_from_seconds_literal 0)

(* Minimum amount of collateral for the burrow to be considered collateralized. *)
let barely_not_overburrowed_test =
  "barely_not_overburrowed_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "7_673_400mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is not overburrowed" (not (burrow_is_overburrowed params burrow));
    assert_bool "is not optimistically overburrowed" (not (burrow_is_optimistically_overburrowed params burrow));
    assert_bool "is not liquidatable" (not (burrow_is_liquidatable params burrow));

    let expected_liquidation_result = None (* Unnecessary *) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result

(* Maximum amount of collateral for the burrow to be considered
 * under-collateralized, but not liquidatable. *)
let barely_overburrowed_test =
  "barely_overburrowed_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "7_673_399mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is not liquidatable" (not (burrow_is_liquidatable params burrow));

    let expected_liquidation_result = None (* Unnecessary *) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result

(* Minimum amount of collateral for the burrow to be considered
 * under-collateralized, but not liquidatable. *)
let barely_non_liquidatable_test =
  "barely_non_liquidatable_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "6_171_200mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is not liquidatable" (not (burrow_is_liquidatable params burrow));

    let expected_liquidation_result = None (* Unnecessary *) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result

(* Maximum amount of collateral for the burrow to be considered partially
 * liquidatable. *)
let barely_liquidatable_test =
  "barely_liquidatable_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "6_171_199mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (burrow_is_liquidatable params burrow);

    let expected_liquidation_result =
      Some
        ( Partial,
          { liquidation_reward = Ligo.tez_from_literal "1_006_171mutez";
            tez_to_auction = Ligo.tez_from_literal "2_818_396mutez";
            burrow_state =
              make_burrow_for_test
                ~active:true
                ~owner:bob_addr
                ~delegate:None
                ~collateral:(Ligo.tez_from_literal "2_346_632mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:fixedpoint_one
                ~collateral_at_auction:(Ligo.tez_from_literal "2_818_396mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Complete, _) | Some (Close, _) -> failwith "impossible"
      | Some (Partial, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "8_677_329n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "467912067393300348926951424";
        den = Ligo.int_from_literal "67404402845334701604000000";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_properties_of_partial_liquidation params burrow details

(* Minimum amount of collateral for the burrow to be considered partially
 * liquidatable, but a candidate for collateral depletion (the collateral is
 * depleted of course, but at least it looks like once kit is received from
 * auctions things will return to normal). *)
let barely_non_complete_liquidatable_test =
  "barely_non_complete_liquidatable_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "5_065_065mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (burrow_is_liquidatable params burrow);

    let expected_liquidation_result =
      Some
        ( Partial,
          { liquidation_reward = Ligo.tez_from_literal "1_005_065mutez";
            tez_to_auction = Ligo.tez_from_literal "4_060_000mutez";
            burrow_state =
              make_burrow_for_test
                ~active:true
                ~owner:bob_addr
                ~delegate:None
                ~collateral:(Ligo.tez_from_literal "0mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:fixedpoint_one
                ~collateral_at_auction:(Ligo.tez_from_literal "4_060_000mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Complete, _) | Some (Close, _) -> failwith "impossible"
      | Some (Partial, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "15_229_815n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "67404402845334701604864";
        den = Ligo.int_from_literal "6740440284533470160400";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_properties_of_partial_liquidation params burrow details

(* Maximum amount of collateral for the burrow to be liquidatable in a way thay
 * recovery seems impossible. *)
let barely_complete_liquidatable_test =
  "barely_complete_liquidatable_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "5_065_064mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (burrow_is_liquidatable params burrow);

    let expected_liquidation_result =
      Some
        ( Complete,
          { liquidation_reward = Ligo.tez_from_literal "1_005_065mutez";
            tez_to_auction = Ligo.tez_from_literal "4_059_999mutez";
            burrow_state =
              make_burrow_for_test
                ~active:true
                ~owner:bob_addr
                ~delegate:None
                ~collateral:(Ligo.tez_from_literal "0mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:fixedpoint_one
                ~collateral_at_auction:(Ligo.tez_from_literal "4_059_999mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Partial, _) | Some (Close, _) -> failwith "impossible"
      | Some (Complete, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "15_229_814n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "674043862432650352662675456";
        den = Ligo.int_from_literal "67404402845334701604000000";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_properties_of_complete_liquidation params burrow details

(* Minimum amount of collateral for the burrow to be liquidatable in a way thay
 * recovery seems impossible, but without having to deactivate it. *)
let barely_non_close_liquidatable_test =
  "barely_non_close_liquidatable_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "1_001_001mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (burrow_is_liquidatable params burrow);

    let expected_liquidation_result =
      Some
        ( Complete,
          { liquidation_reward = Ligo.tez_from_literal "1_001_001mutez";
            tez_to_auction = (Ligo.tez_from_literal "0mutez");
            burrow_state =
              make_burrow_for_test
                ~active:true
                ~owner:bob_addr
                ~delegate:None
                ~collateral:(Ligo.tez_from_literal "0mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:fixedpoint_one
                ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Partial, _) | Some (Close, _) -> failwith "impossible"
      | Some (Complete, details) -> details in

    let expected_min_kit_for_unwarranted = kit_zero in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit = zero_ratio in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_properties_of_complete_liquidation params burrow details

(* Maximum amount of collateral for the burrow to be liquidatable and have to
 * be deactivated. *)
let barely_close_liquidatable_test =
  "barely_close_liquidatable_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "1_001_000mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in
    assert_bool "is overburrowed" (burrow_is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (burrow_is_liquidatable params burrow);

    let expected_liquidation_result =
      Some
        ( Close,
          { liquidation_reward = Ligo.tez_from_literal "1_001_001mutez";
            tez_to_auction = Ligo.tez_from_literal "999_999mutez";
            burrow_state =
              make_burrow_for_test
                ~active:false
                ~owner:bob_addr
                ~delegate:None
                ~collateral:(Ligo.tez_from_literal "0mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:fixedpoint_one
                ~collateral_at_auction:(Ligo.tez_from_literal "999_999mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in
    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Partial, _) | Some (Complete, _) -> failwith "impossible"
      | Some (Close, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "18_981_000n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "166020530642689301158035456";
        den = Ligo.int_from_literal "67404402845334701604000000";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_properties_of_close_liquidation params burrow details

let unwarranted_liquidation_unit_test =
  "unwarranted_liquidation_unit_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "7_673_400mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "10_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in

    assert_bool "is not overburrowed" (not (burrow_is_overburrowed params burrow));
    assert_bool "is not optimistically overburrowed" (not (burrow_is_optimistically_overburrowed params burrow));
    assert_bool "is not liquidatable" (not (burrow_is_liquidatable params burrow));

    let liquidation_result = burrow_request_liquidation params burrow in
    assert_equal None (* Unnecessary *) liquidation_result ~printer:show_liquidation_result

let partial_liquidation_unit_test =
  "partial_liquidation_unit_test" >:: fun _ ->
    let burrow = initial_burrow in

    let expected_liquidation_result =
      Some
        ( Partial,
          { liquidation_reward = Ligo.add_tez_tez Constants.creation_deposit (Ligo.tez_from_literal "10_000mutez");
            tez_to_auction = Ligo.tez_from_literal "7_142_472mutez";
            burrow_state =
              make_burrow_for_test
                ~owner:bob_addr
                ~delegate:None
                ~active:true
                ~collateral:(Ligo.tez_from_literal "1_847_528mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "20_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:(compute_adjustment_index params)
                ~collateral_at_auction:(Ligo.tez_from_literal "7_142_472mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in

    let liquidation_result = burrow_request_liquidation params burrow in

    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Complete, _) | Some (Close, _) -> failwith "impossible"
      | Some (Partial, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "27_141_394n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "1185798177338727676948512768";
        den = Ligo.int_from_literal "67404402845334701604000000";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_bool "is optimistically overburrowed" (burrow_is_optimistically_overburrowed params burrow);
    assert_properties_of_partial_liquidation params burrow details

let complete_liquidation_unit_test =
  "complete_liquidation_unit_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "10_000_000mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "100_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in

    let expected_liquidation_result =
      Some
        ( Complete,
          { liquidation_reward = Ligo.add_tez_tez Constants.creation_deposit (Ligo.tez_from_literal "10_000mutez");
            tez_to_auction = Ligo.tez_from_literal "8_990_000mutez";
            burrow_state =
              make_burrow_for_test
                ~owner:bob_addr
                ~delegate:None
                ~active:true
                ~collateral:(Ligo.tez_from_literal "0mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "100_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:(compute_adjustment_index params)
                ~collateral_at_auction:(Ligo.tez_from_literal "8_990_000mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in

    let liquidation_result = burrow_request_liquidation params burrow in

    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Partial, _) | Some (Close, _) -> failwith "impossible"
      | Some (Complete, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "170_810_000n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "149252606300383982125056";
        den = Ligo.int_from_literal "6740440284533470160400";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_bool
      "input burrow is optimistically overburrowed"
      (burrow_is_optimistically_overburrowed params burrow);
    assert_properties_of_complete_liquidation params burrow details

let complete_and_close_liquidation_test =
  "complete_and_close_liquidation_test" >:: fun _ ->
    let burrow =
      make_burrow_for_test
        ~owner:bob_addr
        ~delegate:None
        ~active:true
        ~collateral:(Ligo.tez_from_literal "1_000_000mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "100_000_000n"))
        ~excess_kit:kit_zero
        ~adjustment_index:(compute_adjustment_index params)
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
    in

    let expected_liquidation_result =
      Some
        ( Close,
          { liquidation_reward = Ligo.add_tez_tez Constants.creation_deposit (Ligo.tez_from_literal "1_000mutez");
            tez_to_auction = Ligo.tez_from_literal "999_000mutez";
            burrow_state =
              make_burrow_for_test
                ~owner:bob_addr
                ~delegate:None
                ~active:false
                ~collateral:(Ligo.tez_from_literal "0mutez")
                ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "100_000_000n"))
                ~excess_kit:kit_zero
                ~adjustment_index:(compute_adjustment_index params)
                ~collateral_at_auction:(Ligo.tez_from_literal "999_000mutez")
                ~last_touched:(Ligo.timestamp_from_seconds_literal 0)
          }
        ) in

    let liquidation_result = burrow_request_liquidation params burrow in

    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:show_liquidation_result;

    let details = match liquidation_result with
      | None | Some (Partial, _) | Some (Complete, _) -> failwith "impossible"
      | Some (Close, details) -> details in

    let expected_min_kit_for_unwarranted = kit_of_mukit (Ligo.nat_from_literal "189_810_000n") in
    assert_equal
      ~printer:show_kit_option
      (Some expected_min_kit_for_unwarranted)
      (compute_min_kit_for_unwarranted params burrow details.tez_to_auction);

    let expected_expected_kit =
      { num = Ligo.int_from_literal "165854675966722578579456";
        den = Ligo.int_from_literal "67404402845334701604000";
      } in
    let expected_kit = compute_expected_kit params details.tez_to_auction in

    assert_equal
      ~printer:show_ratio
      ~cmp:eq_ratio_ratio
      expected_expected_kit
      expected_kit;

    assert_bool
      "input burrow is optimistically overburrowed"
      (burrow_is_optimistically_overburrowed params burrow);
    assert_bool
      "output burrow is optimistically overburrowed"
      (burrow_is_optimistically_overburrowed params details.burrow_state);
    assert_properties_of_close_liquidation params burrow details

let test_burrow_request_liquidation_invariant_close =
  let upper_collat_bound_for_test = 1_001_000 in
  (* upper_collat_bound_for_test / 1.9 + 1 *)
  let kit_to_allow_liquidation = kit_of_mukit (Ligo.nat_from_literal "526_843n") in
  let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(0 -- upper_collat_bound_for_test) in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"burrow_request_liquidation - burrow returned in case 2a (close burrow) obeys burrow invariants"
    ~count:property_test_count
    arb_tez
  @@ fun collateral ->

  let burrow0 = make_burrow_for_test
      ~owner:bob_addr
      ~delegate:None
      ~active:true
      ~collateral:collateral
      ~outstanding_kit:kit_to_allow_liquidation
      ~excess_kit:kit_zero
      ~adjustment_index:(compute_adjustment_index params)
      ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
      ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

  let liquidation_details = match Burrow.burrow_request_liquidation initial_parameters burrow0 with
    | Some (Burrow.Close, liquidation_details) -> liquidation_details
    | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
    | Some (liquidation_type, _) -> failwith (Format.sprintf "liquidation_type returned by burrow_request_liquidation was %s but Close was expected" (Burrow.show_liquidation_type liquidation_type))
  in

  Burrow.assert_burrow_invariants liquidation_details.burrow_state;
  assert_properties_of_close_liquidation initial_parameters burrow0 liquidation_details;
  true

let test_burrow_request_liquidation_invariant_complete =
  (* 1 / liquidation_reward * creation_deposit *)
  let lower_collat_bound_for_test = 1_001_001 in
  let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(lower_collat_bound_for_test -- max_int) in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"burrow_request_liquidation - burrow returned in case 2b (liquidate all collateral) obeys burrow invariants"
    ~count:property_test_count
    (QCheck.pair arb_tez TestArbitrary.arb_kit)
  @@ fun (collateral, extra_kit)->

  (* (999 / 1000 collat - creation_deposit) - 1/10 * (999 / 1000 collat - creation_deposit) + 1 *)
  (* Note: the math below is just a simplified version of the above expression *)
  let min_kit_to_trigger_case = Ligo.sub_int_int
      (Common.cdiv_int_int
         (Ligo.mul_int_int (Ligo.int_from_literal "8_991") (Common.tez_to_mutez collateral))
         (Ligo.int_from_literal "10_000"))
      (Ligo.int_from_literal "899_999") in
  let outstanding_kit = match Ligo.is_nat min_kit_to_trigger_case with
    | Some n -> kit_add (kit_of_mukit n) extra_kit
    | None -> failwith "The calculated outstanding_kit for the test case was not a nat"
  in
  let burrow0 = make_burrow_for_test
      ~owner:bob_addr
      ~delegate:None
      ~active:true
      ~collateral:collateral
      ~outstanding_kit:outstanding_kit
      ~excess_kit:kit_zero
      ~adjustment_index:(compute_adjustment_index params)
      ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
      ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

  let liquidation_details = match Burrow.burrow_request_liquidation initial_parameters burrow0 with
    | Some (Burrow.Complete, liquidation_details) -> liquidation_details
    | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
    | Some (liquidation_type, _) -> failwith (Format.sprintf "liquidation_type returned by burrow_request_liquidation was %s but Complete was expected" (Burrow.show_liquidation_type liquidation_type))
  in
  Burrow.assert_burrow_invariants liquidation_details.burrow_state;
  assert_properties_of_complete_liquidation initial_parameters burrow0 liquidation_details;
  true

let test_burrow_request_liquidation_invariant_partial =
  (* Holding collateral constant and varying kit since both bounds of the range of outstanding_kit to trigger this case
   * depend on the collateral. *)
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

  let burrow0 = make_burrow_for_test
      ~owner:bob_addr
      ~delegate:None
      ~active:true
      ~collateral:(Ligo.tez_from_literal (string_of_int collateral ^ "mutez"))
      ~outstanding_kit:outstanding_kit
      ~excess_kit:kit_zero
      ~adjustment_index:(compute_adjustment_index params)
      ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
      ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

  let liquidation_details = match Burrow.burrow_request_liquidation initial_parameters burrow0 with
    | Some (Burrow.Partial, liquidation_details) -> liquidation_details
    | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
    | Some (liquidation_type, _) -> failwith (Format.sprintf "liquidation_type returned by burrow_request_liquidation was %s but Partial was expected" (Burrow.show_liquidation_type liquidation_type))
  in
  Burrow.assert_burrow_invariants liquidation_details.burrow_state;
  assert_properties_of_partial_liquidation initial_parameters burrow0 liquidation_details;
  true


let test_burrow_request_liquidation_preserves_tez =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"burrow_request_liquidation - total tez is preserved"
    ~count:property_test_count
    (arbitrary_burrow initial_parameters)
  @@ fun burrow0 ->
  let _ = match Burrow.burrow_request_liquidation initial_parameters burrow0 with
    | None -> ()
    | Some (_, liquidation_details) ->
      let tez_in = burrow_total_associated_tez burrow0 in
      let tez_out = Ligo.add_tez_tez liquidation_details.liquidation_reward (burrow_total_associated_tez liquidation_details.burrow_state) in

      assert (Ligo.eq_tez_tez tez_in tez_out);
      (* Also check that tez_to_auction is exactly reflected in collateral_at_auction *)
      assert (Ligo.eq_tez_tez
                (Burrow.burrow_collateral_at_auction burrow0)
                (Ligo.sub_tez_tez
                   (Burrow.burrow_collateral_at_auction liquidation_details.burrow_state)
                   liquidation_details.tez_to_auction
                )
             );
  in
  true

let regression_test_72 =
  "regression_test_72" >:: fun _ ->
    let burrow0 =
      Burrow.make_burrow_for_test
        ~collateral:(Ligo.tez_from_literal "4369345928872593390mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "3928478924648448718n"))
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~excess_kit:kit_zero
        ~active:true
        ~owner:bob_addr
        ~delegate:None
        ~adjustment_index:fixedpoint_one
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

    let liquidation_details = match Burrow.burrow_request_liquidation Parameters.initial_parameters burrow0 with
      | Some (Burrow.Complete, liquidation_details) -> liquidation_details
      | None -> failwith "liquidation_result returned by burrow_request_liquidation was None but the test expects a value."
      | Some (liquidation_type, _) -> failwith (Format.sprintf "liquidation_type returned by burrow_request_liquidation was %s but Complete was expected" (Burrow.show_liquidation_type liquidation_type))
    in
    assert_properties_of_complete_liquidation Parameters.initial_parameters burrow0 liquidation_details

let regression_test_93 =
  "regression_test_93" >:: fun _ ->
    let burrow_in =
      Burrow.make_burrow_for_test
        ~collateral:(Ligo.tez_from_literal "4369345928872593390mutez")
        ~outstanding_kit:(kit_of_mukit (Ligo.nat_from_literal "3928478924648448718n"))
        ~collateral_at_auction:(Ligo.tez_from_literal "0mutez")
        ~excess_kit:kit_zero
        ~active:true
        ~owner:bob_addr
        ~delegate:None
        ~adjustment_index:fixedpoint_one
        ~last_touched:(Ligo.timestamp_from_seconds_literal 0) in

    (* First liquidation must be complete *)
    let liquidation_details = match Burrow.burrow_request_liquidation Parameters.initial_parameters burrow_in with
      | Some (Burrow.Complete, liquidation_details) -> liquidation_details
      | liquidation_result -> assert_failure ("Unexpected liquidation result: " ^ Burrow.show_liquidation_result liquidation_result)
    in
    assert_properties_of_complete_liquidation Parameters.initial_parameters burrow_in liquidation_details;
    (* The following line must succeed. *)
    let _ = compute_min_kit_for_unwarranted Parameters.initial_parameters burrow_in liquidation_details.tez_to_auction in

    (* Second liquidation must be close *)
    let burrow_in = liquidation_details.burrow_state in
    let liquidation_details = match Burrow.burrow_request_liquidation Parameters.initial_parameters burrow_in with
      | Some (Burrow.Close, liquidation_details) -> liquidation_details
      | liquidation_result -> assert_failure ("Unexpected liquidation result: " ^ Burrow.show_liquidation_result liquidation_result)
    in
    assert_properties_of_close_liquidation Parameters.initial_parameters burrow_in liquidation_details;
    assert_bool
      "For this test to be potent the collateral should have been zero"
      (Ligo.eq_tez_tez (burrow_collateral burrow_in) (Ligo.tez_from_literal "0mutez"));
    (* The following line must succeed. *)
    let _ = compute_min_kit_for_unwarranted Parameters.initial_parameters burrow_in liquidation_details.tez_to_auction in
    ()

let suite =
  "LiquidationTests" >::: [
    partial_liquidation_unit_test;
    unwarranted_liquidation_unit_test;
    complete_liquidation_unit_test;
    complete_and_close_liquidation_test;

    (* Test the boundaries *)
    barely_not_overburrowed_test;
    barely_overburrowed_test;
    barely_non_liquidatable_test;
    barely_liquidatable_test;
    barely_non_complete_liquidatable_test;
    barely_complete_liquidatable_test;
    barely_non_close_liquidatable_test;
    barely_close_liquidatable_test;

    (* General, property-based random tests *)
    liquidatable_implies_overburrowed;
    optimistically_overburrowed_implies_overburrowed;
    test_burrow_request_liquidation_preserves_tez;

    (* General, property-based random tests regarding liquidation calculations. *)
    test_general_liquidation_properties;

    (* General, property-based random tests for checking that burrow_request_liquidation
     * preserves invariants. *)
    test_burrow_request_liquidation_invariant_close;
    test_burrow_request_liquidation_invariant_complete;
    test_burrow_request_liquidation_invariant_partial;

    (* Regression tests *)
    regression_test_72;
    regression_test_93;
  ]
