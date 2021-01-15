open Burrow
open OUnit2

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

(* Create an arbitrary burrow state, given the set of checker's parameters (NB:
 * most values are fixed). *)
let arbitrary_burrow (params: Parameters.t) =
  (* More likely to give Close/Unnecessary ones *)
  let arb_smart_tez_kit_1 =
    let positive_int = QCheck.(1 -- max_int) in
    QCheck.map
      (fun (t, k, factor) ->
         let tez = Tez.of_ratio_floor (Ratio.div (Ratio.of_int t) (Ratio.mul (Ratio.of_int 2) (Ratio.of_int factor))) in
         let kit = Kit.of_ratio_floor (Ratio.div (Ratio.of_int k) (Ratio.of_int factor)) in
         (tez, kit)
      )
      (QCheck.triple positive_int positive_int positive_int) in
  (* More likely to give Complete/Partial/Unnecessary ones *)
  let arb_smart_tez_kit_2 =
    QCheck.map
      (fun (tez, kit) ->
         let tez = Tez.of_ratio_floor (Ratio.div (Tez.to_ratio tez) (Ratio.of_int 2)) in
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
       Burrow.make_for_test
         ~permission_version:0
         ~allow_all_tez_deposits:false
         ~allow_all_kit_burnings:false
         ~delegate:None
         ~active:true
         ~collateral:tez
         ~outstanding_kit:kit
         ~excess_kit:Kit.zero
         ~adjustment_index:(Parameters.compute_adjustment_index params)
         ~collateral_at_auction:Tez.zero
         ~last_touched:(Timestamp.of_seconds 0)
         ~liquidation_slices:None
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

let params : Parameters.t =
  { q = FixedPoint.of_ratio_floor (Ratio.make (Ligo.int_from_literal 1015) (Ligo.int_from_literal 1000));
    index = Tez.of_mutez (Ligo.int_from_literal 320_000);
    protected_index = Tez.of_mutez (Ligo.int_from_literal 360_000);
    target = FixedPoint.of_ratio_floor (Ratio.make (Ligo.int_from_literal 108) (Ligo.int_from_literal 100));
    drift = FixedPoint.zero;
    drift_derivative = FixedPoint.zero;
    burrow_fee_index = FixedPoint.one;
    imbalance_index = FixedPoint.one;
    outstanding_kit = Kit.one;
    circulating_kit = Kit.one;
    last_touched = Timestamp.of_seconds 0;
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
    Burrow.is_liquidatable params burrow
    ==> Burrow.is_overburrowed params burrow
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
    Burrow.is_optimistically_overburrowed params burrow
    ==> Burrow.is_overburrowed params burrow
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
let assert_properties_of_partial_liquidation burrow_in details =
  let burrow_out = details.burrow_state in
  assert_bool
    "partial liquidation means overburrowed input burrow"
    (Burrow.is_overburrowed params burrow_in);
  assert_bool
    "partial liquidation means liquidatable input burrow"
    (Burrow.is_liquidatable params burrow_in);
  assert_bool
    "partial liquidation means non-liquidatable output burrow"
    (not (Burrow.is_liquidatable params burrow_out));
  assert_bool
    "partial liquidation means overburrowed output burrow"
    (Burrow.is_overburrowed params burrow_out);
  assert_bool
    "partial liquidation means non-optimistically-overburrowed output burrow"
    (not (Burrow.is_optimistically_overburrowed params burrow_out));
  assert_equal
    (Burrow.collateral burrow_in)
    (Tez.add (Tez.add (Burrow.collateral burrow_out) details.tez_to_auction) details.liquidation_reward)
    ~printer:Tez.show;
  assert_equal
    (Burrow.collateral_at_auction burrow_in)
    (Tez.sub (Burrow.collateral_at_auction burrow_out) details.tez_to_auction)
    ~printer:Tez.show;
  assert_bool
    "partial liquidation does not deactivate burrows"
    (Burrow.active burrow_out)

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
let assert_properties_of_complete_liquidation burrow_in details =
  let burrow_out = details.burrow_state in
  assert_bool
    "complete liquidation means liquidatable input burrow"
    (Burrow.is_liquidatable params burrow_in);
  assert_bool
    "complete liquidation means overburrowed input burrow"
    (Burrow.is_overburrowed params burrow_in);
  assert_bool
    "complete liquidation means liquidatable output burrow"
    (Burrow.is_liquidatable params burrow_out);
  assert_bool
    "complete liquidation means overburrowed output burrow"
    (Burrow.is_overburrowed params burrow_out);
  assert_bool
    "complete liquidation means optimistically-overburrowed output burrow"
    (Burrow.is_optimistically_overburrowed params burrow_out);
  assert_bool
    "complete liquidation means no collateral in the output burrow"
    (Burrow.collateral burrow_out = Tez.zero);
  assert_equal
    (Burrow.collateral burrow_in)
    (Tez.add (Tez.add (Burrow.collateral burrow_out) details.tez_to_auction) details.liquidation_reward)
    ~printer:Tez.show;
  assert_equal
    (Burrow.collateral_at_auction burrow_in)
    (Tez.sub (Burrow.collateral_at_auction burrow_out) details.tez_to_auction)
    ~printer:Tez.show;
  assert_bool
    "complete liquidation does not deactivate burrows"
    (Burrow.active burrow_out)

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
let assert_properties_of_close_liquidation burrow_in details =
  let burrow_out = details.burrow_state in
  assert_bool
    "close liquidation means overburrowed input burrow"
    (Burrow.is_overburrowed params burrow_in);
  assert_bool
    "close liquidation means liquidatable input burrow"
    (Burrow.is_liquidatable params burrow_in);
  assert_bool
    "close liquidation means overburrowed output burrow"
    (Burrow.is_overburrowed params burrow_out);
  assert_bool
    "close liquidation means non-liquidatable output burrow"
    (not (Burrow.is_liquidatable params burrow_out));
  assert_bool
    "close liquidation means no collateral in the output burrow"
    (Burrow.collateral burrow_out = Tez.zero);
  assert_bool
    "close liquidation means inactive output burrow"
    (not (Burrow.active burrow_out));
  assert_equal
    (Tez.add (Burrow.collateral burrow_in) Constants.creation_deposit)
    (Tez.add (Tez.add (Burrow.collateral burrow_out) details.tez_to_auction) details.liquidation_reward)
    ~printer:Tez.show;
  assert_equal
    (Burrow.collateral_at_auction burrow_in)
    (Tez.sub (Burrow.collateral_at_auction burrow_out) details.tez_to_auction)
    ~printer:Tez.show

let test_general_liquidation_properties =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_general_liquidation_properties"
    ~count:property_test_count
    (arbitrary_burrow params)
  @@ fun burrow ->
  match Burrow.request_liquidation params burrow with
  (* If a liquidation was deemed Unnecessary then is_liquidatable
   * must be false for the input burrow. *)
  | Unnecessary ->
    assert_bool
      "unnecessary liquidation means non-liquidatable input burrow"
      (not (Burrow.is_liquidatable params burrow));
    true
  | Partial details ->
    assert_properties_of_partial_liquidation burrow details; true
  | Complete details ->
    assert_properties_of_complete_liquidation burrow details; true
  | Close details ->
    assert_properties_of_close_liquidation burrow details; true

let initial_burrow =
  Burrow.make_for_test
    ~permission_version:0
    ~allow_all_tez_deposits:false
    ~allow_all_kit_burnings:false
    ~delegate:None
    ~active:true
    ~collateral:(Tez.of_mutez (Ligo.int_from_literal 10_000_000))
    ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 20_000_000))
    ~excess_kit:Kit.zero
    ~adjustment_index:(Parameters.compute_adjustment_index params)
    ~collateral_at_auction:Tez.zero
    ~last_touched:(Timestamp.of_seconds 0)
    ~liquidation_slices:None

(* Minimum amount of collateral for the burrow to be considered collateralized. *)
let barely_not_overburrowed_test =
  "barely_not_overburrowed_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 7_673_400))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is not overburrowed" (not (Burrow.is_overburrowed params burrow));
    assert_bool "is not optimistically overburrowed" (not (Burrow.is_optimistically_overburrowed params burrow));
    assert_bool "is not liquidatable" (not (Burrow.is_liquidatable params burrow));

    let expected_liquidation_result = Unnecessary in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result

(* Maximum amount of collateral for the burrow to be considered
 * under-collateralized, but not liquidatable. *)
let barely_overburrowed_test =
  "barely_overburrowed_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 7_673_399))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is not liquidatable" (not (Burrow.is_liquidatable params burrow));

    let expected_liquidation_result = Unnecessary in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result

(* Minimum amount of collateral for the burrow to be considered
 * under-collateralized, but not liquidatable. *)
let barely_non_liquidatable_test =
  "barely_non_liquidatable_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 6_171_200))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is not liquidatable" (not (Burrow.is_liquidatable params burrow));

    let expected_liquidation_result = Unnecessary in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result

(* Maximum amount of collateral for the burrow to be considered partially
 * liquidatable. *)
let barely_liquidatable_test =
  "barely_liquidatable_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 6_171_199))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

    let expected_liquidation_result =
      Partial
        { liquidation_reward = Tez.of_mutez (Ligo.int_from_literal 1_006_171);
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 2_818_396);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 6_941_863);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 8_677_329);
          burrow_state =
            Burrow.make_for_test
              ~active:true
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~collateral:(Tez.of_mutez (Ligo.int_from_literal 2_346_632))
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:FixedPoint.one
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 2_818_396))
              ~liquidation_slices:None
              ~last_touched:(Timestamp.of_seconds 0)
        } in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Complete _ | Close _ -> failwith "impossible"
      | Partial details -> details in
    assert_properties_of_partial_liquidation burrow details

(* Minimum amount of collateral for the burrow to be considered partially
 * liquidatable, but a candidate for collateral depletion (the collateral is
 * depleted of course, but at least it looks like once kit is received from
 * auctions things will return to normal). *)
let barely_non_complete_liquidatable_test =
  "barely_non_complete_liquidatable_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 5_065_065))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

    let expected_liquidation_result =
      Partial
        { liquidation_reward = Tez.of_mutez (Ligo.int_from_literal 1_005_065);
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 4_060_000);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 10_000_001);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 15_229_815);
          burrow_state =
            Burrow.make_for_test
              ~active:true
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:FixedPoint.one
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 4_060_000))
              ~liquidation_slices:None
              ~last_touched:(Timestamp.of_seconds 0)
        } in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Complete _ | Close _ -> failwith "impossible"
      | Partial details -> details in
    assert_properties_of_partial_liquidation burrow details

(* Maximum amount of collateral for the burrow to be liquidatable in a way thay
 * recovery seems impossible. *)
let barely_complete_liquidatable_test =
  "barely_complete_liquidatable_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 5_065_064))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

    let expected_liquidation_result =
      Complete
        { liquidation_reward = Tez.of_mutez (Ligo.int_from_literal 1_005_065);
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 4_059_999);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 9_999_998);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 15_229_814);
          burrow_state =
            Burrow.make_for_test
              ~active:true
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:FixedPoint.one
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 4_059_999))
              ~liquidation_slices:None
              ~last_touched:(Timestamp.of_seconds 0)
        } in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Partial _ | Close _ -> failwith "impossible"
      | Complete details -> details in
    assert_properties_of_complete_liquidation burrow details

(* Minimum amount of collateral for the burrow to be liquidatable in a way thay
 * recovery seems impossible, but without having to deactivate it. *)
let barely_non_close_liquidatable_test =
  "barely_non_close_liquidatable_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 1_001_000))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

    let expected_liquidation_result =
      Complete
        { liquidation_reward = Tez.of_mutez (Ligo.int_from_literal 1_001_000);
          tez_to_auction = Tez.zero;
          expected_kit = Kit.zero;
          min_kit_for_unwarranted = Kit.zero;
          burrow_state =
            Burrow.make_for_test
              ~active:true
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:FixedPoint.one
              ~collateral_at_auction:Tez.zero
              ~liquidation_slices:None
              ~last_touched:(Timestamp.of_seconds 0)
        } in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Partial _ | Close _ -> failwith "impossible"
      | Complete details -> details in
    assert_properties_of_complete_liquidation burrow details

(* Maximum amount of collateral for the burrow to be liquidatable and have to
 * be deactivated. *)
let barely_close_liquidatable_test =
  "barely_close_liquidatable_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 1_000_999))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in
    assert_bool "is overburrowed" (Burrow.is_overburrowed params burrow);
    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool "is liquidatable" (Burrow.is_liquidatable params burrow);

    let expected_liquidation_result =
      Close
        { liquidation_reward = Tez.of_mutez (Ligo.int_from_literal 1_001_000);
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 999_999);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 2_463_052);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 18_981_019);
          burrow_state =
            Burrow.make_for_test
              ~active:false
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:FixedPoint.one
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 999_999))
              ~liquidation_slices:None
              ~last_touched:(Timestamp.of_seconds 0)
        } in
    let liquidation_result = Burrow.request_liquidation params burrow in
    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Partial _ | Complete _ -> failwith "impossible"
      | Close details -> details in
    assert_properties_of_close_liquidation burrow details

let unwarranted_liquidation_unit_test =
  "unwarranted_liquidation_unit_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 7_673_400))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 10_000_000))
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
    assert_equal Unnecessary liquidation_result ~printer:Burrow.show_liquidation_result

let partial_liquidation_unit_test =
  "partial_liquidation_unit_test" >:: fun _ ->
    let burrow = initial_burrow in

    let expected_liquidation_result =
      Partial
        { liquidation_reward = Tez.add Constants.creation_deposit (Tez.of_mutez (Ligo.int_from_literal 9_999));
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 7_142_471);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 17_592_294);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 27_141_390);
          burrow_state =
            Burrow.make_for_test
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~active:true
              ~collateral:(Tez.of_mutez (Ligo.int_from_literal 1_847_530))
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 20_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:(Parameters.compute_adjustment_index params)
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 7_142_471))
              ~last_touched:(Timestamp.of_seconds 0)
              ~liquidation_slices:None
        } in

    let liquidation_result = Burrow.request_liquidation params burrow in

    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Complete _ | Close _ -> failwith "impossible"
      | Partial details -> details in

    assert_bool "is optimistically overburrowed" (Burrow.is_optimistically_overburrowed params burrow);
    assert_properties_of_partial_liquidation burrow details

let complete_liquidation_unit_test =
  "complete_liquidation_unit_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 10_000_000))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 100_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in

    let expected_liquidation_result =
      Complete
        { liquidation_reward = Tez.add Constants.creation_deposit (Tez.of_mutez (Ligo.int_from_literal 9_999));
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 8_990_001);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 22_142_860);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 170_810_019);
          burrow_state =
            Burrow.make_for_test
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~active:true
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 100_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:(Parameters.compute_adjustment_index params)
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 8_990_001))
              ~last_touched:(Timestamp.of_seconds 0)
              ~liquidation_slices:None
        } in

    let liquidation_result = Burrow.request_liquidation params burrow in

    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Partial _ | Close _ -> failwith "impossible"
      | Complete details -> details in

    assert_bool
      "input burrow is optimistically overburrowed"
      (Burrow.is_optimistically_overburrowed params burrow);
    assert_properties_of_complete_liquidation burrow details

let complete_and_close_liquidation_test =
  "complete_and_close_liquidation_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez (Ligo.int_from_literal 1_000_000))
        ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 100_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in

    let expected_liquidation_result =
      Close
        { liquidation_reward = Tez.add Constants.creation_deposit (Tez.of_mutez (Ligo.int_from_literal 999));
          tez_to_auction = Tez.of_mutez (Ligo.int_from_literal 999_001);
          expected_kit = Kit.of_mukit (Ligo.int_from_literal 2_460_594);
          min_kit_for_unwarranted = Kit.of_mukit (Ligo.int_from_literal 189_810_190);
          burrow_state =
            Burrow.make_for_test
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~active:false
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Ligo.int_from_literal 100_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:(Parameters.compute_adjustment_index params)
              ~collateral_at_auction:(Tez.of_mutez (Ligo.int_from_literal 999_001))
              ~last_touched:(Timestamp.of_seconds 0)
              ~liquidation_slices:None
        } in

    let liquidation_result = Burrow.request_liquidation params burrow in

    assert_equal
      expected_liquidation_result
      liquidation_result
      ~printer:Burrow.show_liquidation_result;

    let details = match liquidation_result with
      | Unnecessary | Partial _ | Complete _ -> failwith "impossible"
      | Close details -> details in

    assert_bool
      "input burrow is optimistically overburrowed"
      (Burrow.is_optimistically_overburrowed params burrow);
    assert_bool
      "output burrow is optimistically overburrowed"
      (Burrow.is_optimistically_overburrowed params details.burrow_state);
    assert_properties_of_close_liquidation burrow details

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

    (* General, property-based randomg tests regarding liquidation calculations. *)
    test_general_liquidation_properties;
  ]
