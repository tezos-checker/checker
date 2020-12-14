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
         let tez = Tez.of_q_floor Q.(of_int t / (of_int factor * of_int 2)) in
         let kit = Kit.of_q_floor Q.(of_int k /  of_int factor) in
         (tez, kit)
      )
      (QCheck.triple positive_int positive_int positive_int) in
  (* More likely to give Complete/Partial/Unnecessary ones *)
  let arb_smart_tez_kit_2 =
    QCheck.map
      (fun (tez, kit) ->
         let tez = Tez.of_q_floor Q.(Tez.to_q tez / of_int 2) in
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
  { q = FixedPoint.of_q_floor (Q.of_string "1015/1000");
    index = Tez.of_mutez 320_000;
    protected_index = Tez.of_mutez 360_000;
    target = FixedPoint.of_q_floor (Q.of_string "108/100");
    drift = FixedPoint.zero;
    drift' = FixedPoint.zero;
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
    Tez.(Burrow.collateral burrow_out + details.tez_to_auction + details.liquidation_reward)
    ~printer:Tez.show;
  assert_equal
    (Burrow.collateral_at_auction burrow_in)
    Tez.(Burrow.collateral_at_auction burrow_out - details.tez_to_auction)
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
    Tez.(Burrow.collateral burrow_out + details.tez_to_auction + details.liquidation_reward)
    ~printer:Tez.show;
  assert_equal
    (Burrow.collateral_at_auction burrow_in)
    Tez.(Burrow.collateral_at_auction burrow_out - details.tez_to_auction)
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
    Tez.(Burrow.collateral burrow_in + Constants.creation_deposit)
    Tez.(Burrow.collateral burrow_out + details.tez_to_auction + details.liquidation_reward)
    ~printer:Tez.show;
  assert_equal
    (Burrow.collateral_at_auction burrow_in)
    Tez.(Burrow.collateral_at_auction burrow_out - details.tez_to_auction)
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
    ~collateral:(Tez.of_mutez 10_000_000)
    ~outstanding_kit:(Kit.of_mukit (Z.of_int 20_000_000))
    ~excess_kit:Kit.zero
    ~adjustment_index:(Parameters.compute_adjustment_index params)
    ~collateral_at_auction:Tez.zero
    ~last_touched:(Timestamp.of_seconds 0)
    ~liquidation_slices:None

let partial_liquidation_unit_test =
  "partial_liquidation_unit_test" >:: fun _ ->
    let burrow = initial_burrow in

    let expected_liquidation_result =
      Partial
        { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 9_999);
          tez_to_auction = Tez.of_mutez 7_142_471;
          expected_kit = Kit.of_mukit (Z.of_int 17_592_294);
          min_kit_for_unwarranted = Kit.of_mukit (Z.of_int 27_141_390);
          burrow_state =
            Burrow.make_for_test
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~active:true
              ~collateral:(Tez.of_mutez 1_847_530)
              ~outstanding_kit:(Kit.of_mukit (Z.of_int 20_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:(Parameters.compute_adjustment_index params)
              ~collateral_at_auction:(Tez.of_mutez 7_142_471)
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

let unwarranted_liquidation_unit_test =
  "unwarranted_liquidation_unit_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez 10_000_000)
        ~outstanding_kit:(Kit.of_mukit (Z.of_int 10_000_000))
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

let complete_liquidation_unit_test =
  "complete_liquidation_unit_test" >:: fun _ ->
    let burrow =
      Burrow.make_for_test
        ~permission_version:0
        ~allow_all_tez_deposits:false
        ~allow_all_kit_burnings:false
        ~delegate:None
        ~active:true
        ~collateral:(Tez.of_mutez 10_000_000)
        ~outstanding_kit:(Kit.of_mukit (Z.of_int 100_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in

    let expected_liquidation_result =
      Complete
        { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 9_999);
          tez_to_auction = Tez.of_mutez 8_990_001;
          expected_kit = Kit.of_mukit (Z.of_int 22_142_860);
          min_kit_for_unwarranted = Kit.of_mukit (Z.of_int 170_810_019);
          burrow_state =
            Burrow.make_for_test
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~active:true
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Z.of_int 100_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:(Parameters.compute_adjustment_index params)
              ~collateral_at_auction:(Tez.of_mutez 8_990_001)
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
        ~collateral:(Tez.of_mutez 1_000_000)
        ~outstanding_kit:(Kit.of_mukit (Z.of_int 100_000_000))
        ~excess_kit:Kit.zero
        ~adjustment_index:(Parameters.compute_adjustment_index params)
        ~collateral_at_auction:Tez.zero
        ~last_touched:(Timestamp.of_seconds 0)
        ~liquidation_slices:None
    in

    let expected_liquidation_result =
      Close
        { liquidation_reward = Tez.(Constants.creation_deposit + Tez.of_mutez 999);
          tez_to_auction = Tez.of_mutez 999_001;
          expected_kit = Kit.of_mukit (Z.of_int 2_460_594);
          min_kit_for_unwarranted = Kit.of_mukit (Z.of_int 189_810_190);
          burrow_state =
            Burrow.make_for_test
              ~permission_version:0
              ~allow_all_tez_deposits:false
              ~allow_all_kit_burnings:false
              ~delegate:None
              ~active:false
              ~collateral:Tez.zero
              ~outstanding_kit:(Kit.of_mukit (Z.of_int 100_000_000))
              ~excess_kit:Kit.zero
              ~adjustment_index:(Parameters.compute_adjustment_index params)
              ~collateral_at_auction:(Tez.of_mutez 999_001)
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

    (* General, property-based random tests *)
    liquidatable_implies_overburrowed;
    optimistically_overburrowed_implies_overburrowed;

    (* General, property-based randomg tests regarding liquidation calculations. *)
    test_general_liquidation_properties;
  ]
