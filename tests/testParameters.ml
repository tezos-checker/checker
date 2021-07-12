open OUnit2
open TestLib
open Ratio
open FixedPoint
open Kit
open Parameters

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let rec call_touch_times
    (index: Ligo.tez)
    (kit_in_tez: ratio)
    (n: int)
    (params: parameters)
  : parameters =
  if n <= 0
  then params
  else begin
    Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let _total_accrual_to_cfmm, new_params = parameters_touch index kit_in_tez params in
    call_touch_times index kit_in_tez (pred n) new_params
  end

let sort_three_kit_amounts_increasing_order kit1 kit2 kit3 =
  match List.stable_sort kit_compare [kit1;kit2;kit3;] with
  | [kit1;kit2;kit3;] -> (kit1, kit2, kit3)
  | _ -> failwith "impossible"

(* ************************************************************************* *)
(*                        compute_drift_derivative                           *)
(* ************************************************************************* *)
(*
exp( low ): 201/200 = 1.005 = 1.0147AE147AE147AE147B
exp(-low ): 199/200 = 0.995 = 0.FEB851EB851EB851EB85
exp( high): 21/20   = 1.05  = 1.0CCCCCCCCCCCCCCCCCCD
exp(-high): 19/20   = 0.95  = 0.F3333333333333333333

d_t' =  0                   if exp(-0.005) <  p_t < exp(+0.005)
d_t' = +0.0001 / 86400^2    if exp(+0.005) <= p_t < exp(+0.05)
d_t' = -0.0001 / 86400^2    if exp(-0.005) >= p_t > exp(-0.05)
d_t' = +0.0005 / 86400^2    if exp(+0.05)  <= p_t < +infinity
d_t' = -0.0005 / 86400^2    if exp(-0.05)  >= p_t > -infinity
*)

let test_compute_drift_derivative_no_acceleration =
  "test_compute_drift_derivative_no_acceleration" >:: fun _ ->
    (* exp( 0 ): 1 *)
    let target = fixedpoint_one in
    assert_fixedpoint_equal
      ~expected:fixedpoint_zero
      ~real:(compute_drift_derivative target);

    (* exp( low ): 201/200 = 1.005 (rounded DOWN) *)
    let target = fixedpoint_of_hex_string "1.0147AE147AE147AE" in
    assert_fixedpoint_equal
      ~expected:fixedpoint_zero
      ~real:(compute_drift_derivative target);

    (* exp(-low ): 199/200 = 0.995 (rounded UP) *)
    let target = fixedpoint_of_hex_string "0.FEB851EB851EB852" in
    assert_fixedpoint_equal
      ~expected:fixedpoint_zero
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_low_positive_acceleration =
  "test_compute_drift_derivative_low_positive_acceleration" >:: fun _ ->
    (* exp( low ): 201/200 = 1.005 (rounded UP) *)
    let target = fixedpoint_of_hex_string "1.0147AE147AE147AF" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.000000000003C547")
      ~real:(compute_drift_derivative target);

    (* exp( high): 21/20   = 1.05 (rounded DOWN) *)
    let target = fixedpoint_of_hex_string "1.0CCCCCCCCCCCCCCC" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.000000000003C547")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_low_negative_acceleration =
  "test_compute_drift_derivative_low_negative_acceleration" >:: fun _ ->
    (* exp(-low ): 199/200 = 0.995 (rounded DOWN) *)
    let target = fixedpoint_of_hex_string "0.FEB851EB851EB851" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.000000000003C547")
      ~real:(compute_drift_derivative target);

    (* exp(-high): 19/20   = 0.95 (rounded UP) *)
    let target = fixedpoint_of_hex_string "0.F333333333333334" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.000000000003C547")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_high_positive_acceleration =
  "test_compute_drift_derivative_high_positive_acceleration" >:: fun _ ->
    (* exp( high): 21/20   = 1.05 (rounded UP) *)
    let target = fixedpoint_of_hex_string "1.0CCCCCCCCCCCCCCD" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "0.000000000012DA63")
      ~real:(compute_drift_derivative target)

let test_compute_drift_derivative_high_negative_acceleration =
  "test_compute_drift_derivative_high_negative_acceleration" >:: fun _ ->
    (* exp(-high): 19/20   = 0.95 (rounded DOWN) *)
    let target = fixedpoint_of_hex_string "0.F333333333333333" in
    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_hex_string "-0.000000000012DA63")
      ~real:(compute_drift_derivative target)

(* ************************************************************************* *)
(*                     compute_imbalance (unit tests)                        *)
(* ************************************************************************* *)

let test_compute_imbalance_all_zero =
  "test_compute_imbalance_all_zero" >:: fun _ ->
    let outstanding = kit_zero in
    let circulating = kit_zero in
    assert_ratio_equal
      ~expected:zero_ratio
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_zero_outstanding =
  "test_compute_imbalance_zero_outstanding" >:: fun _ ->
    let outstanding = kit_zero in
    let circulating = kit_one in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "100"))
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_zero_circulating =
  "test_compute_imbalance_zero_circulating" >:: fun _ ->
    let outstanding = kit_one in
    let circulating = kit_zero in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100"))
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_equal =
  "test_compute_imbalance_equal" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    assert_ratio_equal
      ~expected:zero_ratio
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_negative_small =
  "test_compute_imbalance_negative_small" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal   "937_500_001n") in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "-187499997") (Ligo.int_from_literal "3750000004")) (* JUST BELOW SATURATION *)
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_negative_big =
  "test_compute_imbalance_negative_big" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal   "937_500_000n") in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100")) (* JUST ABOVE SATURATION *)
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_negative_capped =
  "test_compute_imbalance_negative_capped" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal             "1n") in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100")) (* SATURATED *)
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_positive_small =
  "test_compute_imbalance_positive_small" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal   "933_333_334n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "199999998") (Ligo.int_from_literal "4000000000")) (* JUST BELOW SATURATION *)
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_positive_big =
  "test_compute_imbalance_positive_big" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal   "933_333_333n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "100")) (* JUST ABOVE SATURATION *)
      ~real:(compute_imbalance outstanding circulating)

let test_compute_imbalance_positive_capped =
  "test_compute_imbalance_positive_capped" >:: fun _ ->
    let outstanding = kit_of_mukit (Ligo.nat_from_literal             "1n") in
    let circulating = kit_of_mukit (Ligo.nat_from_literal "1_000_000_000n") in
    assert_ratio_equal
      ~expected:(make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "100")) (* SATURATED *)
      ~real:(compute_imbalance outstanding circulating)

(* ************************************************************************* *)
(*                compute_imbalance (property-based tests)                   *)
(* ************************************************************************* *)

(* Imbalance can never go above 5% *)
let test_imbalance_upper_bound =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_upper_bound"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating) ->
  leq_ratio_ratio
    (compute_imbalance burrowed circulating)
    (make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "100"))

(* Imbalance can never go below -5% *)
let test_imbalance_lower_bound =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_lower_bound"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating) ->
  geq_ratio_ratio
    (compute_imbalance burrowed circulating)
    (make_ratio (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100"))

(* The sign of imbalance is the same as of (circulating - burrowed).
 * If circulating > burrowed then imbalance > 0
 * If circulating < burrowed then imbalance < 0
 * If burrowed = circulating then imbalance = 0 (NOTE: rarely checked, I guess)
*)
let test_imbalance_sign_preservation =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_sign_preservation"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating) ->
  sign_ratio (compute_imbalance burrowed circulating) = kit_compare circulating burrowed

(* If burrowed = circulating then imbalance = 0. *)
let test_imbalance_is_zero_when_equal =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_is_zero_when_equal"
    ~count:property_test_count
    TestArbitrary.arb_kit
  @@ fun kit ->
  eq_ratio_ratio
    (compute_imbalance kit (* burrowed *) kit (* circulating *))
    zero_ratio

(* For a fixed amount of kit in circulation, increasing the burrowed kit
 * decreases the imbalance. *)
let test_imbalance_positive_tendencies =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_positive_tendencies"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (kit1, kit2, kit3) ->
  (* If burrowed1 > burrowed2 > circulating then
   * (compute_imbalance burrowed1 circulating) <= (compute_imbalance burrowed2 circulating) *)
  let (circulating, burrowed2, burrowed1) =
    (* Just using sorting, to avoid expensive assume-conditionals. *)
    sort_three_kit_amounts_increasing_order kit1 kit2 kit3 in
  leq_ratio_ratio
    (compute_imbalance burrowed1 circulating)
    (compute_imbalance burrowed2 circulating)

(* For a fixed amount of burrowed kit, increasing the kit in circulation
 * increases the imbalance. *)
let test_imbalance_negative_tendencies =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_negative_tendencies"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (kit1, kit2, kit3) ->
  (* If circulating1 > circulating2 > burrowed then
   * (compute_imbalance burrowed circulating1) >= (compute_imbalance burrowed circulating2) *)
  let (burrowed, circulating2, circulating1) =
    (* Just using sorting, to avoid expensive assume-conditionals. *)
    sort_three_kit_amounts_increasing_order kit1 kit2 kit3 in
  geq_ratio_ratio
    (compute_imbalance burrowed circulating1)
    (compute_imbalance burrowed circulating2)

(* ************************************************************************* *)
(*                          Index/Protected Index                            *)
(* ************************************************************************* *)

(* The protected index should always follow the tendency of the given index,
 * independently of whether that happens fast or slowly. *)
let test_protected_index_follows_index =
  (* initial *)
  let params = initial_parameters in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = one_ratio in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_protected_index_follows_index"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_small_tez QCheck.small_nat)
  @@ fun (index, lvl) ->
  Ligo.Tezos.reset();

  (* let time pass, please *)
  let lvl = lvl+1 in
  Ligo.Tezos.new_transaction ~seconds_passed:(lvl*60) ~blocks_passed:lvl ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

  let _total_accrual_to_cfmm, new_params = parameters_touch index kit_in_tez params in

  assert_stdlib_int_equal
    ~expected:(compare new_params.index params.index)
    ~real:(compare new_params.protected_index params.protected_index);
  true

(* The protected index should not follow the tendency of the given index "too
 * fast". According to current expectations, the protected index should be able
 * to catch up to a 2x or 0.5x move in 24 hours, and a 3% move in an hour. *)
let test_protected_index_pace =
  "test_protected_index_pace" >:: fun _ ->
    (* initial *)

    let params = initial_parameters in

    (* Neutral kit_in_tez (same as initial) *)
    let kit_in_tez = one_ratio in

    (* UPWARD MOVES *)
    let very_high_index =
      let x = mul_ratio (ratio_of_int (Ligo.int_from_literal "1000")) (ratio_of_tez params.index) in
      fraction_to_tez_floor x.num x.den in
    (* One hour, upward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 1.030420 (=103.0420% of initial; slightly over 3%) *)
    Ligo.Tezos.reset();
    let new_params = call_touch_times very_high_index kit_in_tez (60 (* 60 blocks ~ 1h *)) params in
    assert_tez_equal ~expected:(Ligo.tez_from_literal "1_030_420mutez") ~real:new_params.protected_index;
    (* One day, upward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 2.053031 (=205.3031% of initial; slightly over double) *)
    Ligo.Tezos.reset();
    let new_params = call_touch_times very_high_index kit_in_tez (60 * 24 (* 60 blocks ~ 1h *)) params in
    assert_tez_equal ~expected:(Ligo.tez_from_literal "2_053_031mutez") ~real:new_params.protected_index;

    (* DOWNWARD MOVES *)
    let very_low_index =
      let x = mul_ratio (make_ratio (Ligo.int_from_literal "1") (Ligo.int_from_literal "1000")) (ratio_of_tez params.index) in
      fraction_to_tez_floor x.num x.den in
    (* One hour, downward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 0.970407 (=2.9593% less than initial; slightly under 3% *)
    Ligo.Tezos.reset();
    let new_params = call_touch_times very_low_index kit_in_tez (60 (* 60 blocks ~ 1h *)) params in
    assert_tez_equal ~expected:(Ligo.tez_from_literal "970_407mutez") ~real:new_params.protected_index;
    (* One day, downward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 0.486151 (=51.3849% less than initial; slightly more than halved) *)
    Ligo.Tezos.reset();
    let new_params = call_touch_times very_low_index kit_in_tez (60 * 24 (* 60 blocks ~ 1h *)) params in
    assert_tez_equal ~expected:(Ligo.tez_from_literal "486_151mutez") ~real:new_params.protected_index

(* ************************************************************************* *)
(*                                 Prices                                    *)
(* ************************************************************************* *)

(* This test catches the following mutation:
 *   src/parameters.ml:29 (1_000_000mutez => 999_999mutez) *)
let test_initial_tz_minting_equals_initial_tz_liquidation =
  "initially tz_minting equals tz_liquidation" >:: fun _ ->
    assert_tez_equal
      ~expected:(tz_liquidation initial_parameters)
      ~real:(tz_minting initial_parameters)

(* The pace of change of the minting index is bounded on the low side. George:
 * What about the pace of change of the minting price (affected also by the
 * current quantity q)? *)
let test_minting_index_low_bounded =
  (* initial *)
  let params = initial_parameters in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = one_ratio in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_minting_index_low_bounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Ligo.tez_from_literal (string_of_int x ^ "mutez")) QCheck.(1 -- 999_999))
  @@ fun index ->
  Ligo.Tezos.reset ();

  (* just the next block *)
  Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

  let _total_accrual_to_cfmm, new_params = parameters_touch index kit_in_tez params in

  (tz_minting new_params >= Ligo.tez_from_literal "999_500mutez") (* 0.05% down, at "best" *)

(* The pace of change of the minting index is unbounded on the high side.
 * George: What about the pace of change of the minting price (affected also by
 * the current quantity q)? *)
let test_minting_index_high_unbounded =
  (* initial *)
  let params = initial_parameters in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = one_ratio in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_minting_index_high_unbounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Ligo.tez_from_literal (string_of_int x ^ "mutez")) QCheck.(1_000_001 -- max_int))
  @@ fun index ->
  Ligo.Tezos.reset ();
  (* just the next block *)
  Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

  let _total_accrual_to_cfmm, new_params = parameters_touch index kit_in_tez params in
  assert_tez_equal
    ~expected:index
    ~real:(tz_minting new_params);
  true

(* The pace of change of the liquidation index is bounded on the high side.
 * George: What about the pace of change of the liquidation price (affected
 * also by the current quantity q)? *)
let test_liquidation_index_high_bounded =
  (* initial *)
  let params = initial_parameters in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = one_ratio in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_liquidation_index_high_bounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Ligo.tez_from_literal (string_of_int x ^ "mutez")) QCheck.(1_000_001 -- max_int))
  @@ fun index ->
  Ligo.Tezos.reset ();
  (* just the next block *)
  Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

  let _total_accrual_to_cfmm, new_params = parameters_touch index kit_in_tez params in
  (* not very likely to hit the < case here I think;
   * perhaps we need a different generator *)
  (tz_liquidation new_params <= Ligo.tez_from_literal "1_000_500mutez") (* 0.05% up, at "best" *)

(* The pace of change of the liquidation index is unbounded on the low side.
 * George: What about the pace of change of the liquidation price (affected
 * also by the current quantity q)? *)
let test_liquidation_index_low_unbounded =
  (* initial *)
  Ligo.Tezos.reset ();
  let params = initial_parameters in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = one_ratio in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_liquidation_index_low_unbounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Ligo.tez_from_literal (string_of_int x ^ "mutez")) QCheck.(1 -- 999_999))
  @@ fun index ->
  (* just the next block *)
  Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

  let _total_accrual_to_cfmm, new_params = parameters_touch index kit_in_tez params in
  assert_tez_equal
    ~expected:index
    ~real:(tz_liquidation new_params);
  true

(* ************************************************************************* *)
(*                                  touch                                    *)
(* ************************************************************************* *)

(* Just a simple unit test, where the parameters are touched again without any
 * time passing. In fact, in checker.ml we make sure to update nothing at all,
 * if no time has passed (cf. touch_with_index). *)
let test_touch_0 =
  "test_touch_0" >:: fun _ ->
    Ligo.Tezos.reset ();
    let in_params =
      Parameters.{
        q = fixedpoint_one;
        index = Ligo.tez_from_literal "1_000_000mutez";
        protected_index = Ligo.tez_from_literal "1_000_000mutez";
        target = fixedpoint_one;
        drift = fixedpoint_zero;
        drift_derivative = fixedpoint_zero;
        burrow_fee_index = fixedpoint_one;
        imbalance_index = fixedpoint_one;
        outstanding_kit = kit_zero;
        circulating_kit = kit_zero;
        last_touched = !Ligo.Tezos.now;
      } in
    let expected_out_params =
      Parameters.{
        q = fixedpoint_one;
        index = Ligo.tez_from_literal "500_000mutez";
        protected_index = Ligo.tez_from_literal "1_000_000mutez";
        target = fixedpoint_of_hex_string "0.2AAAAAAAAAAAAAAA";
        drift_derivative = fixedpoint_zero;
        drift = fixedpoint_zero;
        burrow_fee_index = fixedpoint_one;
        imbalance_index = fixedpoint_one;
        outstanding_kit = kit_zero;
        circulating_kit = kit_zero;
        last_touched = !Ligo.Tezos.now;
      } in

    (* Non-trivial values for the arguments. *)
    let kit_in_tez = make_ratio (Ligo.int_from_literal "3") (Ligo.int_from_literal "1") in
    let index = Ligo.tez_from_literal "500_000mutez" in
    (* Touch in the same block. I wonder if we wish to allow this for a local function such as parameters_touch. *)
    Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let total_accrual_to_cfmm, out_params = parameters_touch index kit_in_tez in_params in

    assert_kit_equal
      ~expected:kit_zero
      ~real:total_accrual_to_cfmm;
    assert_parameters_equal
      ~expected:expected_out_params
      ~real:out_params;
    ()

(* Just a simple unit test, testing nothing specific, really. *)
let test_touch_1 =
  "test_touch_1" >:: fun _ ->
    let initial_parameters : parameters =
      { q = fixedpoint_of_hex_string "0.E666666666666666"; (* 0.9 *)
        index = Ligo.tez_from_literal "360_000mutez";
        target = fixedpoint_of_hex_string "1.147AE147AE147AE1"; (* 1.08 *)
        protected_index = Ligo.tez_from_literal "350_000mutez";
        drift = fixedpoint_zero;
        drift_derivative = fixedpoint_zero;
        burrow_fee_index = fixedpoint_one;
        imbalance_index = fixedpoint_one;
        outstanding_kit = kit_one;
        circulating_kit = kit_zero;
        last_touched = Ligo.timestamp_from_seconds_literal 0;
      } in
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:3600 ~blocks_passed:60 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

    let new_index = Ligo.tez_from_literal "340_000mutez" in
    let kit_in_tez = make_ratio (Ligo.int_from_literal "305") (Ligo.int_from_literal "1000") in
    let total_accrual_to_cfmm, new_parameters = parameters_touch new_index kit_in_tez initial_parameters in
    assert_parameters_equal
      ~expected:{
        q = fixedpoint_of_hex_string "0.E6666895A3EC8BA5"; (* 0.90000013020828555983 *)
        index = Ligo.tez_from_literal "340_000mutez";
        protected_index = Ligo.tez_from_literal "340_000mutez";
        target = fixedpoint_of_hex_string "1.00D6E1B366FF4BEE"; (* 1.00327883367481013224 *)
        drift_derivative = fixedpoint_of_hex_string "0.000000000012DA63"; (* 0.00000000000006697957 *)
        drift  = fixedpoint_of_hex_string "0.00000000848F8818"; (* 0.00000000012056322737 *)
        burrow_fee_index = fixedpoint_of_hex_string "1.00000991D674CC29"; (* 1.00000057039729312258 *)
        imbalance_index = fixedpoint_of_hex_string "0.FFFFA04D9F700662";
        outstanding_kit = kit_of_mukit (Ligo.nat_from_literal "999_994n");
        circulating_kit = kit_of_mukit (Ligo.nat_from_literal "0_000_000n"); (* NOTE that it ends up being identical to the one we started with *)
        last_touched = !Ligo.Tezos.now;
      }
      ~real:new_parameters;
    assert_kit_equal ~expected:kit_zero ~real:total_accrual_to_cfmm (* NOTE: I'd expect this to be higher I think. *)

(* Just a simple unit test, testing nothing specific, really. *)
let test_touch_2 =
  "test_touch_2" >:: fun _ ->
    let initial_parameters : parameters =
      { q = fixedpoint_of_hex_string "0.E666666666666666"; (* 0.9 *)
        index = Ligo.tez_from_literal "360_000mutez";
        target = fixedpoint_of_hex_string "1.147AE147AE147AE1"; (* 1.08 *)
        protected_index = Ligo.tez_from_literal "350_000mutez";
        drift = fixedpoint_zero;
        drift_derivative = fixedpoint_zero;
        burrow_fee_index = fixedpoint_one;
        imbalance_index = fixedpoint_one;
        outstanding_kit = (kit_of_mukit (Ligo.nat_from_literal "1_753_165n")); (* 1_753_164n should leave as is *)
        circulating_kit = (kit_of_mukit (Ligo.nat_from_literal "1_000_000n"));
        last_touched = Ligo.timestamp_from_seconds_literal 0;
      } in
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:3600 ~blocks_passed:60 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

    let new_index = Ligo.tez_from_literal "340_000mutez" in
    let kit_in_tez = make_ratio (Ligo.int_from_literal "305") (Ligo.int_from_literal "1000") in
    let total_accrual_to_cfmm, new_parameters = parameters_touch new_index kit_in_tez initial_parameters in
    assert_parameters_equal
      ~expected:{
        q = fixedpoint_of_hex_string "0.E6666895A3EC8BA5";
        index = Ligo.tez_from_literal "340000mutez";
        protected_index = Ligo.tez_from_literal "340000mutez";
        target = fixedpoint_of_hex_string "1.00D6E1B366FF4BEE";
        drift_derivative = fixedpoint_of_hex_string "0.000000000012DA63";
        drift = fixedpoint_of_hex_string "0.00000000848F8818";
        burrow_fee_index = fixedpoint_of_hex_string "1.00000991D674CC29";
        imbalance_index = fixedpoint_of_hex_string "0.FFFFA04D9F700662";
        outstanding_kit = kit_of_mukit (Ligo.nat_from_literal "1_753_155n");
        circulating_kit = kit_of_mukit (Ligo.nat_from_literal "1_000_001n");
        last_touched = !Ligo.Tezos.now;
      }
      ~real:new_parameters;
    assert_kit_equal
      ~expected:(kit_of_mukit (Ligo.nat_from_literal "1n"))
      ~real:total_accrual_to_cfmm

(* ************************************************************************* *)
(*               add/remove circulating_kit/outstanding_kit                  *)
(* ************************************************************************* *)

(* add_circulating_kit and remove_circulating_kit are inverses of each other *)
let test_add_remove_circulating_kit_inverses =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_remove_circulating_kit"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating, kit) ->

  let params1 =
    { initial_parameters with
      outstanding_kit = burrowed;
      circulating_kit = circulating;
    } in
  let params2 = add_circulating_kit params1 kit in
  let params3 = remove_circulating_kit params2 kit in
  (* the parameters should stay as they are *)
  assert_parameters_equal ~expected:params1 ~real:params3;
  true

(* add_outstanding_and_circulating_kit and
 * remove_outstanding_and_circulating_kit are inverses of each other *)
let test_add_remove_outstanding_and_circulating_kit_inverses =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_remove_outstanding_and_circulating_kit_inverses"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating, kit) ->

  let params1 =
    { initial_parameters with
      outstanding_kit = burrowed;
      circulating_kit = circulating;
    } in
  let params2 = add_outstanding_and_circulating_kit params1 kit in
  let params3 = remove_outstanding_and_circulating_kit params2 kit in
  (* the parameters should stay as they are *)
  assert_parameters_equal ~expected:params1 ~real:params3;
  true

(* add_circulating_kit has the expected effect *)
let test_add_circulating_kit_effect =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_circulating_kit_effect"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating, kit) ->

  let params1 =
    { initial_parameters with
      outstanding_kit = burrowed;
      circulating_kit = circulating;
    } in
  let params2 = add_circulating_kit params1 kit in

  (* add_circulating_kit should increase the circulating kit *)
  assert_kit_equal
    ~expected:(Kit.kit_add params1.circulating_kit kit)
    ~real:params2.circulating_kit;
  (* add_circulating_kit should not affect other fields *)
  assert_parameters_equal
    ~expected:params1
    ~real:{ params2 with circulating_kit = params1.circulating_kit };
  true

(* add_outstanding_and_circulating_kit has the expected effect *)
let test_add_outstanding_and_circulating_kit_effect =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_outstanding_and_circulating_kit_effect"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating, kit) ->

  let params1 =
    { initial_parameters with
      outstanding_kit = burrowed;
      circulating_kit = circulating;
    } in
  let params2 = add_outstanding_and_circulating_kit params1 kit in

  (* add_outstanding_and_circulating_kit should increase the outstanding kit *)
  assert_kit_equal
    ~expected:(Kit.kit_add params1.outstanding_kit kit)
    ~real:params2.outstanding_kit;
  (* add_outstanding_and_circulating_kit should increase the circulating kit *)
  assert_kit_equal
    ~expected:(Kit.kit_add params1.circulating_kit kit)
    ~real:params2.circulating_kit;
  (* add_outstanding_and_circulating_kit should not affect other fields *)
  assert_parameters_equal
    ~expected:params1
    ~real:{
      params2 with
      outstanding_kit = params1.outstanding_kit;
      circulating_kit = params1.circulating_kit;
    };
  true

(* remove_circulating_kit has the expected effect *)
let test_remove_circulating_kit_effect =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_circulating_kit_effect"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (kit1, kit2, kit3) ->

  let kit_to_remove = kit_min (kit_min kit1 kit2) kit3 in (* to avoid underflows *)

  let params1 =
    { initial_parameters with
      outstanding_kit = kit1;
      circulating_kit = kit2;
    } in
  let params2 = remove_circulating_kit params1 kit_to_remove in

  (* remove_circulating_kit should decrease the circulating kit *)
  assert_kit_equal
    ~expected:(Kit.kit_add params2.circulating_kit kit_to_remove)
    ~real:params1.circulating_kit;
  (* remove_circulating_kit should not affect other fields *)
  assert_parameters_equal
    ~expected:params1
    ~real:{ params2 with circulating_kit = params1.circulating_kit };
  true

(* remove_outstanding_and_circulating_kit has the expected effect *)
let test_remove_outstanding_and_circulating_kit_effect =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"remove_outstanding_and_circulating_kit"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (kit1, kit2, kit3) ->

  let kit_to_remove = kit_min (kit_min kit1 kit2) kit3 in (* to avoid underflows *)

  let params1 =
    { initial_parameters with
      outstanding_kit = kit1;
      circulating_kit = kit2;
    } in
  let params2 = remove_outstanding_and_circulating_kit params1 kit_to_remove in

  (* remove_outstanding_and_circulating_kit should decrease the outstanding kit *)
  assert_kit_equal
    ~expected:(Kit.kit_add params2.outstanding_kit kit_to_remove)
    ~real:params1.outstanding_kit;
  (* remove_outstanding_and_circulating_kit should decrease the circulating kit *)
  assert_kit_equal
    ~expected:(Kit.kit_add params2.circulating_kit kit_to_remove)
    ~real:params1.circulating_kit;
  (* remove_outstanding_and_circulating_kit should not affect other fields *)
  assert_parameters_equal
    ~expected:params1
    ~real:{
      params2 with
      outstanding_kit = params1.outstanding_kit;
      circulating_kit = params1.circulating_kit;
    };
  true

(* Just a simple unit test, more tightly testing the changes when updating the
 * oustanding and the circulating kit. *)
let test_add_remove_outstanding_circulating_kit_unit =
  "test_add_remove_outstanding_circulating_kit_unit" >:: fun _ ->
    (* initial parameters *)
    let params = initial_parameters in
    (* initially both zero *)
    assert_kit_equal
      ~expected:kit_zero
      ~real:params.circulating_kit;
    assert_kit_equal
      ~expected:kit_zero
      ~real:params.outstanding_kit;
    (* add some circulating kit only *)
    let kit_to_add_to_circulating = Kit.kit_of_mukit (Ligo.nat_from_literal "172_635_932_647n") in
    let params = add_circulating_kit params kit_to_add_to_circulating in
    (* the circulating should have increased, but not the outstanding *)
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "172_635_932_647n"))
      ~real:params.circulating_kit;
    assert_kit_equal
      ~expected:kit_zero
      ~real:params.outstanding_kit;
    (* add some to both the outstanding and the circulating kit *)
    let kit_to_add_to_both = Kit.kit_of_mukit (Ligo.nat_from_literal "5_473_635_298_465n") in
    let params = add_outstanding_and_circulating_kit params kit_to_add_to_both in
    (* both should have increased *)
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "5_646_271_231_112n"))
      ~real:params.circulating_kit;
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "5_473_635_298_465n"))
      ~real:params.outstanding_kit;
    (* remove some from both the outstanding and the circulating kit *)
    let kit_to_remove_from_both = Kit.kit_of_mukit (Ligo.nat_from_literal "765_601_721n") in
    let params = remove_outstanding_and_circulating_kit params kit_to_remove_from_both in
    (* both should have decreased *)
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "5_645_505_629_391n"))
      ~real:params.circulating_kit;
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "5_472_869_696_744n"))
      ~real:params.outstanding_kit;
    (* remove some from the circulating kit only *)
    let kit_to_remove_from_circulating = Kit.kit_of_mukit (Ligo.nat_from_literal "4_123_827_936_001n") in
    let params = remove_circulating_kit params kit_to_remove_from_circulating in
    (*the circulating should have decreased, not not the outstanding *)
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "1_521_677_693_390n"))
      ~real:params.circulating_kit;
    assert_kit_equal
      ~expected:(Kit.kit_of_mukit (Ligo.nat_from_literal "5_472_869_696_744n"))
      ~real:params.outstanding_kit;
    ()

(* ************************************************************************* *)
(*                            compute_current_q                              *)
(* ************************************************************************* *)

(* drift derivatives can only take one of the following values:
 *   - high_negative_acceleration = fixedpoint_of_raw (Ligo.int_from_literal "-1235555")
 *   - low_negative_acceleration  = fixedpoint_of_raw (Ligo.int_from_literal "-247111")
 *   - fixedpoint_zero
 *   - low_positive_acceleration  = fixedpoint_of_raw (Ligo.int_from_literal "247111")
 *   - high_positive_acceleration = fixedpoint_of_raw (Ligo.int_from_literal "1235555")
 *
 * q should in general stay in the vicinity of 1.
 *
 * the drift is supposed to stay small, but I am not convinced about this from its definition.
*)

(* Just a simple unit test, more tightly testing the changes when updating q *)
let test_compute_current_q =
  "test_compute_current_q" >:: fun _ ->
    (* some random input values *)
    let last_q                   = fixedpoint_one in
    let last_drift               = fixedpoint_of_raw (Ligo.int_from_literal "1_234_567") in

    let last_drift_derivative    = Constants.low_positive_acceleration in
    let current_drift_derivative = Constants.high_positive_acceleration in
    let duration_in_seconds      = Ligo.int_from_literal "193" in (* around three blocks *)

    let current_q = compute_current_q last_q last_drift last_drift_derivative current_drift_derivative duration_in_seconds in

    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_raw (Ligo.int_from_literal "18446744084686566959"))
      ~real:current_q

(* TODO: Properties I expect, given the definition of compute_current_q:
   (assuming all (>0)) increase last_q                   => increase current_q (proportionately)
   (assuming all (>0)) increase last_drift               => increase current_q
   (assuming all (>0)) increase last_drift_derivative    => increase current_q
   (assuming all (>0)) increase current_drift_derivative => increase current_q (half the effect of last_drift_derivative)
   (assuming all (>0)) increase duration_in_seconds      => increase current_q

   q_{i+1} = FLOOR (q_i * EXP((drift_i + (1/6) * (2*drift'_i + drift'_{i+1}) * (t_{i+1} - t_i)) * (t_{i+1} - t_i)))
*)

(* ************************************************************************* *)
(*                          compute_current_drift                            *)
(* ************************************************************************* *)

(* drift derivatives can only take one of the following values:
 *   - high_negative_acceleration = fixedpoint_of_raw (Ligo.int_from_literal "-1235555")
 *   - low_negative_acceleration  = fixedpoint_of_raw (Ligo.int_from_literal "-247111")
 *   - fixedpoint_zero
 *   - low_positive_acceleration  = fixedpoint_of_raw (Ligo.int_from_literal "247111")
 *   - high_positive_acceleration = fixedpoint_of_raw (Ligo.int_from_literal "1235555")
 *
 * the drift is supposed to stay small, but I am not convinced about this from its definition.
*)

(* Just a simple unit test, more tightly testing the changes when updating the drift *)
let test_compute_current_drift =
  "test_compute_current_drift" >:: fun _ ->
    (* some random input values *)
    let last_drift               = fixedpoint_of_raw (Ligo.int_from_literal "1_234_567") in
    let last_drift_derivative    = Constants.low_positive_acceleration in
    let current_drift_derivative = Constants.high_positive_acceleration in
    let duration_in_seconds      = Ligo.int_from_literal "193" in (* around three blocks *)

    let current_drift = compute_current_drift last_drift last_drift_derivative current_drift_derivative duration_in_seconds in

    assert_fixedpoint_equal
      ~expected:(fixedpoint_of_raw (Ligo.int_from_literal "144311836"))
      ~real:current_drift

(* TODO: Look into property-based tests too, based on its definition.

   drift_{i+1} = FLOOR (drift_i + (1/2) * (drift'_i + drift'_{i+1}) * (t_{i+1} - t_i))
*)

let suite =
  "Parameters tests" >::: [
    (* compute_drift_derivative *)
    test_compute_drift_derivative_no_acceleration;
    test_compute_drift_derivative_low_positive_acceleration;
    test_compute_drift_derivative_low_negative_acceleration;
    test_compute_drift_derivative_high_positive_acceleration;
    test_compute_drift_derivative_high_negative_acceleration;

    (* compute_imbalance (unit tests) *)
    test_compute_imbalance_all_zero;
    test_compute_imbalance_zero_outstanding;
    test_compute_imbalance_zero_circulating;
    test_compute_imbalance_equal;

    test_compute_imbalance_positive_small;
    test_compute_imbalance_positive_big;
    test_compute_imbalance_positive_capped;

    test_compute_imbalance_negative_small;
    test_compute_imbalance_negative_big;
    test_compute_imbalance_negative_capped;

    (* compute_imbalance (property-based random tests) *)
    test_imbalance_upper_bound;
    test_imbalance_lower_bound;
    test_imbalance_sign_preservation;
    test_imbalance_is_zero_when_equal;
    test_imbalance_positive_tendencies;
    test_imbalance_negative_tendencies;

    (* protected index movements *)
    test_protected_index_follows_index;
    test_protected_index_pace;

    (* prices and movement *)
    test_initial_tz_minting_equals_initial_tz_liquidation;

    test_minting_index_low_bounded;
    test_minting_index_high_unbounded;

    test_liquidation_index_high_bounded;
    test_liquidation_index_low_unbounded;

    (* touch *)
    test_touch_0;
    test_touch_1;
    test_touch_2;

    (* add/remove circulating_kit/outstanding_kit (property-based random tests) *)
    test_add_remove_circulating_kit_inverses;
    test_add_remove_outstanding_and_circulating_kit_inverses;
    test_add_circulating_kit_effect;
    test_add_outstanding_and_circulating_kit_effect;
    test_remove_circulating_kit_effect;
    test_remove_outstanding_and_circulating_kit_effect;

    (* add/remove circulating_kit/outstanding_kit (unit tests) *)
    test_add_remove_outstanding_circulating_kit_unit;

    (* compute_current_q *)
    test_compute_current_q;

    (* compute_current_drift *)
    test_compute_current_drift;
  ]

let () =
  run_test_tt_main
    suite
