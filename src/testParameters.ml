open OUnit2

(*
Parameter-related things we might want to add tests for:
- What do the prices do? (we already have some data for the tz indices)
- TODO: add a gazillion things here.
*)

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let initial_tezos =
  Tezos.{
    now = Ligo.timestamp_from_seconds_literal 0;
    level = Level.of_int 0;
    self = Address.of_string "checker";
  }

let rec call_touch_times
    (index: Tez.t)
    (kit_in_tez: Ratio.t)
    (n: int)
    (tezos: Tezos.t)
    (params: Parameters.t)
  : Parameters.t =
  if n <= 0
  then params
  else
    let new_tezos =
      { tezos with
        now = Ligo.add_timestamp_int tezos.now (Ligo.int_from_literal 60);
        level = Level.(of_int (succ (to_int tezos.level)));
      } in
    let _total_accrual_to_uniswap, new_params = Parameters.touch new_tezos index kit_in_tez params in
    call_touch_times index kit_in_tez (pred n) new_tezos new_params

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
    let target = FixedPoint.one in
    assert_equal
      ~printer:FixedPoint.show
      FixedPoint.zero
      (Parameters.compute_drift_derivative target);

    (* exp( low ): 201/200 = 1.005 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "1.0147AE147AE147AE" in
    assert_equal
      ~printer:FixedPoint.show
      FixedPoint.zero
      (Parameters.compute_drift_derivative target);

    (* exp(-low ): 199/200 = 0.995 (rounded UP) *)
    let target = FixedPoint.of_hex_string "0.FEB851EB851EB852" in
    assert_equal
      ~printer:FixedPoint.show
      FixedPoint.zero
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_low_positive_acceleration =
  "test_compute_drift_derivative_low_positive_acceleration" >:: fun _ ->
    (* exp( low ): 201/200 = 1.005 (rounded UP) *)
    let target = FixedPoint.of_hex_string "1.0147AE147AE147AF" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "0.000000000003C547")
      (Parameters.compute_drift_derivative target);

    (* exp( high): 21/20   = 1.05 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "1.0CCCCCCCCCCCCCCC" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "0.000000000003C547")
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_low_negative_acceleration =
  "test_compute_drift_derivative_low_negative_acceleration" >:: fun _ ->
    (* exp(-low ): 199/200 = 0.995 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "0.FEB851EB851EB851" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "-0.000000000003C547")
      (Parameters.compute_drift_derivative target);

    (* exp(-high): 19/20   = 0.95 (rounded UP) *)
    let target = FixedPoint.of_hex_string "0.F333333333333334" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "-0.000000000003C547")
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_high_positive_acceleration =
  "test_compute_drift_derivative_high_positive_acceleration" >:: fun _ ->
    (* exp( high): 21/20   = 1.05 (rounded UP) *)
    let target = FixedPoint.of_hex_string "1.0CCCCCCCCCCCCCCD" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "0.000000000012DA63")
      (Parameters.compute_drift_derivative target)

let test_compute_drift_derivative_high_negative_acceleration =
  "test_compute_drift_derivative_high_negative_acceleration" >:: fun _ ->
    (* exp(-high): 19/20   = 0.95 (rounded DOWN) *)
    let target = FixedPoint.of_hex_string "0.F333333333333333" in
    assert_equal
      ~printer:FixedPoint.show
      (FixedPoint.of_hex_string "-0.000000000012DA63")
      (Parameters.compute_drift_derivative target)

(* ************************************************************************* *)
(*                     compute_imbalance (unit tests)                        *)
(* ************************************************************************* *)

let test_compute_imbalance_all_zero =
  "test_compute_imbalance_all_zero" >:: fun _ ->
    let burrowed    = Kit.zero in
    let circulating = Kit.zero in
    assert_equal
      ~printer:Ratio.show
      Ratio.zero
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_zero_burrowed =
  "test_compute_imbalance_zero_burrowed" >:: fun _ ->
    let burrowed    = Kit.zero in
    let circulating = Kit.one in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal (-5)) (Ligo.int_from_literal 100))
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_equal =
  "test_compute_imbalance_equal" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    assert_equal
      ~printer:Ratio.show
      Ratio.zero
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_small =
  "test_compute_imbalance_positive_small" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal   800_000_001) in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal 199999999) (Ligo.int_from_literal 4000000000)) (* JUST BELOW SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_big =
  "test_compute_imbalance_positive_big" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal   800_000_000) in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal 5) (Ligo.int_from_literal 100)) (* JUST ABOVE SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_capped =
  "test_compute_imbalance_positive_capped" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal             1) in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal 5) (Ligo.int_from_literal 100)) (* SATURATED *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_small =
  "test_compute_imbalance_negative_small" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal   833_333_334) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal (-83333333)) (Ligo.int_from_literal 1666666668)) (* JUST BELOW SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_big =
  "test_compute_imbalance_negative_big" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal   833_333_333) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal (-5)) (Ligo.int_from_literal 100)) (* JUST ABOVE SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_capped =
  "test_compute_imbalance_negative_capped" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Ligo.int_from_literal             1) in
    let circulating = Kit.of_mukit (Ligo.int_from_literal 1_000_000_000) in
    assert_equal
      ~printer:Ratio.show
      (Ratio.make (Ligo.int_from_literal (-5)) (Ligo.int_from_literal 100)) (* SATURATED *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

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
  Ratio.leq
    (Parameters.compute_imbalance ~burrowed ~circulating)
    (Ratio.make (Ligo.int_from_literal 5) (Ligo.int_from_literal 100))

(* Imbalance can never go below -5% *)
let test_imbalance_lower_bound =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_lower_bound"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating) ->
  Ratio.geq
    (Parameters.compute_imbalance ~burrowed ~circulating)
    (Ratio.make (Ligo.int_from_literal (-5)) (Ligo.int_from_literal 100))

(* The sign of imbalance is the same as of (burrowed - circulating).
 * If burrowed > circulating then imbalance > 0
 * If burrowed < circulating then imbalance < 0
 * If burrowed = circulating then imbalance = 0 (NOTE: rarely checked, I guess)
*)
let test_imbalance_sign_preservation =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_sign_preservation"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating) ->
  Ratio.sign (Parameters.compute_imbalance ~burrowed ~circulating)
  = Ratio.sign (Kit.to_ratio (Kit.sub burrowed circulating))

(* If burrowed = circulating then imbalance = 0. *)
let test_imbalance_is_zero_when_equal =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_is_zero_when_equal"
    ~count:property_test_count
    TestArbitrary.arb_kit
  @@ fun kit ->
  Ratio.equal
    (Parameters.compute_imbalance ~burrowed:kit ~circulating:kit)
    Ratio.zero

(* For a fixed amount of kit in circulation, increasing the burrowed kit
 * increases the imbalance. *)
let test_imbalance_positive_tendencies =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_positive_tendencies"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (kit1, kit2, kit3) ->
  (* If burrowed1 > burrowed2 > circulating then
   * (compute_imbalance burrowed1 circulating) >= (compute_imbalance burrowed2 circulating) *)
  let (circulating, burrowed2, burrowed1) = (
    (* Just using sorting, to avoid expensive assume-conditionals. *)
    match List.stable_sort Kit.compare [kit1;kit2;kit3;] with
    | [circulating; burrowed2; burrowed1] -> (circulating, burrowed2, burrowed1)
    | _ -> failwith "impossible"
  ) in
  Ratio.geq
    (Parameters.compute_imbalance ~burrowed:burrowed1 ~circulating)
    (Parameters.compute_imbalance ~burrowed:burrowed2 ~circulating)

(* For a fixed amount of burrowed kit, increasing the kit in circulation
 * decreases the imbalance. *)
let test_imbalance_negative_tendencies =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_imbalance_negative_tendencies"
    ~count:property_test_count
    (QCheck.triple TestArbitrary.arb_kit TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (kit1, kit2, kit3) ->
  (* If circulating1 > circulating2 > burrowed then
   * (compute_imbalance burrowed circulating1) <= (compute_imbalance burrowed circulating2) *)
  let (burrowed, circulating2, circulating1) = (
    (* Just using sorting, to avoid expensive assume-conditionals. *)
    match List.stable_sort Kit.compare [kit1;kit2;kit3;] with
    | [burrowed; circulating2; circulating1] -> (burrowed, circulating2, circulating1)
    | _ -> failwith "impossible"
  ) in
  Ratio.leq
    (Parameters.compute_imbalance ~burrowed ~circulating:circulating1)
    (Parameters.compute_imbalance ~burrowed ~circulating:circulating2)

(* ************************************************************************* *)
(*                          Index/Protected Index                            *)
(* ************************************************************************* *)

(* The protected index should always follow the tendency of the given index,
 * independently of whether that happens fast or slowly. *)
let test_protected_index_follows_index =
  (* initial *)
  let tezos = initial_tezos in
  let params = Parameters.make_initial tezos.now in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = Ratio.one in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_protected_index_follows_index"
    ~count:property_test_count
    (QCheck.pair TestArbitrary.arb_small_tez QCheck.small_nat)
  @@ fun (index, lvl) ->
  let lvl = lvl + 1 in (* let time pass, please *)
  let new_tezos =
    { tezos with
      now = Ligo.timestamp_from_seconds_literal (lvl * 60);
      level = Level.of_int lvl;
    } in

  let _total_accrual_to_uniswap, new_params =
    Parameters.touch new_tezos index kit_in_tez params in

  assert_equal
    (compare new_params.index params.index)
    (compare new_params.protected_index params.protected_index)
    ~printer:string_of_int;
  true

(* The protected index should not follow the tendency of the given index "too
 * fast". According to current expectations, the protected index should be able
 * to catch up to a 2x or 0.5x move in 24 hours, and a 3% move in an hour. *)
let test_protected_index_pace =
  "test_protected_index_pace" >:: fun _ ->
    (* initial *)
    let tezos = initial_tezos in
    let params = Parameters.make_initial tezos.now in

    (* Neutral kit_in_tez (same as initial) *)
    let kit_in_tez = Ratio.one in

    (* UPWARD MOVES *)
    let very_high_index = Tez.of_ratio_floor (Ratio.mul (Ratio.of_int 1000) (Tez.to_ratio params.index)) in
    (* One hour, upward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 1.030420 (=103.0420% of initial; slightly over 3%) *)
    let new_params = call_touch_times very_high_index kit_in_tez (60 (* 60 blocks ~ 1h *)) tezos params in
    assert_equal ~printer:Tez.show (Tez.of_mutez (Ligo.int_from_literal 1_030_420)) new_params.protected_index;
    (* One day, upward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 2.053031 (=205.3031% of initial; slightly over double) *)
    let new_params = call_touch_times very_high_index kit_in_tez (60 * 24 (* 60 blocks ~ 1h *)) tezos params in
    assert_equal ~printer:Tez.show (Tez.of_mutez (Ligo.int_from_literal 2_053_031)) new_params.protected_index;

    (* DOWNWARD MOVES *)
    let very_low_index = Tez.of_ratio_floor (Ratio.mul (Ratio.make (Ligo.int_from_literal 1) (Ligo.int_from_literal 1000)) (Tez.to_ratio params.index)) in
    (* One hour, downward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 0.970407 (=2.9593% less than initial; slightly under 3% *)
    let new_params = call_touch_times very_low_index kit_in_tez (60 (* 60 blocks ~ 1h *)) tezos params in
    assert_equal ~printer:Tez.show (Tez.of_mutez (Ligo.int_from_literal 970_407)) new_params.protected_index;
    (* One day, downward move, touched in every block *)
    (* Initial : 1.000000 *)
    (* Final   : 0.486151 (=51.3849% less than initial; slightly more than halved) *)
    let new_params = call_touch_times very_low_index kit_in_tez (60 * 24 (* 60 blocks ~ 1h *)) tezos params in
    assert_equal ~printer:Tez.show (Tez.of_mutez (Ligo.int_from_literal 486_151)) new_params.protected_index

(* ************************************************************************* *)
(*                                 Prices                                    *)
(* ************************************************************************* *)

(* The pace of change of the minting index is bounded on the low side. George:
 * What about the pace of change of the minting price (affected also by the
 * current quantity q)? *)
let test_minting_index_low_bounded =
  (* initial *)
  let tezos = initial_tezos in
  let params = Parameters.make_initial tezos.now in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = Ratio.one in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_minting_index_low_bounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Tez.of_mutez (Ligo.int_from_literal x)) QCheck.(0 -- 999_999))
  @@ fun index ->
  (* just the next block *)
  let new_tezos =
    { tezos with
      now = Ligo.timestamp_from_seconds_literal 60;
      level = Level.of_int 1;
    } in
  let _total_accrual_to_uniswap, new_params =
    Parameters.touch new_tezos index kit_in_tez params in
  (Parameters.tz_minting new_params >= Tez.of_mutez (Ligo.int_from_literal 999_500)) (* 0.05% down, at "best" *)

(* The pace of change of the minting index is unbounded on the high side.
 * George: What about the pace of change of the minting price (affected also by
 * the current quantity q)? *)
let test_minting_index_high_unbounded =
  (* initial *)
  let tezos = initial_tezos in
  let params = Parameters.make_initial tezos.now in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = Ratio.one in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_minting_index_high_unbounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Tez.of_mutez (Ligo.int_from_literal x)) QCheck.(1_000_001 -- max_int))
  @@ fun index ->
  (* just the next block *)
  let new_tezos =
    { tezos with
      now = Ligo.timestamp_from_seconds_literal 60;
      level = Level.of_int 1;
    } in
  let _total_accrual_to_uniswap, new_params =
    Parameters.touch new_tezos index kit_in_tez params in
  assert_equal
    index
    (Parameters.tz_minting new_params)
    ~printer:Tez.show;
  true

(* The pace of change of the liquidation index is bounded on the high side.
 * George: What about the pace of change of the liquidation price (affected
 * also by the current quantity q)? *)
let test_liquidation_index_high_bounded =
  (* initial *)
  let tezos = initial_tezos in
  let params = Parameters.make_initial tezos.now in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = Ratio.one in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_liquidation_index_high_bounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Tez.of_mutez (Ligo.int_from_literal x)) QCheck.(1_000_001 -- max_int))
  @@ fun index ->
  (* just the next block *)
  let new_tezos =
    { tezos with
      now = Ligo.timestamp_from_seconds_literal 60;
      level = Level.of_int 1;
    } in
  let _total_accrual_to_uniswap, new_params =
    Parameters.touch new_tezos index kit_in_tez params in
  (* not very likely to hit the < case here I think;
   * perhaps we need a different generator *)
  (Parameters.tz_liquidation new_params <= Tez.of_mutez (Ligo.int_from_literal 1_000_500)) (* 0.05% up, at "best" *)

(* The pace of change of the liquidation index is unbounded on the low side.
 * George: What about the pace of change of the liquidation price (affected
 * also by the current quantity q)? *)
let test_liquidation_index_low_unbounded =
  (* initial *)
  let tezos = initial_tezos in
  let params = Parameters.make_initial tezos.now in

  (* Neutral kit_in_tez (same as initial) *)
  let kit_in_tez = Ratio.one in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_liquidation_index_low_unbounded"
    ~count:property_test_count
    (QCheck.map (fun x -> Tez.of_mutez (Ligo.int_from_literal x)) QCheck.(0 -- 999_999))
  @@ fun index ->
  (* just the next block *)
  let new_tezos =
    { tezos with
      now = Ligo.timestamp_from_seconds_literal 60;
      level = Level.of_int 1;
    } in
  let _total_accrual_to_uniswap, new_params =
    Parameters.touch new_tezos index kit_in_tez params in
  assert_equal
    index
    (Parameters.tz_liquidation new_params)
    ~printer:Tez.show;
  true

(* ************************************************************************* *)
(*                                  touch                                    *)
(* ************************************************************************* *)

(* With the index staying at its initial value and the price of kit in tez
 * fixed at 1, most of the parameters should stay the same, even if a long time
 * passes. In fact, the only parameters that should change are (a) the
 * timestamp, naturally, (b) the burrowing fee index, which is always
 * increasing, and (c) the number of burrowed and circulating kit, due to the
 * increased burrowing fee index. *)
let test_touch_identity =
  (* initial *)
  let tezos = initial_tezos in
  let params = Parameters.make_initial tezos.now in

  (* neutral arguments *)
  let index = params.index in
  let kit_in_tez = Ratio.one in

  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_touch_identity"
    ~count:property_test_count
    TestArbitrary.arb_tezos
  @@ fun new_tezos ->
  let _total_accrual_to_uniswap, new_params = Parameters.touch new_tezos index kit_in_tez params in

  (* Most of the parameters remain the same *)
  assert_equal
    { params with
      last_touched = new_params.last_touched;
      burrow_fee_index = new_params.burrow_fee_index;
      outstanding_kit = new_params.outstanding_kit;
      circulating_kit = new_params.circulating_kit;
    }
    new_params
    ~printer:Parameters.show;

  (* Burrow fee index though is ever increasing (if time passes!) *)
  assert_equal
    (compare new_tezos.now tezos.now)
    (compare new_params.burrow_fee_index params.burrow_fee_index)
    ~printer:string_of_int;

  (* Outstanding kit and circulating kit increase (imbalance starts at zero
   * and stays there, so we only have burrowing fees). *)
  assert_bool
    "outstanding kit should increase over time"
    (new_params.outstanding_kit >= params.outstanding_kit); (* most of the time equal, but over enough time greater-than *)
  assert_bool
    "circulating kit should increase over time"
    (new_params.circulating_kit >= params.circulating_kit); (* most of the time equal, but over enough time greater-than *)
  assert_bool
    "imbalance should not increase or decrease over time"
    (let new_imbalance =
       Parameters.compute_imbalance
         ~burrowed:new_params.outstanding_kit
         ~circulating:new_params.circulating_kit in
     let old_imbalance =
       Parameters.compute_imbalance
         ~burrowed:params.outstanding_kit
         ~circulating:params.circulating_kit in
     new_imbalance = old_imbalance);
  true

(* Just a simple unit test, testing nothing specific, really. *)
let test_touch =
  "test_touch" >:: fun _ ->
    let initial_parameters : Parameters.t =
      { q = FixedPoint.of_hex_string "0.E666666666666666"; (* 0.9 *)
        index = Tez.of_mutez (Ligo.int_from_literal 360_000);
        target = FixedPoint.of_hex_string "1.147AE147AE147AE1"; (* 1.08 *)
        protected_index = Tez.of_mutez (Ligo.int_from_literal 350_000);
        drift = FixedPoint.zero;
        drift_derivative = FixedPoint.zero;
        burrow_fee_index = FixedPoint.one;
        imbalance_index = FixedPoint.one;
        outstanding_kit = Kit.one;
        circulating_kit = Kit.zero;
        last_touched = Ligo.timestamp_from_seconds_literal 0;
      } in
    let tezos = Tezos.{
        now = Ligo.timestamp_from_seconds_literal 3600;
        level = Level.of_int 60;
        self = Address.of_string "checker";
      } in

    let new_index = Tez.of_mutez (Ligo.int_from_literal 340_000) in
    let kit_in_tez = Ratio.make (Ligo.int_from_literal 305) (Ligo.int_from_literal 1000) in
    let total_accrual_to_uniswap, new_parameters = Parameters.touch tezos new_index kit_in_tez initial_parameters in
    assert_equal
      { q = FixedPoint.of_hex_string "0.E6666895A3EC8BA5"; (* 0.90000013020828555983 *)
        index = Tez.of_mutez (Ligo.int_from_literal 340_000);
        protected_index = Tez.of_mutez (Ligo.int_from_literal 340_000);
        target = FixedPoint.of_hex_string "1.00D6E1B366FF4BEE"; (* 1.00327883367481013224 *)
        drift_derivative = FixedPoint.of_hex_string "0.000000000012DA63"; (* 0.00000000000006697957 *)
        drift  = FixedPoint.of_hex_string "0.00000000848F8818"; (* 0.00000000012056322737 *)
        burrow_fee_index = FixedPoint.of_hex_string "1.00000991D674CC29"; (* 1.00000057039729312258 *)
        imbalance_index = FixedPoint.of_hex_string "1.00005FB2608FF99D"; (* 1.000005703972931226 *)
        outstanding_kit = Kit.of_mukit (Ligo.int_from_literal 1_000_005);
        circulating_kit = Kit.of_mukit (Ligo.int_from_literal 0_000_000); (* NOTE that it ends up being identical to the one we started with *)
        last_touched = tezos.now;
      }
      new_parameters
      ~printer:Parameters.show;
    assert_equal
      Kit.zero (* NOTE: I'd expect this to be higher I think. *)
      total_accrual_to_uniswap
      ~printer:Kit.show

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
    test_compute_imbalance_zero_burrowed;
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

    (* price movement *)
    test_minting_index_low_bounded;
    test_minting_index_high_unbounded;

    test_liquidation_index_high_bounded;
    test_liquidation_index_low_unbounded;

    (* touch *)
    test_touch_identity;
    test_touch;
  ]
