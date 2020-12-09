open OUnit2

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

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
      ~printer:(Q.sprint ())
      Q.zero
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_zero_burrowed =
  "test_compute_imbalance_zero_burrowed" >:: fun _ ->
    let burrowed    = Kit.zero in
    let circulating = Kit.one in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-5/100")
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_equal =
  "test_compute_imbalance_equal" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int 1_000_000_000) in
    let circulating = Kit.of_mukit (Z.of_int 1_000_000_000) in
    assert_equal
      ~printer:(Q.sprint ())
      Q.zero
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_small =
  "test_compute_imbalance_positive_small" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int 1_000_000_000) in
    let circulating = Kit.of_mukit (Z.of_int   800_000_001) in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "199999999/4000000000") (* JUST BELOW SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_big =
  "test_compute_imbalance_positive_big" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int 1_000_000_000) in
    let circulating = Kit.of_mukit (Z.of_int   800_000_000) in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "5/100") (* JUST ABOVE SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_positive_capped =
  "test_compute_imbalance_positive_capped" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int 1_000_000_000) in
    let circulating = Kit.of_mukit (Z.of_int             1) in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "5/100") (* SATURATED *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_small =
  "test_compute_imbalance_negative_small" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int   833_333_334) in
    let circulating = Kit.of_mukit (Z.of_int 1_000_000_000) in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-83333333/1666666668") (* JUST BELOW SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_big =
  "test_compute_imbalance_negative_big" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int   833_333_333) in
    let circulating = Kit.of_mukit (Z.of_int 1_000_000_000) in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-5/100") (* JUST ABOVE SATURATION *)
      (Parameters.compute_imbalance ~burrowed ~circulating)

let test_compute_imbalance_negative_capped =
  "test_compute_imbalance_negative_capped" >:: fun _ ->
    let burrowed    = Kit.of_mukit (Z.of_int             1) in
    let circulating = Kit.of_mukit (Z.of_int 1_000_000_000) in
    assert_equal
      ~printer:(Q.sprint ())
      (Q.of_string "-5/100") (* SATURATED *)
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
       Q.leq
         (Parameters.compute_imbalance ~burrowed ~circulating)
         (Q.of_string "5/100")

(* Imbalance can never go below -5% *)
let test_imbalance_lower_bound =
  qcheck_to_ounit
  @@ QCheck.Test.make
       ~name:"test_imbalance_lower_bound"
       ~count:property_test_count
       (QCheck.pair TestArbitrary.arb_kit TestArbitrary.arb_kit)
  @@ fun (burrowed, circulating) ->
       Q.geq
         (Parameters.compute_imbalance ~burrowed ~circulating)
         (Q.of_string "-5/100")

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
       Q.sign (Parameters.compute_imbalance ~burrowed ~circulating)
       = Q.sign Kit.(to_q (burrowed - circulating))

(* If burrowed = circulating then imbalance = 0. *)
let test_imbalance_is_zero_when_equal =
  qcheck_to_ounit
  @@ QCheck.Test.make
       ~name:"test_imbalance_is_zero_when_equal"
       ~count:property_test_count
       TestArbitrary.arb_kit
  @@ fun kit ->
       Q.equal
         (Parameters.compute_imbalance ~burrowed:kit ~circulating:kit)
         Q.zero

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
       Q.geq
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
       Q.leq
          (Parameters.compute_imbalance ~burrowed ~circulating:circulating1)
          (Parameters.compute_imbalance ~burrowed ~circulating:circulating2)

(* ************************************************************************* *)
(*                                  touch                                    *)
(* ************************************************************************* *)

let test_touch =
  "test_touch" >:: fun _ ->
    let initial_parameters : Parameters.t =
      { q = FixedPoint.of_hex_string "0.E666666666666666"; (* 0.9 *)
        index = Tez.of_mutez 360_000;
        target = FixedPoint.of_hex_string "1.147AE147AE147AE1"; (* 1.08 *)
        protected_index = Tez.of_mutez 350_000;
        drift = FixedPoint.zero;
        drift' = FixedPoint.zero;
        burrow_fee_index = FixedPoint.one;
        imbalance_index = FixedPoint.one;
        outstanding_kit = Kit.one; (* TODO: What should that be? *)
        circulating_kit = Kit.zero; (* TODO: What should that be? *)
        last_touched = Timestamp.of_seconds 0;
      } in
    let tezos = Tezos.{
        now = Timestamp.of_seconds 3600;
        level = Level.of_int 60;
        self = Address.of_string "checker";
      } in

    let new_index = Tez.of_mutez 340_000 in
    let kit_in_tez = Q.of_string "305/1000" in
    let total_accrual_to_uniswap, new_parameters = Parameters.touch tezos new_index kit_in_tez initial_parameters in
    assert_equal
      { q = FixedPoint.of_hex_string "0.E6666895A3EC8BA5"; (* 0.90000013020828555983 *)
        index = Tez.of_mutez 340_000;
        protected_index = Tez.of_mutez 340_000;
        target = FixedPoint.of_hex_string "1.00D6E1B366FF4BEE"; (* 1.00327883367481013224 *)
        drift' = FixedPoint.of_hex_string "0.000000000012DA63"; (* 0.00000000000006697957 *)
        drift  = FixedPoint.of_hex_string "0.00000000848F8818"; (* 0.00000000012056322737 *)
        burrow_fee_index = FixedPoint.of_hex_string "1.00000991D674CC29"; (* 1.00000057039729312258 *)
        imbalance_index = FixedPoint.of_hex_string "1.00005FB2608FF99D"; (* 1.000005703972931226 *)
        outstanding_kit = Kit.of_mukit (Z.of_int 1_000_005);
        circulating_kit = Kit.of_mukit (Z.of_int 0_000_000); (* NOTE that it ends up being identical to the one we started with *)
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

    (* touch *)
    test_touch;
  ]
