open OUnit2

let suite =
  "Parameters tests" >::: [
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
    ("test_compute_drift_derivative_no_acceleration" >:: fun _ ->
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
          (Parameters.compute_drift_derivative target);
    );

    ("test_compute_drift_derivative_low_positive_acceleration" >:: fun _ ->
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
          (Parameters.compute_drift_derivative target);
    );

    ("test_compute_drift_derivative_low_negative_acceleration" >:: fun _ ->
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
          (Parameters.compute_drift_derivative target);
    );

    ("test_compute_drift_derivative_high_positive_acceleration" >:: fun _ ->
        (* exp( high): 21/20   = 1.05 (rounded UP) *)
        let target = FixedPoint.of_hex_string "1.0CCCCCCCCCCCCCCD" in
        assert_equal
          ~printer:FixedPoint.show
          (FixedPoint.of_hex_string "0.000000000012DA63")
          (Parameters.compute_drift_derivative target);
    );

    ("test_compute_drift_derivative_high_negative_acceleration" >:: fun _ ->
        (* exp(-high): 19/20   = 0.95 (rounded DOWN) *)
        let target = FixedPoint.of_hex_string "0.F333333333333333" in
        assert_equal
          ~printer:FixedPoint.show
          (FixedPoint.of_hex_string "-0.000000000012DA63")
          (Parameters.compute_drift_derivative target);
    );

    ("test_touch" >:: fun _ ->
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
            imbalance_index = FixedPoint.of_hex_string "1.00001323ACE99852"; (* 1.00000114079458624517 *)
            outstanding_kit = Kit.of_mukit 1_000_001;
            circulating_kit = Kit.of_mukit 0_000_000; (* NOTE that it ends up being identical to the one we started with *)
            last_touched = tezos.now;
          }
          new_parameters
          ~printer:Parameters.show;
        assert_equal
          Kit.zero (* NOTE: I'd expect this to be higher I think. *)
          total_accrual_to_uniswap
          ~printer:Kit.show;
    );
  ]
