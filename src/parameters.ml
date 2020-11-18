(* ************************************************************************* *)
(*                               Parameters                                  *)
(* ************************************************************************* *)
type t =
  { (* TODO: Perhaps maintain 1/q instead of q? TBD *)
    q : FixedPoint.t; (* 1/kit, really *)
    index: Tez.t;
    protected_index: Tez.t;
    target: FixedPoint.t;
    drift': FixedPoint.t;
    drift: FixedPoint.t;
    burrow_fee_index: FixedPoint.t;
    imbalance_index: FixedPoint.t;
    (* TODO: What would be a good starting value for this? Cannot be zero
     * because then it stays zero forever (only multiplications occur). *)
    outstanding_kit: Kit.t;
    circulating_kit: Kit.t;
    last_touched: Timestamp.t;
  }
[@@deriving show]

(** Initial state of the parameters. TODO: Contents TBD. *)
let make_initial (ts: Timestamp.t) : t =
  { q = FixedPoint.one;
    index = Tez.one;
    protected_index = Tez.one;
    target = FixedPoint.one;
    drift = FixedPoint.zero;
    drift' = FixedPoint.zero;
    burrow_fee_index = FixedPoint.one;
    imbalance_index = FixedPoint.one;
    outstanding_kit = Kit.of_mukit 1_000_000;
    circulating_kit = Kit.of_mukit 1_000_000;
    last_touched = ts;
  }

(* tez. To get tez/kit must multiply with q. *)
let tz_minting (p: t) : Tez.t = max p.index p.protected_index

(* tez. To get tez/kit must multiply with q. *)
let tz_liquidation (p: t) : Tez.t = min p.index p.protected_index

(** Current minting price (tez/kit). *)
let minting_price (p: t) : Q.t =
  Q.(FixedPoint.to_q p.q * Tez.to_q (tz_minting p))

(** Current liquidation price (tez/kit). *)
let liquidation_price (p: t) : Q.t =
  Q.(FixedPoint.to_q p.q * Tez.to_q (tz_liquidation p))

(** Given the amount of kit necessary to close all existing burrows
  * (burrowed) and the amount of kit that are currently in circulation,
  * compute the current imbalance adjustment (can be either a fee or a
  * bonus).
  *
  * If we call "burrowed" the total amount of kit necessary to close all
  * existing burrows, and "circulating" the total amount of kit in circulation,
  * then the imbalance fee/bonus is calculated as follows (per year):
  *
  *   min(   5 * burrowed, (burrowed - circulating) ) * 1.0 cNp / burrowed , if burrowed >= circulating
  *   max( - 5 * burrowed, (burrowed - circulating) ) * 1.0 cNp / burrowed , otherwise
*)
let compute_imbalance ~(burrowed: Kit.t) ~(circulating: Kit.t) : FixedPoint.t =
  assert (burrowed >= Kit.zero);
  assert (circulating >= Kit.zero);
  (* No kit in burrows or in circulation means no imbalance adjustment *)
  if burrowed = Kit.zero then
    (* TODO: George: though unlikely, it is possible to have kit in
     * circulation, even when nothing is burrowed. How can we compute the
     * imbalance in this edge case? *)
    (assert (circulating = Kit.zero); FixedPoint.zero) (* George: the assert is just as a reminder *)
  else if burrowed = circulating then
    FixedPoint.zero (* George: I add this special case, to avoid rounding issues *)
  else
    FixedPoint.(
      Kit.(to_fp
             (scale
                (min (scale burrowed (of_string "5.0")) (burrowed - circulating))
                (of_string "0.01"))
          ) / Kit.to_fp burrowed
    )

(** Compute the current adjustment index. Basically this is the product of
  * the burrow fee index and the imbalance adjustment index. *)
let compute_adjustment_index (p: t) : FixedPoint.t =
  let burrow_fee_index = FixedPoint.to_q p.burrow_fee_index in
  let imbalance_index = FixedPoint.to_q p.imbalance_index in
  FixedPoint.of_q_floor Q.(burrow_fee_index * imbalance_index) (* TODO: round up or down here? *)

(** Given the current target, calculate the rate of change of the drift (drift
  * derivative). Thresholds were given in cnp / day^2, so we convert them to
  * cnp / second^2, assuming we're measuring time in seconds. Also, since exp
  * is monotonic, we exponentiate the whole equation to avoid using log. TODO:
  * double-check these calculations. *)
let compute_drift_derivative (target : FixedPoint.t) : FixedPoint.t =
  assert (target > FixedPoint.zero);
  FixedPoint.(
    let cnp_001 = of_string "0.01" / of_string "100.0" in
    let cnp_005 = of_string "0.05" / of_string "100.0" in
    let secs_in_a_day = of_int Stdlib.(24 * 3600) in
    match () with
    (* No acceleration (0) *)
    | () when exp (neg Constants.target_low_bracket) < target && target < exp Constants.target_low_bracket -> zero
    (* Low acceleration (-/+) *)
    | () when exp (neg Constants.target_high_bracket) < target && target <= exp (neg Constants.target_low_bracket) -> neg (cnp_001 / pow secs_in_a_day 2)
    | () when exp      Constants.target_high_bracket  > target && target >= exp      Constants.target_low_bracket  ->     (cnp_001 / pow secs_in_a_day 2)
    (* High acceleration (-/+) *)
    | () when target <= exp (neg Constants.target_high_bracket) -> neg (cnp_005 / pow secs_in_a_day 2)
    | () when target >= exp      Constants.target_high_bracket  ->     (cnp_005 / pow secs_in_a_day 2)
    | _ -> failwith "impossible"
  )

let clamp (v: 'a) (lower: 'a) (upper: 'a) : 'a =
  assert (lower <= upper);
  min upper (max v lower)

(** Update the checker's parameters, given (a) the current timestamp
  * (Tezos.now), (b) the current index (the median of the oracles right now),
  * and (c) the current price of kit in tez, as given by the uniswap
  * sub-contract. *)
let touch
    (tezos: Tezos.t)
    (current_index: FixedPoint.t)
    (current_kit_in_tez: Q.t)
    (parameters: t)
  : Kit.t * t =
  (* TODO: George: I think that we should use Q everywhere for the
   * calculations, and only cast to FixedPoint.t before storing it. Otherwise
   * we can have signigicant precision loss. *)
  let current_kit_in_tez = FixedPoint.of_q_floor current_kit_in_tez in
  let duration_in_seconds = FixedPoint.of_int (Timestamp.seconds_elapsed ~start:parameters.last_touched ~finish:tezos.now) in
  let seconds_in_a_year = FixedPoint.of_int Constants.seconds_in_a_year in
  let current_protected_index =
    let upper_lim = FixedPoint.(exp     (Constants.protected_index_epsilon * duration_in_seconds)) in
    let lower_lim = FixedPoint.(exp (neg Constants.protected_index_epsilon * duration_in_seconds)) in
    FixedPoint.(
      Tez.to_fp parameters.protected_index
      * clamp
        (current_index / Tez.to_fp parameters.protected_index)
        lower_lim
        upper_lim
    ) in
  let current_drift' = compute_drift_derivative parameters.target in
  let current_drift =
    FixedPoint.(
      parameters.drift
      + (of_int 1 / of_int 2)
        * (parameters.drift' + current_drift')
        * duration_in_seconds
    ) in

  let current_q =
    FixedPoint.(
      parameters.q
      * exp ( ( parameters.drift
                + (of_int 1 / of_int 6)
                  * (of_int 2 * (parameters.drift' + current_drift'))
                  * duration_in_seconds )
              * duration_in_seconds )
    ) in
  let current_target = FixedPoint.(current_q * current_index / current_kit_in_tez) in

  (* Update the indices *)
  let current_burrow_fee_index = FixedPoint.(
      parameters.burrow_fee_index * (one + Constants.burrow_fee_percentage * duration_in_seconds / seconds_in_a_year)
    ) in
  let imbalance_percentage = compute_imbalance ~burrowed:parameters.outstanding_kit ~circulating:parameters.circulating_kit in
  let current_imbalance_index = FixedPoint.(
      parameters.imbalance_index * (one + imbalance_percentage * duration_in_seconds / seconds_in_a_year)
    ) in
  let with_burrow_fee = Kit.scale Kit.one FixedPoint.(Kit.to_fp parameters.outstanding_kit * current_burrow_fee_index / parameters.burrow_fee_index) in
  let total_accrual_to_uniswap = Kit.(with_burrow_fee - parameters.outstanding_kit) in
  let current_outstanding_kit = Kit.scale Kit.one FixedPoint.(Kit.to_fp with_burrow_fee * (current_imbalance_index / parameters.imbalance_index)) in
  let current_circulating_kit = Kit.(parameters.circulating_kit + total_accrual_to_uniswap) in
  ( total_accrual_to_uniswap
  , {
    index = Tez.(scale one current_index);
    protected_index = Tez.(scale one current_protected_index);
    target = current_target;
    drift = current_drift;
    drift' = current_drift';
    q = current_q;
    burrow_fee_index = current_burrow_fee_index;
    imbalance_index = current_imbalance_index;
    outstanding_kit = current_outstanding_kit;
    circulating_kit = current_circulating_kit;
    last_touched = tezos.now;
  }
  )

(** Add some kit to the kit in circulation. *)
let add_circulating_kit (parameters: t) (kit: Kit.t) : t =
  assert (kit >= Kit.zero);
  { parameters with circulating_kit = Kit.(parameters.circulating_kit + kit); }

(** Remove some kit from circulation. *)
let remove_circulating_kit (parameters: t) (kit: Kit.t) : t =
  assert (kit >= Kit.zero);
  assert (parameters.circulating_kit >= kit);
  { parameters with circulating_kit = Kit.(parameters.circulating_kit - kit); }

