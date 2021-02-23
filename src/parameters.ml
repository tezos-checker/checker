open Kit
open Ratio
open FixedPoint
open Common
open Constants
open Error

type parameters =
  { (* TODO: Perhaps maintain 1/q instead of q? TBD *)
    q : fixedpoint; (* 1/kit, really *)
    index: Ligo.tez;
    protected_index: Ligo.tez;
    target: fixedpoint;
    drift_derivative: fixedpoint;
    drift: fixedpoint;
    burrow_fee_index: fixedpoint;
    imbalance_index: fixedpoint;
    (* TODO: Test that this value does not drift too far from the real value
     * (the total amount of kit needed to close all burrows). Errors of a few
     * percents per year are NOT acceptable. Errors of 0.1% or so per year
     * would be tolerable. *)
    outstanding_kit: kit;
    circulating_kit: kit;
    last_touched: Ligo.timestamp;
  }
[@@deriving show]

(** Initial state of the parameters. TODO: Contents TBD. *)
let initial_parameters : parameters =
  { q = fixedpoint_one;
    index = Ligo.tez_from_literal "1_000_000mutez";
    protected_index = Ligo.tez_from_literal "1_000_000mutez";
    target = fixedpoint_one;
    drift = fixedpoint_zero;
    drift_derivative = fixedpoint_zero;
    burrow_fee_index = fixedpoint_one;
    imbalance_index = fixedpoint_one;
    (* Cannot be zero because then it stays
     * zero forever; only multiplications occur. *)
    outstanding_kit = kit_of_mukit (Ligo.nat_from_literal "1n");
    circulating_kit = kit_of_mukit (Ligo.nat_from_literal "1n");
    last_touched = !Ligo.Tezos.now;
  }

(* tez. To get tez/kit must multiply with q. *)
let tz_minting (p: parameters) : Ligo.tez = max_tez p.index p.protected_index

(* tez. To get tez/kit must multiply with q. *)
let tz_liquidation (p: parameters) : Ligo.tez = min_tez p.index p.protected_index

(** Current minting price (tez/kit). *)
let minting_price (p: parameters) : ratio =
  mul_ratio (fixedpoint_to_ratio p.q) (ratio_of_tez (tz_minting p))

(** Current liquidation price (tez/kit). *)
let liquidation_price (p: parameters) : ratio =
  mul_ratio (fixedpoint_to_ratio p.q) (ratio_of_tez (tz_liquidation p))

(** Given the amount of kit necessary to close all existing burrows
  * (burrowed) and the amount of kit that are currently in circulation,
  * compute the current imbalance adjustment (can be either a fee or a
  * bonus).
  *
  * If we call "burrowed" the total amount of kit necessary to close all
  * existing burrows, and "circulating" the total amount of kit in circulation,
  * then the imbalance fee/bonus is calculated as follows (per year):
  *
  *   min((burrowed - circulating) / burrowed,   0.20) * (1/0.20) * 0.05 , if burrowed >= circulating
  *   max((burrowed - circulating) / burrowed, - 0.20) * (1/0.20) * 0.05 , otherwise
  *
  * or, equivalently,
  *
  *   min(5 * (burrowed - circulating),   burrowed) / (20 * burrowed) , if burrowed >= circulating
  *   max(5 * (burrowed - circulating), - burrowed) / (20 * burrowed) , otherwise
  *
  * Edge cases:
  * - burrowed = 0, circulating = 0
  *     The imbalance fee/bonus is 0.
  * - burrowed = 0, circulating > 0
  *     Well, burrowed is "infinitely" smaller than circulating so let's
  *     saturate the imbalance to -5 cNp.
  * NOTE: Alternatively: add (universally) 1mukit to the denominator to avoid
  *   doing conditionals and save gas costs. Messes only slightly with the
  *   computations, but can save quite some gas. *)
let compute_imbalance (burrowed: kit) (circulating: kit) : ratio =
  if burrowed = kit_zero && circulating = kit_zero then
    zero_ratio
  else if burrowed = kit_zero && circulating <> kit_zero then
    make_ratio (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100")
  else if burrowed >= circulating then
    div_ratio
      (min_ratio (mul_ratio (ratio_of_int (Ligo.int_from_literal "5")) (kit_to_ratio (kit_sub burrowed circulating))) (kit_to_ratio burrowed))
      (mul_ratio (ratio_of_int (Ligo.int_from_literal "20")) (kit_to_ratio burrowed))
  else (* burrowed < circulating *)
    neg_ratio
      (div_ratio
         (min_ratio (mul_ratio (ratio_of_int (Ligo.int_from_literal "5")) (kit_to_ratio (kit_sub circulating burrowed))) (kit_to_ratio burrowed))
         (mul_ratio (ratio_of_int (Ligo.int_from_literal "20")) (kit_to_ratio burrowed))
      )

(** Compute the current adjustment index. Basically this is the product of
  * the burrow fee index and the imbalance adjustment index. *)
let compute_adjustment_index (p: parameters) : fixedpoint =
  let burrow_fee_index = fixedpoint_to_ratio p.burrow_fee_index in
  let imbalance_index = fixedpoint_to_ratio p.imbalance_index in
  fixedpoint_of_ratio_floor (mul_ratio burrow_fee_index imbalance_index)

(** Given the current target, calculate the rate of change of the drift (drift
  * derivative). That's how the following calculations came to be:
  *
  *   let X = log (p_t) be the "measure of imbalance". The original doc gave:
  *
  *   d_t' = 0                             if 0       <= |X| < 0.5 cNp
  *   d_t' = sign(X) * 0.01 cNp / day^2    if 0.5 cNp <= |X| <   5 cNp
  *   d_t' = sign(X) * 0.05 cNp / day^2    if   5 cNp <= |X| < infinity
  *
  *   1. Inline the numbers: cNp ~= 1/100, day ~= 24 * 60 * 60 = 86400 seconds
  *
  *   d_t' = 0                             if 0     <= |X| < 0.005
  *   d_t' = sign(X) * 0.0001 / 86400^2    if 0.005 <= |X| < 0.05
  *   d_t' = sign(X) * 0.0005 / 86400^2    if 0.05  <= |X| < infinity
  *
  *   2. Remove absolute values
  *
  *   d_t' =  0                   if -0.005 <  X <  0.005
  *   d_t' = +0.0001 / 86400^2    if +0.005 <= X < +0.05
  *   d_t' = -0.0001 / 86400^2    if -0.005 >= X > -0.05
  *   d_t' = +0.0005 / 86400^2    if +0.05  <= X < +infinity
  *   d_t' = -0.0005 / 86400^2    if -0.05  >= X > -infinity
  *
  *   3. Exponentiate the inequalities
  *
  *   d_t' =  0                   if exp(-0.005) <  p_t < exp(+0.005)
  *   d_t' = +0.0001 / 86400^2    if exp(+0.005) <= p_t < exp(+0.05)
  *   d_t' = -0.0001 / 86400^2    if exp(-0.005) >= p_t > exp(-0.05)
  *   d_t' = +0.0005 / 86400^2    if exp(+0.05)  <= p_t < +infinity
  *   d_t' = -0.0005 / 86400^2    if exp(-0.05)  >= p_t > -infinity
  *
  * I've left these calculations here so that others could double-check them too.
*)
let compute_drift_derivative (target : fixedpoint) : fixedpoint =
  assert (target > fixedpoint_zero);
  let target = fixedpoint_to_ratio target in
  let target_low_bracket  = target_low_bracket in
  let target_high_bracket = target_high_bracket in
  let cnp_001 = fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "1") (Ligo.int_from_literal "10000")) in
  let cnp_005 = fixedpoint_of_ratio_floor (make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "10000")) in
  let secs_in_a_day = fixedpoint_of_int seconds_in_a_day in

  if lt_ratio_ratio (qexp (neg_ratio target_low_bracket)) target && lt_ratio_ratio target (qexp target_low_bracket) then
    (* No acceleration (0) *)
    fixedpoint_zero
  else if lt_ratio_ratio (qexp (neg_ratio target_high_bracket)) target && leq_ratio_ratio target (qexp (neg_ratio target_low_bracket)) then
    (* Low acceleration (-) *)
    fixedpoint_neg (fixedpoint_div cnp_001 (fixedpoint_mul secs_in_a_day secs_in_a_day))
  else if gt_ratio_ratio (qexp (          target_high_bracket)) target && geq_ratio_ratio target (qexp (          target_low_bracket)) then
    (* Low acceleration (+) *)
    (fixedpoint_div cnp_001 (fixedpoint_mul secs_in_a_day secs_in_a_day))
  else if leq_ratio_ratio target (qexp (neg_ratio target_high_bracket)) then
    (* High acceleration (-) *)
    fixedpoint_neg (fixedpoint_div cnp_005 (fixedpoint_mul secs_in_a_day secs_in_a_day))
  else if geq_ratio_ratio target (qexp (          target_high_bracket)) then
    (* High acceleration (+) *)
    (fixedpoint_div cnp_005 (fixedpoint_mul secs_in_a_day secs_in_a_day))
  else
    (failwith "impossible" : fixedpoint)

(** Update the checker's parameters, given (a) the current timestamp
  * (Tezos.now), (b) the current index (the median of the oracles right now),
  * and (c) the current price of kit in tez, as given by the uniswap
  * sub-contract. *)
let parameters_touch
    (current_index: Ligo.tez)
    (current_kit_in_tez: ratio)
    (parameters: parameters)
  : kit * parameters =
  let duration_in_seconds = (* NOTE: can it be negative? Does the protocol ensure this? *)
    let duration =
      ratio_of_int (Ligo.sub_timestamp_timestamp !Ligo.Tezos.now parameters.last_touched) in
    if lt_ratio_ratio duration zero_ratio
    then (Ligo.failwith error_TouchParametersInThePast : ratio)
    else duration
  in

  let current_protected_index =
    let upper_lim = qexp (mul_ratio           (protected_index_epsilon) duration_in_seconds) in
    let lower_lim = qexp (mul_ratio (neg_ratio protected_index_epsilon) duration_in_seconds) in
    let ratio = make_ratio (tez_to_mutez current_index) (tez_to_mutez parameters.protected_index) in
    ratio_to_tez_floor
      (mul_ratio
         (ratio_of_tez parameters.protected_index)
         (clamp ratio lower_lim upper_lim)
      ) in
  let current_drift_derivative =
    compute_drift_derivative parameters.target in
  let current_drift =
    fixedpoint_of_ratio_floor
      (add_ratio
         (fixedpoint_to_ratio parameters.drift)
         (mul_ratio
            (make_ratio (Ligo.int_from_literal "1") (Ligo.int_from_literal "2"))
            (mul_ratio
               (fixedpoint_to_ratio (fixedpoint_add parameters.drift_derivative current_drift_derivative))
               duration_in_seconds
            )
         )
      ) in
  let current_q =
    fixedpoint_of_ratio_floor
      (mul_ratio
         (fixedpoint_to_ratio parameters.q)
         (qexp
            (mul_ratio
               (add_ratio
                  (fixedpoint_to_ratio parameters.drift)
                  (mul_ratio
                     (make_ratio (Ligo.int_from_literal "1") (Ligo.int_from_literal "6"))
                     (mul_ratio
                        (add_ratio
                           (mul_ratio
                              (ratio_of_int (Ligo.int_from_literal "2"))
                              (fixedpoint_to_ratio parameters.drift_derivative)
                           )
                           (fixedpoint_to_ratio current_drift_derivative)
                        )
                        duration_in_seconds
                     )
                  )
               )
               duration_in_seconds
            )
         )
      ) in

  let current_target =
    fixedpoint_of_ratio_floor
      (div_ratio
         (mul_ratio
            (fixedpoint_to_ratio current_q)
            (ratio_of_tez current_index)
         )
         current_kit_in_tez
      ) in
  (* Update the indices *)
  let current_burrow_fee_index =
    (* NOTE: This formula means that burrow_fee_index is ever-increasing. *)
    fixedpoint_of_ratio_floor
      (mul_ratio
         (fixedpoint_to_ratio parameters.burrow_fee_index)
         (add_ratio
            one_ratio
            (div_ratio
               (mul_ratio burrow_fee_percentage duration_in_seconds)
               (ratio_of_int seconds_in_a_year)
            )
         )
      ) in

  let current_imbalance_index =
    let imbalance_rate =
      compute_imbalance
        parameters.outstanding_kit (* burrowed *)
        parameters.circulating_kit (* circulating *) in
    (* NOTE: Even if the imbalance_rate is bounded (from -5 cNp to +5 cNp), the
     * following calculation of the balance index is not. We use the formula
     *
     *   imbalance_index_{i+1} = imbalance_index_i
     *                         * (1 + imbalance * (t_{i+1} - t_i) / <seconds_in_a_year>)
     *
     * which for imbalance = -5cNp and 20 years time in seconds elapsed gives
     * imbalance_index_{i+1} = 0 (for longer time it gives
     * imbalance_index_{i+1} < 0). This can make calculations below fail. All
     * of this of course refers to the possibility of nobody touching checker
     * for over 20 years, which I guess should be practically impossible. *)
    fixedpoint_of_ratio_floor
      (mul_ratio
         (fixedpoint_to_ratio parameters.imbalance_index)
         (add_ratio
            one_ratio
            (div_ratio
               (mul_ratio imbalance_rate duration_in_seconds)
               (ratio_of_int seconds_in_a_year)
            )
         )
      ) in

  let outstanding_with_fees =
    kit_of_ratio_floor
      (div_ratio
         (mul_ratio
            (kit_to_ratio parameters.outstanding_kit)
            (fixedpoint_to_ratio current_burrow_fee_index)
         )
         (fixedpoint_to_ratio parameters.burrow_fee_index)
      ) in

  let accrual_to_uniswap = kit_sub outstanding_with_fees parameters.outstanding_kit in (* NOTE: can this be negative? *)

  let current_outstanding_kit =
    kit_of_ratio_floor
      (div_ratio
         (mul_ratio
            (kit_to_ratio outstanding_with_fees)
            (fixedpoint_to_ratio current_imbalance_index)
         )
         (fixedpoint_to_ratio parameters.imbalance_index)
      ) in

  let current_circulating_kit = kit_add parameters.circulating_kit accrual_to_uniswap in

  ( accrual_to_uniswap
  , {
    index = current_index;
    protected_index = current_protected_index;
    target = current_target;
    drift = current_drift;
    drift_derivative = current_drift_derivative;
    q = current_q;
    burrow_fee_index = current_burrow_fee_index;
    imbalance_index = current_imbalance_index;
    outstanding_kit = current_outstanding_kit;
    circulating_kit = current_circulating_kit;
    last_touched = !Ligo.Tezos.now;
  }
  )

(** Add some kit to the total amount of kit in circulation. *)
let[@inline] add_circulating_kit (parameters: parameters) (kit: kit) : parameters =
  { parameters with circulating_kit = kit_add parameters.circulating_kit kit; }

(** Remove some kit from the total amount of kit in circulation. *)
let[@inline] remove_circulating_kit (parameters: parameters) (kit: kit) : parameters =
  assert (parameters.circulating_kit >= kit);
  { parameters with circulating_kit = kit_sub parameters.circulating_kit kit; }

(** Add some kit to the total amount of kit required to close all burrows. *)
let[@inline] add_outstanding_kit (parameters: parameters) (kit: kit) : parameters =
  { parameters with outstanding_kit = kit_add parameters.outstanding_kit kit; }

(** Remove some kit from the total amount of kit required to close all burrows. *)
let[@inline] remove_outstanding_kit (parameters: parameters) (kit: kit) : parameters =
  assert (parameters.outstanding_kit >= kit);
  { parameters with outstanding_kit = kit_sub parameters.outstanding_kit kit; }
