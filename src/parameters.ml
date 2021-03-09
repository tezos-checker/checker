open Kit
open Ratio
open FixedPoint
open Common
open Constants

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
let[@inline] tz_minting (p: parameters) : Ligo.tez = max_tez p.index p.protected_index

(* tez. To get tez/kit must multiply with q. *)
let[@inline] tz_liquidation (p: parameters) : Ligo.tez = min_tez p.index p.protected_index

(** Current minting price (tez/kit). *)
let minting_price (p: parameters) : ratio =
  make_real_unsafe
    (Ligo.mul_int_int (fixedpoint_to_raw p.q) (tez_to_mutez (tz_minting p)))
    (Ligo.mul_int_int (fixedpoint_to_raw fixedpoint_one) (Ligo.int_from_literal "1_000_000"))

(** Current liquidation price (tez/kit). *)
let liquidation_price (p: parameters) : ratio =
  make_real_unsafe
    (Ligo.mul_int_int (fixedpoint_to_raw p.q) (tez_to_mutez (tz_liquidation p)))
    (Ligo.mul_int_int (fixedpoint_to_raw fixedpoint_one) (Ligo.int_from_literal "1_000_000"))

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
let[@inline] compute_imbalance (burrowed: kit) (circulating: kit) : ratio =
  let burrowed = Ligo.int (kit_to_mukit burrowed) in
  let circulating = Ligo.int (kit_to_mukit circulating) in
  if burrowed = Ligo.int_from_literal "0" && circulating = Ligo.int_from_literal "0" then
    zero_ratio
  else if burrowed = Ligo.int_from_literal "0" && circulating <> Ligo.int_from_literal "0" then
    make_real_unsafe (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100")
  else if Ligo.geq_int_int burrowed circulating then
    make_real_unsafe
      (min_int (Ligo.mul_int_int (Ligo.int_from_literal "5") (Ligo.sub_int_int burrowed circulating)) burrowed)
      (Ligo.mul_int_int (Ligo.int_from_literal "20") burrowed)
  else (* burrowed < circulating *)
    make_real_unsafe
      (neg_int (min_int (Ligo.mul_int_int (Ligo.int_from_literal "5") (Ligo.sub_int_int circulating burrowed)) burrowed))
      (Ligo.mul_int_int (Ligo.int_from_literal "20") burrowed)

(** Compute the current adjustment index. Basically this is the product of
  * the burrow fee index and the imbalance adjustment index.
  *
  *   adjustment_index_i = FLOOR (burrow_fee_index_i * imabalance_index_i)
  *)
let compute_adjustment_index (p: parameters) : fixedpoint =
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.mul_int_int
          (fixedpoint_to_raw p.burrow_fee_index)
          (fixedpoint_to_raw p.imbalance_index)
       )
       (fixedpoint_to_raw fixedpoint_one)
    )

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
  let cnp_001 = fixedpoint_of_ratio_floor (make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "10000")) in
  let cnp_005 = fixedpoint_of_ratio_floor (make_real_unsafe (Ligo.int_from_literal "5") (Ligo.int_from_literal "10000")) in
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

(** Calculate the current burrow fee index based on the last index and the
  * number of seconds that have elapsed.
  *
  * burrow_fee_index_{i+1} = FLOOR (burrow_fee_index_i * (1 + burrow_fee_percentage * (t_{i+1} - t_i)) / <seconds_in_a_year>)
  *
  * Keep in mind that this formula means that the burrow fee index is
  * ever-increasing. *)
let[@inline] compute_current_burrow_fee_index (last_burrow_fee_index: fixedpoint) (duration_in_seconds: Ligo.int) : fixedpoint =
  let { num = num; den = den; } = burrow_fee_percentage in
  let denom = Ligo.mul_int_int den seconds_in_a_year in
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.mul_int_int
          (fixedpoint_to_raw last_burrow_fee_index)
          (Ligo.add_int_int
             denom
             (Ligo.mul_int_int num duration_in_seconds)
          )
       )
       denom
    )

(** Calculate the current protected index based on the last protected index,
  * the current index (as provided by the oracle), and the number of seconds
  * that have elapsed.
  *
  *   protected_index_{i+1} = FLOOR (
  *     protected_index_i * CLAMP (index_{i+1}/protected_index_i, EXP(-epsilon * (t_{i+1} - t_i)), EXP(+epsilon * (t_{i+1} - t_i)))
  *   )
  *)
let[@inline] compute_current_protected_index (last_protected_index: Ligo.tez) (current_index: Ligo.tez) (duration_in_seconds: Ligo.int) : Ligo.tez =
  assert (Ligo.gt_tez_tez last_protected_index (Ligo.tez_from_literal "0mutez"));
  (* TODO: ADD MORE ASSERTIONS: STRICTLY POSITIVE LAST PROTECTED INDEX *)
  let last_protected_index = tez_to_mutez last_protected_index in
  ratio_to_tez_floor
    (make_real_unsafe
       (clamp_int
          (Ligo.mul_int_int
             (tez_to_mutez current_index)
             protected_index_inverse_epsilon
          )
          (Ligo.mul_int_int
             last_protected_index
             (Ligo.sub_int_int
                protected_index_inverse_epsilon
                duration_in_seconds
             )
          )
          (Ligo.mul_int_int
             last_protected_index
             (Ligo.add_int_int
                protected_index_inverse_epsilon
                duration_in_seconds
             )
          )
       )
       (Ligo.mul_int_int
          protected_index_inverse_epsilon
          (Ligo.int_from_literal "1_000_000")
       )
    )

(** Calculate the current drift based on the last drift, the last drift
  * derivative, the current drift derivative, and the number of seconds that
  * have elapsed.
  *
  *   drift_{i+1} = FLOOR (drift_i + (1/2) * (drift'_i + drift'_{i+1}) * (t_{i+1} - t_i))
  *)
let[@inline] compute_current_drift (last_drift: fixedpoint) (last_drift_derivative: fixedpoint) (current_drift_derivative: fixedpoint) (duration_in_seconds: Ligo.int) : fixedpoint =
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.add_int_int
          (Ligo.mul_int_int (Ligo.int_from_literal "2") (fixedpoint_to_raw last_drift))
          (Ligo.mul_int_int
             (fixedpoint_to_raw (fixedpoint_add last_drift_derivative current_drift_derivative))
             duration_in_seconds
          )
       )
       (Ligo.int_from_literal "2")
    )

(** Calculate the current quantity based on the last quantity, the last drift,
  * the last drift derivative, the current drift derivative, and the number of
  * seconds that have elapsed.
  *
  *   q_{i+1} = FLOOR (q_i * EXP((drift_i + (1/6) * (2*drift'_i + drift'_{i+1}) * (t_{i+1} - t_i)) * (t_{i+1} - t_i)))
  *
  * where EXP(X) = X+1.
  *)
let[@inline] compute_current_q (last_q: fixedpoint) (last_drift: fixedpoint) (last_drift_derivative: fixedpoint) (current_drift_derivative: fixedpoint) (duration_in_seconds: Ligo.int) : fixedpoint =
  let six_sf =
    Ligo.mul_int_int
      (Ligo.int_from_literal "6")
      (fixedpoint_to_raw fixedpoint_one) in
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.mul_int_int
          (fixedpoint_to_raw last_q)
          (Ligo.add_int_int
             (Ligo.mul_int_int
                (Ligo.add_int_int
                   (Ligo.mul_int_int
                      (Ligo.int_from_literal "6")
                      (fixedpoint_to_raw last_drift)
                   )
                   (Ligo.mul_int_int
                      (Ligo.add_int_int
                         (Ligo.mul_int_int
                            (Ligo.int_from_literal "2")
                            (fixedpoint_to_raw last_drift_derivative)
                         )
                         (fixedpoint_to_raw current_drift_derivative)
                      )
                      duration_in_seconds
                   )
                )
                duration_in_seconds
             )
             six_sf
          )
       )
       six_sf
    )

(** Calculate the current target based on the current quantity, the current
  * index, and the current price of kit in tez (as provided by the uniswap
  * sub-contract, from the previous block).
  *
  * target_{i+1} = FLOOR (q_{i+1} * index_{i+1} / kit_in_tez_{i+1})
  *)
let[@inline] compute_current_target (current_q: fixedpoint) (current_index: Ligo.tez) (current_kit_in_tez: ratio) : fixedpoint =
  let { num = num; den = den; } = current_kit_in_tez in
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.mul_int_int
          den
          (Ligo.mul_int_int
             (fixedpoint_to_raw current_q)
             (tez_to_mutez current_index)
          )
       )
       (Ligo.mul_int_int
          (Ligo.int_from_literal "1_000_000")
          num
       )
    )

(** Calculate the current imbalance index based on the last amount of
  * outstanding (burrowed) kit, the last amount of circulating kit, the last
  * imbalance index, and the number of seconds that have elapsed, using the
  * following formula:
  *
  *   imbalance_index_{i+1} = FLOOR (
  *     imbalance_index_i * (1 + imbalance * (t_{i+1} - t_i) / <seconds_in_a_year>)
  *   )
  *
  * (note that the last outstanding kit and circulating kit are used in the
  * calculation of imbalance; see compute_imbalance).
  *
  * This calculation means that even if the imbalance_rate is bounded (from -5
  * cNp to +5 cNp), the imbalance index is not. The above formula for
  * imbalance = -5cNp and 20 years time in seconds elapsed gives
  * imbalance_index_{i+1} = 0 (for longer time it gives
  * imbalance_index_{i+1} < 0). This can make calculations below fail. All of
  * this of course refers to the possibility of nobody touching checker for
  * over 20 years, which I guess should be practically impossible. *)
let[@inline] compute_current_imbalance_index (last_outstanding_kit: kit) (last_circulating_kit: kit) (last_imbalance_index: fixedpoint) (duration_in_seconds: Ligo.int) : fixedpoint =
  let { num = num; den = den; } =
    compute_imbalance
      last_outstanding_kit (* burrowed *)
      last_circulating_kit (* circulating *) in
  let denom = Ligo.mul_int_int den seconds_in_a_year in
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.mul_int_int
          (fixedpoint_to_raw last_imbalance_index)
          (Ligo.add_int_int
             denom
             (Ligo.mul_int_int num duration_in_seconds)
          )
       )
       denom
    )

(** Compute current outstanding kit, taking burrow fees into account:
  *
  *   outstanding_with_fees_{i+1} = FLOOR (
  *     outstanding_kit_i * burrow_fee_index_{i+1} / burrow_fee_index_i
  *   )
  *)
let[@inline] compute_current_outstanding_with_fees (last_outstanding_kit: kit) (last_burrow_fee_index: fixedpoint) (current_burrow_fee_index: fixedpoint) : kit =
  kit_of_ratio_floor
    (make_real_unsafe
       (Ligo.mul_int_int (Ligo.int (kit_to_mukit last_outstanding_kit)) (fixedpoint_to_raw current_burrow_fee_index))
       (Ligo.mul_int_int kit_scaling_factor_int (fixedpoint_to_raw last_burrow_fee_index))
    )

(** Compute current outstanding kit, given that the burrow fees have already
  * been added (that is, compute the effect of the imbalance index):
  *
  *   outstanding_kit_{i+1} = FLOOR (
  *     outstanding_with_fees_{i+1} * imbalance_index_{i+1} / imbalance_index_i
  *   )
  *)
let[@inline] compute_current_outstanding_kit (current_outstanding_with_fees: kit) (last_imbalance_index: fixedpoint) (current_imbalance_index: fixedpoint) : kit =
  kit_of_ratio_floor
    (make_real_unsafe
       (Ligo.mul_int_int (Ligo.int (kit_to_mukit current_outstanding_with_fees)) (fixedpoint_to_raw current_imbalance_index))
       (Ligo.mul_int_int kit_scaling_factor_int (fixedpoint_to_raw last_imbalance_index))
    )

(** Update the checker's parameters, given (a) the current timestamp
  * (Tezos.now), (b) the current index (the median of the oracles right now),
  * and (c) the current price of kit in tez, as given by the uniswap
  * sub-contract. *)
let parameters_touch
    (current_index: Ligo.tez)
    (current_kit_in_tez: ratio)
    (parameters: parameters)
  : kit * parameters =
  let
    { q = parameters_q;
      index = _parameters_index; (* NOTE: unused. Always set to the new one. *)
      protected_index = parameters_protected_index;
      target = parameters_target;
      drift_derivative = parameters_drift_derivative;
      drift = parameters_drift;
      burrow_fee_index = parameters_burrow_fee_index;
      imbalance_index = parameters_imbalance_index;
      outstanding_kit = parameters_outstanding_kit;
      circulating_kit = parameters_circulating_kit;
      last_touched = parameters_last_touched;
    } = parameters in

  (* Calculate the number of seconds elapsed. *)
  let duration_in_seconds = Ligo.sub_timestamp_timestamp !Ligo.Tezos.now parameters_last_touched in
  assert (Ligo.geq_int_int duration_in_seconds (Ligo.int_from_literal "0")); (* NOTE: the protocol should ensure this I believe. *)

  (* Update the indices *)
  let current_burrow_fee_index =
    compute_current_burrow_fee_index parameters_burrow_fee_index duration_in_seconds in
  let current_imbalance_index =
    compute_current_imbalance_index parameters_outstanding_kit parameters_circulating_kit parameters_imbalance_index duration_in_seconds in

  (* Calculate all parameter updates and accrual to uniswap. *)
  let current_protected_index =
    compute_current_protected_index parameters_protected_index current_index duration_in_seconds in
  let current_drift_derivative =
    compute_drift_derivative parameters_target in
  let current_drift =
    compute_current_drift parameters_drift parameters_drift_derivative current_drift_derivative duration_in_seconds in
  let current_q =
    compute_current_q parameters_q parameters_drift parameters_drift_derivative current_drift_derivative duration_in_seconds in
  let current_target =
    compute_current_target current_q current_index current_kit_in_tez in
  let current_outstanding_with_fees =
    compute_current_outstanding_with_fees parameters_outstanding_kit parameters_burrow_fee_index current_burrow_fee_index in
  let accrual_to_uniswap =
    kit_sub current_outstanding_with_fees parameters_outstanding_kit in (* NOTE: can this be negative? *)
  let current_outstanding_kit =
    compute_current_outstanding_kit current_outstanding_with_fees parameters_imbalance_index current_imbalance_index in
  let current_circulating_kit =
    kit_add parameters_circulating_kit accrual_to_uniswap in

  (* Update all values *)
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

(* BEGIN_OCAML *)
(* NOTE: These are "model" implementations of the functions above, using
 * ratios. The only reason I left them here is so that we can check that each
 * function implementation above (which does not use the ratio and fixedpoint
 * abstractions), gives identical results to the corresponding model
 * implementation below. If we are not gonna do that, we can drop the model
 * functions. *)
let model_minting_price (p: parameters) : ratio =
  mul_ratio (fixedpoint_to_ratio p.q) (ratio_of_tez (tz_minting p))

let model_liquidation_price (p: parameters) : ratio =
  mul_ratio (fixedpoint_to_ratio p.q) (ratio_of_tez (tz_liquidation p))

let model_compute_imbalance (burrowed: kit) (circulating: kit) : ratio =
  if burrowed = kit_zero && circulating = kit_zero then
    zero_ratio
  else if burrowed = kit_zero && circulating <> kit_zero then
    make_real_unsafe (Ligo.int_from_literal "-5") (Ligo.int_from_literal "100")
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

let model_compute_adjustment_index (p: parameters) : fixedpoint =
  let burrow_fee_index = fixedpoint_to_ratio p.burrow_fee_index in
  let imbalance_index = fixedpoint_to_ratio p.imbalance_index in
  fixedpoint_of_ratio_floor (mul_ratio burrow_fee_index imbalance_index)

let model_compute_current_burrow_fee_index
    (last_burrow_fee_index: fixedpoint)
    (duration_in_seconds: Ligo.int)
  : fixedpoint =
  let duration_in_seconds = ratio_of_int duration_in_seconds in
  fixedpoint_of_ratio_floor
    (mul_ratio
       (fixedpoint_to_ratio last_burrow_fee_index)
       (add_ratio
          one_ratio
          (div_ratio
             (mul_ratio burrow_fee_percentage duration_in_seconds)
             (ratio_of_int seconds_in_a_year)
          )
       )
    )

let model_compute_current_protected_index
    (last_protected_index: Ligo.tez)
    (current_index: Ligo.tez)
    (duration_in_seconds: Ligo.int)
  : Ligo.tez =
  let duration_in_seconds = ratio_of_int duration_in_seconds in
  let protected_index_epsilon = make_ratio (Ligo.int_from_literal "1") protected_index_inverse_epsilon in
  let upper_lim = qexp (mul_ratio           (protected_index_epsilon) duration_in_seconds) in
  let lower_lim = qexp (mul_ratio (neg_ratio protected_index_epsilon) duration_in_seconds) in
  let ratio = make_ratio (tez_to_mutez current_index) (tez_to_mutez last_protected_index) in
  ratio_to_tez_floor
    (mul_ratio
       (ratio_of_tez last_protected_index)
       (clamp_ratio ratio lower_lim upper_lim)
    )

let model_compute_current_drift
    (last_drift: fixedpoint)
    (last_drift_derivative: fixedpoint)
    (current_drift_derivative: fixedpoint)
    (duration_in_seconds: Ligo.int)
  : fixedpoint =
  let duration_in_seconds = ratio_of_int duration_in_seconds in
  fixedpoint_of_ratio_floor
    (add_ratio
       (fixedpoint_to_ratio last_drift)
       (mul_ratio
          (make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "2"))
          (mul_ratio
             (fixedpoint_to_ratio (fixedpoint_add last_drift_derivative current_drift_derivative))
             duration_in_seconds
          )
       )
    )

let model_compute_current_q
    (last_q: fixedpoint)
    (last_drift: fixedpoint)
    (last_drift_derivative: fixedpoint)
    (current_drift_derivative: fixedpoint)
    (duration_in_seconds: Ligo.int)
  : fixedpoint =
  let duration_in_seconds = ratio_of_int duration_in_seconds in
  fixedpoint_of_ratio_floor
    (mul_ratio
       (fixedpoint_to_ratio last_q)
       (qexp
          (mul_ratio
             (add_ratio
                (fixedpoint_to_ratio last_drift)
                (mul_ratio
                   (make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "6"))
                   (mul_ratio
                      (add_ratio
                         (mul_ratio
                            (ratio_of_int (Ligo.int_from_literal "2"))
                            (fixedpoint_to_ratio last_drift_derivative)
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
    )

let model_compute_current_target
    (current_q: fixedpoint)
    (current_index: Ligo.tez)
    (current_kit_in_tez: ratio)
  : fixedpoint =
  fixedpoint_of_ratio_floor
    (div_ratio
       (mul_ratio
          (fixedpoint_to_ratio current_q)
          (ratio_of_tez current_index)
       )
       current_kit_in_tez
    )

let model_compute_current_imbalance_index
    (last_outstanding_kit: kit)
    (last_circulating_kit: kit)
    (last_imbalance_index: fixedpoint)
    (duration_in_seconds: Ligo.int)
  : fixedpoint =
  let duration_in_seconds = ratio_of_int duration_in_seconds in
  let imbalance_rate =
    compute_imbalance
      last_outstanding_kit (* burrowed *)
      last_circulating_kit (* circulating *) in
  fixedpoint_of_ratio_floor
    (mul_ratio
       (fixedpoint_to_ratio last_imbalance_index)
       (add_ratio
          one_ratio
          (div_ratio
             (mul_ratio imbalance_rate duration_in_seconds)
             (ratio_of_int seconds_in_a_year)
          )
       )
    )

let model_compute_current_outstanding_with_fees
    (last_outstanding_kit: kit)
    (last_burrow_fee_index: fixedpoint)
    (current_burrow_fee_index: fixedpoint)
  : kit =
  kit_of_ratio_floor
    (div_ratio
       (mul_ratio
          (kit_to_ratio last_outstanding_kit)
          (fixedpoint_to_ratio current_burrow_fee_index)
       )
       (fixedpoint_to_ratio last_burrow_fee_index)
    )

let model_compute_current_outstanding_kit
    (current_outstanding_with_fees: kit)
    (last_imbalance_index: fixedpoint)
    (current_imbalance_index: fixedpoint)
  : kit =
  kit_of_ratio_floor
    (div_ratio
       (mul_ratio
          (kit_to_ratio current_outstanding_with_fees)
          (fixedpoint_to_ratio current_imbalance_index)
       )
       (fixedpoint_to_ratio last_imbalance_index)
    )
(* END_OCAML *)
