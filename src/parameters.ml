open Kit
open Ratio
open FixedPoint
open Common
open Constants

type parameters =
  { q : fixedpoint; (* 1/kit, really *)
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

(** Initial state of the parameters. *)
let initial_parameters : parameters =
  { q = fixedpoint_one;
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
  }

(** Compute the current minting index (in tez). To get tez/kit must multiply with q. *)
let[@inline] tz_minting (p: parameters) : Ligo.tez = max_tez p.index p.protected_index

(** Compute the current liquidation index (in tez). To get tez/kit must multiply with q. *)
let[@inline] tz_liquidation (p: parameters) : Ligo.tez = min_tez p.index p.protected_index

(** Current minting price (in tez/kit). *)
let minting_price (p: parameters) : ratio =
  make_real_unsafe
    (Ligo.mul_int_int (fixedpoint_to_raw p.q) (tez_to_mutez (tz_minting p)))
    (Ligo.mul_int_int (fixedpoint_to_raw fixedpoint_one) (Ligo.int_from_literal "1_000_000"))

(** Current liquidation price (in tez/kit). *)
let liquidation_price (p: parameters) : ratio =
  make_real_unsafe
    (Ligo.mul_int_int (fixedpoint_to_raw p.q) (tez_to_mutez (tz_liquidation p)))
    (Ligo.mul_int_int (fixedpoint_to_raw fixedpoint_one) (Ligo.int_from_literal "1_000_000"))

(** Given the amount of kit necessary to close all existing burrows
    (outstanding) and the amount of kit that is currently in circulation
    (circulating), compute the current imbalance adjustment using the following
    formula:
    {[
      clamp
        ( imbalance_scaling_factor * (circulating - outstanding) / circulating,
          -imbalance_limit,
          +imbalance_limit
        )
    ]}

    or, equivalently,
    {[
      min (imbalance_scaling_factor * (circulating - outstanding) / circulating, +imbalance_limit), if circulating >= outstanding
      max (imbalance_scaling_factor * (circulating - outstanding) / circulating, -imbalance_limit), if circulating < outstanding
    ]}

    Edge cases:
    - [circulating = 0] and [outstanding = 0].
        The imbalance fee/bonus is 0.
    - [circulating = 0] and [outstanding > 0].
        Well, outstanding is "infinitely" greater than circulating so let's
        saturate the imbalance to -imbalance_limit.
*)
let[@inline] compute_imbalance (outstanding: kit) (circulating: kit) : ratio =
  let outstanding = kit_to_mukit_int outstanding in
  let circulating = kit_to_mukit_int circulating in
  let { num = num_il; den = den_il; } = imbalance_limit in

  if circulating = Ligo.int_from_literal "0" && outstanding = Ligo.int_from_literal "0" then
    zero_ratio
  else if circulating = Ligo.int_from_literal "0" && outstanding <> Ligo.int_from_literal "0" then
    make_real_unsafe (neg_int num_il) den_il
  else
    let { num = num_isf; den = den_isf; } = imbalance_scaling_factor in
    let denominator = Ligo.mul_int_int den_isf circulating in

    if Ligo.geq_int_int circulating outstanding then
      make_real_unsafe
        (min_int (Ligo.mul_int_int (Ligo.mul_int_int num_isf (Ligo.sub_int_int circulating outstanding)) den_il) (Ligo.mul_int_int num_il denominator))
        (Ligo.mul_int_int den_il denominator)
    else (* circulating < outstanding *)
      make_real_unsafe
        (neg_int (min_int (Ligo.mul_int_int (Ligo.mul_int_int num_isf (Ligo.sub_int_int outstanding circulating)) den_il) (Ligo.mul_int_int num_il denominator)))
        (Ligo.mul_int_int den_il denominator)

(** Compute the current adjustment index. Basically this is the product of
    the burrow fee index and the imbalance adjustment index.
    {[
       adjustment_index_i = FLOOR (burrow_fee_index_i * imabalance_index_i)
    ]}
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
    derivative). That's how the following calculations came to be:

    let [X = log (p_t)] be the "measure of imbalance". The original doc gave:
    {[
      d_t' = 0                             if 0       <= |X| < 0.5 cNp
      d_t' = sign(X) * 0.01 cNp / day^2    if 0.5 cNp <= |X| <   5 cNp
      d_t' = sign(X) * 0.05 cNp / day^2    if   5 cNp <= |X| < infinity
    ]}

    1. Inline the numbers: cNp ~= 1/100, day ~= 24 * 60 * 60 = 86400 seconds
    {[
      d_t' = 0                             if 0     <= |X| < 0.005
      d_t' = sign(X) * 0.0001 / 86400^2    if 0.005 <= |X| < 0.05
      d_t' = sign(X) * 0.0005 / 86400^2    if 0.05  <= |X| < infinity
    ]}

    2. Remove absolute values
    {[
      d_t' =  0                   if -0.005 <  X <  0.005
      d_t' = +0.0001 / 86400^2    if +0.005 <= X < +0.05
      d_t' = -0.0001 / 86400^2    if -0.005 >= X > -0.05
      d_t' = +0.0005 / 86400^2    if +0.05  <= X < +infinity
      d_t' = -0.0005 / 86400^2    if -0.05  >= X > -infinity
    ]}

    3. Exponentiate the inequalities
    {[
      d_t' =  0                   if exp(-0.005) <  p_t < exp(+0.005)
      d_t' = +0.0001 / 86400^2    if exp(+0.005) <= p_t < exp(+0.05)
      d_t' = -0.0001 / 86400^2    if exp(-0.005) >= p_t > exp(-0.05)
      d_t' = +0.0005 / 86400^2    if exp(+0.05)  <= p_t < +infinity
      d_t' = -0.0005 / 86400^2    if exp(-0.05)  >= p_t > -infinity
    ]}
*)
let compute_drift_derivative (target : fixedpoint) : fixedpoint =
  assert (target > fixedpoint_zero);

  let { num = num_tlb; den = den_tlb; } = target_low_bracket in
  let { num = num_thb; den = den_thb; } = target_high_bracket in
  let target = fixedpoint_to_raw target in

  let mul_target_tlb = Ligo.mul_int_int target den_tlb in
  let mul_sub_den_tlb_num_tlb_sf = Ligo.mul_int_int (Ligo.sub_int_int den_tlb num_tlb) fixedpoint_scaling_factor in
  let mul_add_den_tlb_num_tlb_sf = Ligo.mul_int_int (Ligo.add_int_int den_tlb num_tlb) fixedpoint_scaling_factor in

  let mul_target_thb = Ligo.mul_int_int target den_thb in
  let mul_sub_den_thb_num_thb_sf = Ligo.mul_int_int (Ligo.sub_int_int den_thb num_thb) fixedpoint_scaling_factor in
  let mul_add_den_thb_num_thb_sf = Ligo.mul_int_int (Ligo.add_int_int den_thb num_thb) fixedpoint_scaling_factor in

  if Ligo.lt_int_int mul_sub_den_tlb_num_tlb_sf mul_target_tlb && Ligo.lt_int_int mul_target_tlb mul_add_den_tlb_num_tlb_sf then
    fixedpoint_zero (* no acceleration (0) *)
  else if Ligo.lt_int_int mul_sub_den_thb_num_thb_sf mul_target_thb && Ligo.leq_int_int mul_target_tlb mul_sub_den_tlb_num_tlb_sf then
    low_negative_acceleration
  else if Ligo.gt_int_int mul_add_den_thb_num_thb_sf mul_target_thb && Ligo.geq_int_int mul_target_tlb mul_add_den_tlb_num_tlb_sf then
    low_positive_acceleration
  else if Ligo.leq_int_int mul_target_thb mul_sub_den_thb_num_thb_sf then
    high_negative_acceleration
  else if Ligo.geq_int_int mul_target_thb mul_add_den_thb_num_thb_sf then
    high_positive_acceleration
  else
    (failwith "impossible" : fixedpoint)

(** Calculate the current burrow fee index based on the last index and the
    number of seconds that have elapsed.
    {[
      burrow_fee_index_{i+1} = FLOOR (burrow_fee_index_i * (1 + burrow_fee_percentage * ((t_{i+1} - t_i) / <seconds_in_a_year>)))
    ]}
    Keep in mind that this formula means that the burrow fee index is
    ever-increasing. *)
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
    the current index (as provided by the oracle), and the number of seconds
    that have elapsed.
    {[
      protected_index_{i+1} = FLOOR (
        protected_index_i * CLAMP (index_{i+1}/protected_index_i, EXP(-epsilon * (t_{i+1} - t_i)), EXP(+epsilon * (t_{i+1} - t_i)))
      )
    ]}
*)
let[@inline] compute_current_protected_index (last_protected_index: Ligo.tez) (current_index: Ligo.tez) (duration_in_seconds: Ligo.int) : Ligo.tez =
  assert (Ligo.gt_tez_tez last_protected_index (Ligo.tez_from_literal "0mutez"));
  let last_protected_index = tez_to_mutez last_protected_index in
  fraction_to_tez_floor
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

(** Calculate the current drift based on the last drift, the last drift
    derivative, the current drift derivative, and the number of seconds that
    have elapsed.
    {[
      drift_{i+1} = FLOOR (drift_i + (1/2) * (drift'_i + drift'_{i+1}) * (t_{i+1} - t_i))
    ]}
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
    the last drift derivative, the current drift derivative, and the number of
    seconds that have elapsed.
    {[
      q_{i+1} = FLOOR (q_i * EXP((drift_i + (1/6) * (2*drift'_i + drift'_{i+1}) * (t_{i+1} - t_i)) * (t_{i+1} - t_i)))
    ]}
    where [EXP(X) = X+1].
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
    index, and the current price of kit in tez (as provided by the cfmm
    sub-contract, from the previous block).
    {[
      target_{i+1} = FLOOR (q_{i+1} * index_{i+1} / kit_in_tez_{i+1})
    ]}
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
    outstanding kit, the last amount of circulating kit, the last imbalance
    index, and the number of seconds that have elapsed, using the following
    formula:
    {[
      imbalance_index_{i+1} = FLOOR (
        imbalance_index_i * (1 + imbalance * (t_{i+1} - t_i) / <seconds_in_a_year>)
      )
    ]}
    (note that the last outstanding kit and circulating kit are used in the
    calculation of imbalance; see {!compute_imbalance}).

    This calculation means that even if the imbalance_rate is bounded (from -5
    cNp to +5 cNp), the imbalance index is not. The above formula for
    [imbalance = -5cNp] and 20 years time in seconds elapsed gives
    [imbalance_index_{i+1} = 0] (for longer time it gives
    [imbalance_index_{i+1} < 0]). This can make calculations below fail. All of
    this of course refers to the possibility of nobody touching checker for
    over 20 years, which I guess should be practically impossible. *)
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
    {[
      outstanding_with_fees_{i+1} = FLOOR (
        outstanding_kit_i * burrow_fee_index_{i+1} / burrow_fee_index_i
      )
    ]}
*)
let[@inline] compute_current_outstanding_with_fees (last_outstanding_kit: kit) (last_burrow_fee_index: fixedpoint) (current_burrow_fee_index: fixedpoint) : kit =
  kit_of_fraction_floor
    (Ligo.mul_int_int (kit_to_mukit_int last_outstanding_kit) (fixedpoint_to_raw current_burrow_fee_index))
    (Ligo.mul_int_int kit_scaling_factor_int (fixedpoint_to_raw last_burrow_fee_index))

(** Compute current outstanding kit, given that the burrow fees have already
    been added (that is, compute the effect of the imbalance index):
    {[
      outstanding_kit_{i+1} = FLOOR (
        outstanding_with_fees_{i+1} * imbalance_index_{i+1} / imbalance_index_i
      )
    ]}
*)
let[@inline] compute_current_outstanding_kit (current_outstanding_with_fees: kit) (last_imbalance_index: fixedpoint) (current_imbalance_index: fixedpoint) : kit =
  kit_of_fraction_floor
    (Ligo.mul_int_int (kit_to_mukit_int current_outstanding_with_fees) (fixedpoint_to_raw current_imbalance_index))
    (Ligo.mul_int_int kit_scaling_factor_int (fixedpoint_to_raw last_imbalance_index))

(** Update the checker's parameters, given (a) the current timestamp
    (Tezos.now), (b) the current index (the median of the oracles right now),
    and (c) the current price of kit in tez, as given by the cfmm
    sub-contract. *)
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

  (* Calculate all parameter updates and accrual to cfmm. *)
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
  let accrual_to_cfmm =
    kit_sub current_outstanding_with_fees parameters_outstanding_kit in (* NOTE: can this be negative? *)
  let current_outstanding_kit =
    compute_current_outstanding_kit current_outstanding_with_fees parameters_imbalance_index current_imbalance_index in
  let current_circulating_kit =
    kit_add parameters_circulating_kit accrual_to_cfmm in

  (* Update all values *)
  ( accrual_to_cfmm
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

(** Add some kit to the total amount of kit required to close all burrows and
    the kit in circulation. This is the case when a burrow owner mints kit. *)
let[@inline] add_outstanding_and_circulating_kit (parameters: parameters) (kit: kit) : parameters =
  { parameters with
    outstanding_kit = kit_add parameters.outstanding_kit kit;
    circulating_kit = kit_add parameters.circulating_kit kit;
  }

(** Remove some kit from the total amount of kit required to close all burrows
    and the kit in circulation. This is the case when a burrow owner burns kit. *)
let[@inline] remove_outstanding_and_circulating_kit (parameters: parameters) (kit: kit) : parameters =
  assert (parameters.outstanding_kit >= kit);
  { parameters with
    outstanding_kit = kit_sub parameters.outstanding_kit kit;
    circulating_kit = kit_sub parameters.circulating_kit kit;
  }
