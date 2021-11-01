open Common
open FixedPoint

(** Given the current target, calculate the rate of change of the drift (drift
    derivative). That's how the following calculations came to be:

    d'(x) = clamp (((A/C^2) * sign(x) * x^2), -A, A)
          = A * clamp ((sign(x) * (x/C)^2), -1, 1)
          = A * sign(x) * min (1, x^2/C^2)                  (* move sign(x) out of clamp(..) *)
          = A * sign(log(p_t)) * min (1, (log(p_t))^2/C^2)  (* x = log(p_t) *)
          = A * sign(p_t - 1) * min (1, (p_t - 1)^2/C^2)    (* log(Y) ~= Y - 1 *)

    d'(x) = -A * min (1, (p_t - 1)^2/C^2)    , if p_t <= 1
    d'(x) = +A * min (1, (p_t - 1)^2/C^2)    , if p_t >  1

    For precision:

    d'(x) = max (-A, -A * (p_t - 1)^2/C^2)   , if 0 < p_t < 1
    d'(x) = 0                                , if p_t = 1
    d'(x) = min (+A, +A * (p_t - 1)^2/C^2)   , if 1 < p_t < +infinity

    A = 0.05 cNp/day^2 ~= 0.05/100 * (86400 * 86400) = 5/74649600000000 = 1235555 in fixedpoint
    C = 0.05

    d'(x) = max (-A, -A * (p_t - 1)^2/C^2)   , if 0 < p_t < 1
    d'(x) = 0                                , if p_t = 1
    d'(x) = min (+A, +A * (p_t - 1)^2/C^2)   , if 1 < p_t < +infinity
*)
let[@inline] compute_drift_derivative (target : fixedpoint) : fixedpoint =
  assert (target > fixedpoint_zero);
  let target = fixedpoint_to_raw target in

  let[@inline] amplitude : fixedpoint = fixedpoint_of_raw (Ligo.int_from_literal "1235555") in
  let amplitude = fixedpoint_to_raw amplitude in

  let[@inline] minus_amplitude : fixedpoint = fixedpoint_of_raw (Ligo.int_from_literal "-1235555") in
  let minus_amplitude = fixedpoint_to_raw minus_amplitude in

  let[@inline] cutoff : ratio = make_ratio (Ligo.int_from_literal "5") (Ligo.int_from_literal "100") in
  let { num = num_c; den = den_c; } = cutoff in

  (*
    d'(x) = max (-A, -A * ((p_t - sf)^2 * den_c^2) / (sf^2 * num_c^2))   , if 0 < p_t < 1
    d'(x) = 0                                                            , if p_t = 1
    d'(x) = min (+A, +A * ((p_t - sf)^2 * den_c^2) / (sf^2 * num_c^2))   , if 1 < p_t < +infinity
  *)
  let numerator =
    let sub_target_sf = Ligo.sub_int_int target fixedpoint_scaling_factor in
    let mul_sub_target_sf_den_c = Ligo.mul_int_int sub_target_sf den_c in
    Ligo.mul_int_int amplitude (Ligo.mul_int_int mul_sub_target_sf_den_c mul_sub_target_sf_den_c) in
  let denominator =
    let mul_num_c_sf = Ligo.mul_int_int num_c fixedpoint_scaling_factor in
    Ligo.mul_int_int mul_num_c_sf mul_num_c_sf in

  let actual = fdiv_int_int numerator denominator in

  if Ligo.lt_int_int target (fixedpoint_to_raw fixedpoint_one) then
    fixedpoint_of_raw (max_int minus_amplitude (neg_int actual))
  else if Ligo.eq_int_int target (fixedpoint_to_raw fixedpoint_one) then
    fixedpoint_zero
  else (* Ligo.gt_int_int target (fixedpoint_to_raw fixedpoint_one) *)
    fixedpoint_of_raw (min_int amplitude actual)
