open Common
open FixedPoint

(** Calculate the current target based on the current quantity, the current
    index, and the current price of kit in tok.
    {[
      target_{i+1} = FLOOR (q_{i+1} * index_{i+1} / kit_in_tok_{i+1})
    ]}
*)
let[@inline] compute_current_target (current_q: fixedpoint) (current_index: fixedpoint) (current_kit_in_tok: ratio) : fixedpoint =
  let { num = num; den = den; } = current_kit_in_tok in
  fixedpoint_of_raw
    (fdiv_int_int
       (Ligo.mul_int_int
          den
          (Ligo.mul_int_int
             (fixedpoint_to_raw current_q)
             (fixedpoint_to_raw current_index)
          )
       )
       (Ligo.mul_int_int
          fixedpoint_scaling_factor
          num
       )
    )
