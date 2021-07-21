open Common

(* BEGIN_OCAML *)
[@@@coverage off]

let[@inline] ratio_of_tez (x: Ligo.tez) : ratio = { num = tez_to_mutez x; den = Ligo.int_from_literal "1_000_000"; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let lt_ratio_ratio (x: ratio) (y: ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Ligo.lt_int_int
    (Ligo.mul_int_int x_num y_den)
    (Ligo.mul_int_int y_num x_den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let mul_ratio (x: ratio) (y: ratio) : ratio =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_ratio
    (Ligo.mul_int_int x_num y_num)
    (Ligo.mul_int_int x_den y_den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let leq_ratio_ratio (x: ratio) (y: ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Ligo.leq_int_int
    (Ligo.mul_int_int x_num y_den)
    (Ligo.mul_int_int y_num x_den)

let[@inline] geq_ratio_ratio (x: ratio) (y: ratio) : bool = leq_ratio_ratio y x

let[@inline] gt_ratio_ratio (x: ratio) (y: ratio) : bool = lt_ratio_ratio y x

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let[@inline] neg_ratio (x: ratio) : ratio =
  let { num = x_num; den = x_den; } = x in
  { num = neg_int x_num; den = x_den; }

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let add_ratio (x: ratio) (y: ratio) : ratio =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_ratio
    (Ligo.add_int_int
       (Ligo.mul_int_int x_num y_den)
       (Ligo.mul_int_int y_num x_den))
    (Ligo.mul_int_int x_den y_den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let sub_ratio (x: ratio) (y: ratio) : ratio =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_ratio
    (Ligo.sub_int_int
       (Ligo.mul_int_int x_num y_den)
       (Ligo.mul_int_int y_num x_den))
    (Ligo.mul_int_int x_den y_den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let div_ratio (x: ratio) (y: ratio) : ratio =
  let { num = y_num; den = y_den; } = y in
  if Ligo.eq_int_int y_num (Ligo.int_from_literal "0") then
    (failwith "Ratio.div_ratio: division by zero" : ratio)
  else if Ligo.gt_int_int y_num (Ligo.int_from_literal "0") then
    mul_ratio x { num = y_den; den = y_num; }
  else
    mul_ratio x { num = neg_int y_den; den = neg_int y_num; }

(* NOTE: this implementation relies on the fact that the rationals are normalized. *)
let sign_ratio x =
  if Ligo.lt_int_int x.num (Ligo.int_from_literal "0") then
    -1
  else if Ligo.eq_int_int x.num (Ligo.int_from_literal "0") then
    0
  else
    1

let[@inline] ratio_of_nat (n: Ligo.nat) : ratio = { num = Ligo.int n; den = Ligo.int_from_literal "1"; }

let min_ratio (a: ratio) (b: ratio) : ratio = if leq_ratio_ratio a b then a else b

let max_ratio (a: ratio) (b: ratio) : ratio = if geq_ratio_ratio a b then a else b

let[@inline] clamp_ratio (v: ratio) (lower: ratio) (upper: ratio) : ratio =
  assert (leq_ratio_ratio lower upper);
  min_ratio upper (max_ratio v lower)

let[@inline] ratio_of_int (i: Ligo.int) : ratio = { num = i; den = Ligo.int_from_literal "1"; }

(* If we wish to rely on the fact that the rationals are normalized, we could
 * instead implement equality very efficiently as
 *
 *   x.num = y.num && x.den = y.den
 *
 * but I'd like to avoid that. Ideally we'll drop all fancy normalization, and
 * have rationals be lightweight. We always return to fixed-point, tez, and
 * kit, at the end anyway. *)
let eq_ratio_ratio (x: ratio) (y: ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Ligo.eq_int_int
    (Ligo.mul_int_int x_num y_den)
    (Ligo.mul_int_int y_num x_den)

[@@@coverage on]
(* END_OCAML *)
