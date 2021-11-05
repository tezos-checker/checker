open Common

(* Injections from other types *)

let[@inline] ratio_of_nat (n: Ligo.nat) : ratio = { num = Ligo.int n; den = Ligo.int_from_literal "1"; }
let[@inline] ratio_of_int (i: Ligo.int) : ratio = { num = i; den = Ligo.int_from_literal "1"; }
let[@inline] ratio_of_tez (x: Ligo.tez) : ratio = { num = tez_to_mutez x; den = Ligo.int_from_literal "1_000_000"; }

let[@inline] ratio_of_ctok (x: Ctok.ctok) : ratio = { num = Ctok.ctok_to_denomination_int x; den = Ctok.ctok_scaling_factor_int; }

(* Relational operators *)

let eq_ratio_ratio (x: ratio) (y: ratio) : bool =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Ligo.eq_int_int
    (Ligo.mul_int_int x_num y_den)
    (Ligo.mul_int_int y_num x_den)

let lt_ratio_ratio (x: ratio) (y: ratio) : bool =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  assert (Ligo.gt_int_int y.den (Ligo.int_from_literal "0"));
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Ligo.lt_int_int
    (Ligo.mul_int_int x_num y_den)
    (Ligo.mul_int_int y_num x_den)

let leq_ratio_ratio (x: ratio) (y: ratio) : bool =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  assert (Ligo.gt_int_int y.den (Ligo.int_from_literal "0"));
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  Ligo.leq_int_int
    (Ligo.mul_int_int x_num y_den)
    (Ligo.mul_int_int y_num x_den)

let[@inline] geq_ratio_ratio (x: ratio) (y: ratio) : bool = leq_ratio_ratio y x

let[@inline] gt_ratio_ratio (x: ratio) (y: ratio) : bool = lt_ratio_ratio y x

(* Binary arithmetic operations *)

let add_ratio (x: ratio) (y: ratio) : ratio =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  assert (Ligo.gt_int_int y.den (Ligo.int_from_literal "0"));
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_ratio
    (Ligo.add_int_int
       (Ligo.mul_int_int x_num y_den)
       (Ligo.mul_int_int y_num x_den))
    (Ligo.mul_int_int x_den y_den)

let sub_ratio (x: ratio) (y: ratio) : ratio =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  assert (Ligo.gt_int_int y.den (Ligo.int_from_literal "0"));
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_ratio
    (Ligo.sub_int_int
       (Ligo.mul_int_int x_num y_den)
       (Ligo.mul_int_int y_num x_den))
    (Ligo.mul_int_int x_den y_den)

let mul_ratio (x: ratio) (y: ratio) : ratio =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  assert (Ligo.gt_int_int y.den (Ligo.int_from_literal "0"));
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_ratio
    (Ligo.mul_int_int x_num y_num)
    (Ligo.mul_int_int x_den y_den)

let div_ratio (x: ratio) (y: ratio) : ratio =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  assert (Ligo.gt_int_int y.den (Ligo.int_from_literal "0"));
  let { num = y_num; den = y_den; } = y in
  if Ligo.eq_int_int y_num (Ligo.int_from_literal "0") then
    (failwith "Ratio.div_ratio: division by zero" : ratio)
  else if Ligo.gt_int_int y_num (Ligo.int_from_literal "0") then
    mul_ratio x { num = y_den; den = y_num; }
  else
    mul_ratio x { num = neg_int y_den; den = neg_int y_num; }

(* Other operations *)

let sign_ratio (x: ratio) : int =
  assert (Ligo.gt_int_int x.den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x.num (Ligo.int_from_literal "0") then
    -1
  else if Ligo.eq_int_int x.num (Ligo.int_from_literal "0") then
    0
  else
    1

let min_ratio (a: ratio) (b: ratio) : ratio = if leq_ratio_ratio a b then a else b

let max_ratio (a: ratio) (b: ratio) : ratio = if geq_ratio_ratio a b then a else b
