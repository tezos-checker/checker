open Common

(** A rational is represented as a pair numerator/denominator, reduced to have
  * a positive denominator. This form is canonical. *)
type ratio = {
  num: Ligo.int; (** Numerator. *)
  den: Ligo.int; (** Denominator, > 0 *)
}

(* make and normalize n/d, assuming d > 0 *)
let[@inline] make_real_unsafe (n: Ligo.int) (d: Ligo.int) : ratio =
  assert (Ligo.gt_int_int d (Ligo.int_from_literal "0"));
  { num = n; den = d; }

(* Conversions to/from other types. *)
(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let fraction_to_nat_floor (x_num: Ligo.int) (x_den: Ligo.int) : Ligo.nat =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0") then
    (failwith "Ratio.fraction_to_nat_floor: negative" : Ligo.nat)
  else
    Ligo.abs (fdiv_int_int x_num x_den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let fraction_to_tez_floor (x_num: Ligo.int) (x_den: Ligo.int) : Ligo.tez =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  match Ligo.is_nat x_num with
  | None -> (failwith "Ratio.fraction_to_tez_floor: negative" : Ligo.tez)
  | Some n ->
    let n = Ligo.mul_nat_tez n (Ligo.tez_from_literal "1_000_000mutez") in
    let d = Ligo.abs x_den in
    (match Ligo.ediv_tez_nat n d with
     | None -> (failwith "Ratio.fraction_to_tez_floor: zero denominator" : Ligo.tez)
     | Some quot_and_rem ->
       let (quot, _) = quot_and_rem in
       quot (* ignore the remainder; we floor towards zero here *)
    )

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let fraction_to_tez_ceil (x_num: Ligo.int) (x_den: Ligo.int) : Ligo.tez =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  match Ligo.is_nat x_num with
  | None -> (failwith "Ratio.fraction_to_tez_ceil: negative" : Ligo.tez)
  | Some n ->
    let n = Ligo.mul_nat_tez n (Ligo.tez_from_literal "1_000_000mutez") in
    let d = Ligo.abs x_den in
    (match Ligo.ediv_tez_nat n d with
     | None -> (failwith "Ratio.fraction_to_tez_ceil: zero denominator" : Ligo.tez)
     | Some quot_and_rem ->
       let (quot, rem) = quot_and_rem in
       if Ligo.eq_tez_tez rem (Ligo.tez_from_literal "0mutez")
       then quot
       else Ligo.add_tez_tez quot (Ligo.tez_from_literal "1mutez")
    )

(* Predefined values *)
let[@inline] zero_ratio : ratio = { num = Ligo.int_from_literal "0"; den = Ligo.int_from_literal "1"; }
let[@inline] one_ratio : ratio = { num = Ligo.int_from_literal "1"; den = Ligo.int_from_literal "1"; }

(* BEGIN_OCAML *)
[@@@coverage off]
(* make and normalize any fraction *)
let make_ratio (n: Ligo.int) (d: Ligo.int) : ratio =
  if Ligo.eq_int_int d (Ligo.int_from_literal "0") then
    (failwith "Ratio.make_ratio: division by zero" : ratio)
  else if Ligo.gt_int_int d (Ligo.int_from_literal "0") then
    make_real_unsafe n d
  else
    make_real_unsafe (neg_int n) (neg_int d)

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
  make_real_unsafe
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
  make_real_unsafe
    (Ligo.add_int_int
       (Ligo.mul_int_int x_num y_den)
       (Ligo.mul_int_int y_num x_den))
    (Ligo.mul_int_int x_den y_den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let sub_ratio (x: ratio) (y: ratio) : ratio =
  let { num = x_num; den = x_den; } = x in
  let { num = y_num; den = y_den; } = y in
  make_real_unsafe
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

let qexp (x: ratio) : ratio =
  let { num = x_num; den = x_den; } = x in
  { num = Ligo.add_int_int x_num x_den; den = x_den; }

let show_ratio n = (Ligo.string_of_int n.num) ^ "/" ^ (Ligo.string_of_int n.den)
let pp_ratio f x = Format.pp_print_string f (show_ratio x)

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
