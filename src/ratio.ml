open Common

(** A rational is represented as a pair numerator/denominator, reduced to have
  * a positive denominator and no common factor. This form is canonical. *)
type t = {
  num: Ligo.int; (** Numerator. *)
  den: Ligo.int; (** Denominator, > 0 *)
}

let num (x: t) : Ligo.int = x.num
let den (x: t) : Ligo.int = x.den

let rec gcd_rec (x, y: Ligo.int * Ligo.int) : Ligo.int =
  if y = Ligo.int_from_literal "0"
  then x
  else gcd_rec (y, Ligo.int (Ligo.mod_int_int x y))

let gcd_int_int (x: Ligo.int) (y: Ligo.int) : Ligo.int =
  gcd_rec (abs_int x, abs_int y)

(* normalize n/d, assuming d > 0 *)
let normalize (n: Ligo.int) (d: Ligo.int) : t =
  let g = gcd_int_int n d in
  if g = Ligo.int_from_literal "1"
  then { num = n; den = d }
  else { num = Ligo.div_int_int n g; den = Ligo.div_int_int d g; }

(* make and normalize n/d, assuming d > 0 *)
let make_real (n: Ligo.int) (d: Ligo.int) : t =
  if n = Ligo.int_from_literal "0" || d = Ligo.int_from_literal "1"
  then { num = n; den = Ligo.int_from_literal "1" }
  else normalize n d

(* make and normalize any fraction *)
let make (n: Ligo.int) (d: Ligo.int) : t =
  if Ligo.eq_int_int d (Ligo.int_from_literal "0") then
    (failwith "Ratio.make: division by zero" : t)
  else if Ligo.gt_int_int d (Ligo.int_from_literal "0") then
    make_real n d
  else
    make_real (neg_int n) (neg_int d)

(* Conversions to/from other types. *)
let of_int (i: Ligo.int) : t = { num = i; den = Ligo.int_from_literal "1"; }

let of_nat (n: Ligo.nat) : t = { num = Ligo.int n; den = Ligo.int_from_literal "1"; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_nat_floor (x: t) : Ligo.nat =
  if Ligo.lt_int_int x.num (Ligo.int_from_literal "0") then
    (failwith "Ratio.to_nat_floor: negative" : Ligo.nat)
  else
    Ligo.abs (fdiv_int_int x.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_nat_ceil (x: t) : Ligo.nat =
  if Ligo.lt_int_int x.num (Ligo.int_from_literal "0") then
    (failwith "Ratio.to_nat_ceil: negative" : Ligo.nat)
  else
    Ligo.abs (cdiv_int_int x.num x.den)

let of_tez (x: Ligo.tez) : t = { num = tez_to_mutez x; den = Ligo.int_from_literal "1_000_000"; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_tez_floor (x: t) : Ligo.tez =
  match Ligo.is_nat x.num with
  | None -> (failwith "Ratio.to_tez_floor: negative" : Ligo.tez)
  | Some n ->
    let n = Ligo.mul_nat_tez n (Ligo.tez_from_literal "1_000_000mutez") in
    let d = Ligo.abs x.den in
    (match Ligo.ediv_tez_nat n d with
     | None -> (failwith "Ratio.to_tez_floor: zero denominator" : Ligo.tez)
     | Some quot_and_rem ->
       let (quot, _) = quot_and_rem in
       quot (* ignore the remainder; we floor towards zero here *)
    )

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_tez_ceil (x: t) : Ligo.tez =
  match Ligo.is_nat x.num with
  | None -> (failwith "Ratio.to_tez_ceil: negative" : Ligo.tez)
  | Some n ->
    let n = Ligo.mul_nat_tez n (Ligo.tez_from_literal "1_000_000mutez") in
    let d = Ligo.abs x.den in
    (match Ligo.ediv_tez_nat n d with
     | None -> (failwith "Ratio.to_tez_ceil: zero denominator" : Ligo.tez)
     | Some quot_and_rem ->
       let (quot, rem) = quot_and_rem in
       if Ligo.eq_tez_tez rem (Ligo.tez_from_literal "0mutez")
       then quot
       else Ligo.add_tez_tez quot (Ligo.tez_from_literal "1mutez")
    )

(* Predefined values *)
let zero : t = { num = Ligo.int_from_literal "0"; den = Ligo.int_from_literal "1"; }
let one : t = { num = Ligo.int_from_literal "1"; den = Ligo.int_from_literal "1"; }
let minus_one : t = { num = Ligo.int_from_literal "-1"; den = Ligo.int_from_literal "1"; }

(* If we wish to rely on the fact that the rationals are normalized, we could
 * instead implement equality very efficiently as
 *
 *   x.num = y.num && x.den = y.den
 *
 * but I'd like to avoid that. Ideally we'll drop all fancy normalization, and
 * have rationals be lightweight. We always return to fixed-point, tez, and
 * kit, at the end anyway. *)
let equal (x: t) (y: t) : bool =
  if x.den = y.den (* avoid multiplication, for performance. *)
  then Ligo.eq_int_int x.num y.num
  else
    Ligo.eq_int_int
      (Ligo.mul_int_int x.num y.den)
      (Ligo.mul_int_int y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let leq (x: t) (y: t) : bool =
  if x.den = y.den (* avoid multiplication, for performance. *)
  then Ligo.leq_int_int x.num y.num
  else
    Ligo.leq_int_int
      (Ligo.mul_int_int x.num y.den)
      (Ligo.mul_int_int y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let lt (x: t) (y: t) : bool =
  if x.den = y.den (* avoid multiplication, for performance. *)
  then Ligo.lt_int_int x.num y.num
  else
    Ligo.lt_int_int
      (Ligo.mul_int_int x.num y.den)
      (Ligo.mul_int_int y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let geq (x: t) (y: t) : bool = leq y x

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let gt (x: t) (y: t) : bool = lt y x

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let min (a: t) (b: t) : t = if leq a b then a else b

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let max (a: t) (b: t) : t = if geq a b then a else b

let to_int (x: t) : Ligo.int = Ligo.div_int_int x.num x.den

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let neg (x: t) : t = { num = neg_int x.num; den = x.den; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let abs_ratio (x: t) : t = { num = abs_int x.num; den = x.den; }

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let add (x: t) (y: t) : t =
  if x.den = y.den then (* avoid multiplication, for performance. *)
    make_real (Ligo.add_int_int x.num y.num) x.den
  else
    make_real
      (Ligo.add_int_int
         (Ligo.mul_int_int x.num y.den)
         (Ligo.mul_int_int y.num x.den))
      (Ligo.mul_int_int x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let sub (x: t) (y: t) : t =
  if x.den = y.den then
    make_real (Ligo.sub_int_int x.num y.num) x.den (* avoid multiplication, for performance. *)
  else
    make_real
      (Ligo.sub_int_int
         (Ligo.mul_int_int x.num y.den)
         (Ligo.mul_int_int y.num x.den))
      (Ligo.mul_int_int x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let mul (x: t) (y: t) : t = make_real (Ligo.mul_int_int x.num y.num) (Ligo.mul_int_int x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let inv (x: t) : t =
  if Ligo.eq_int_int x.num (Ligo.int_from_literal "0") then
    (failwith "Ratio.inv: division by zero" : t)
  else if Ligo.gt_int_int x.num (Ligo.int_from_literal "0") then
    { num = x.den; den = x.num; }
  else
    { num = neg_int x.den; den = neg_int x.num; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let div (x: t) (y: t) : t =
  if Ligo.eq_int_int y.num (Ligo.int_from_literal "0") then
    (failwith "Ratio.div: division by zero" : t)
  else if Ligo.gt_int_int y.num (Ligo.int_from_literal "0") then
    mul x { num = y.den; den = y.num; }
  else
    mul x { num = neg_int y.den; den = neg_int y.num; }

(* BEGIN_OCAML *)
let show n = (Ligo.string_of_int n.num) ^ "/" ^ (Ligo.string_of_int n.den)
let pp f x = Format.pp_print_string f (show x)

(* NOTE: this implementation relies on the fact that the rationals are normalized. *)
let sign x =
  if Ligo.lt_int_int x.num (Ligo.int_from_literal "0") then
    -1
  else if Ligo.eq_int_int x.num (Ligo.int_from_literal "0") then
    0
  else
    1
(* END_OCAML *)
