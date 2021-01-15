
(** A rational is represented as a pair numerator/denominator, reduced to have
  * a positive denominator and no common factor. This form is canonical. *)
type t = {
  num: Z.t; (** Numerator. *)
  den: Z.t; (** Denominator, > 0 *)
}

let num x = x.num
let den x = x.den

(* normalize n/d, assuming d > 0 *)
let normalize n d =
  let g = Z.gcd n d in
  if g = Z.one
  then { num = n; den = d }
  else { num = Z.div n g; den = Z.div d g; }

(* make and normalize n/d, assuming d > 0 *)
let make_real n d =
  if n = Z.zero || d = Z.one
  then { num = n; den = Z.one }
  else normalize n d

(* make and normalize any fraction *)
let make n d =
  let sd = Z.sign d in
  if sd = 0 then
    failwith "Ratio.make: division by zero"
  else
  if sd > 0 then make_real n d else
    make_real (Z.neg n) (Z.neg d)

(* Conversions to/from other types. *)
let of_bigint n = { num = n; den = Z.one; }
let of_int n = { num = Z.of_int n; den = Z.one; }

let of_nat n = { num = Nat.int n; den = Z.one }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_nat_floor x =
  if Z.sign x.num = -1 then
    failwith "Ratio.to_nat_floor: negative"
  else
    Nat.abs (Z.fdiv x.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_nat_ceil x =
  if Z.sign x.num = -1 then
    failwith "Ratio.to_nat_ceil: negative"
  else
    Nat.abs (Z.cdiv x.num x.den)

(* Predefined values *)
let zero = { num = Z.zero; den = Z.one; }
let one = { num = Z.one; den = Z.one; }
let minus_one = { num = Z.minus_one; den = Z.one; }

(* NOTE: this implementation relies on the fact that the rationals are normalized. *)
let sign x = Z.sign x.num

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let compare x y =
  if x.den = y.den
  then Z.compare x.num y.num (* avoid multiplication, for performance. *)
  else
    Z.compare
      (Z.mul x.num y.den)
      (Z.mul y.num x.den)

(* If we wish to rely on the fact that the rationals are normalized, we could
 * instead implement equality very efficiently as
 *
 *   (Z.equal x.num y.num) && (Z.equal x.den y.den)
 *
 * but I'd like to avoid that. Ideally we'll drop all fancy normalization, and
 * have rationals be lightweight. We always return to fixed-point, tez, and
 * kit, at the end anyway. *)
let equal x y = compare x y = 0

let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let leq x y =
  if x.den = y.den (* avoid multiplication, for performance. *)
  then Z.leq x.num y.num
  else
    Z.leq
      (Z.mul x.num y.den)
      (Z.mul y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let lt x y =
  if x.den = y.den (* avoid multiplication, for performance. *)
  then Z.lt x.num y.num
  else
    Z.lt
      (Z.mul x.num y.den)
      (Z.mul y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let geq x y = leq y x

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let gt x y = lt y x

let to_bigint x = Z.div x.num x.den

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let neg x = { num = Z.neg x.num; den = x.den; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let abs x = { num = Z.abs x.num; den = x.den; }

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let add x y =
  if x.den = y.den then (* avoid multiplication, for performance. *)
    make_real (Z.add x.num y.num) x.den
  else
    make_real
      (Z.add
         (Z.mul x.num y.den)
         (Z.mul y.num x.den))
      (Z.mul x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let sub x y =
  if x.den = y.den then
    make_real (Z.sub x.num y.num) x.den (* avoid multiplication, for performance. *)
  else
    make_real
      (Z.sub
         (Z.mul x.num y.den)
         (Z.mul y.num x.den))
      (Z.mul x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let mul x y = make_real (Z.mul x.num y.num) (Z.mul x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let inv x =
  match Z.sign x.num with
  | 0 -> failwith "Ratio.inv: division by zero"
  | 1 -> { num = x.den; den = x.num; }
  | _ -> { num = Z.neg x.den; den = Z.neg x.num; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let div x y =
  if y.num = Z.zero then
    failwith "Ratio.div: division by zero"
  else if Z.sign y.num >= 0
  then mul x { num = y.den; den = y.num; }
  else mul x { num = Z.neg y.den; den = Z.neg y.num; }

(* Pretty printing functions *)
let show n = (Z.to_string n.num) ^ "/" ^ (Z.to_string n.den)
let pp f x = Format.pp_print_string f (show x)
