
(** A rational is represented as a pair numerator/denominator, reduced to have
  * a positive denominator and no common factor. This form is canonical. *)
type t = {
  num: Ligo.int; (** Numerator. *)
  den: Ligo.int; (** Denominator, > 0 *)
}

let num x = x.num
let den x = x.den

(* normalize n/d, assuming d > 0 *)
let normalize n d =
  let g = Ligo.gcd_int_int n d in
  if g = Ligo.int_from_literal 1
  then { num = n; den = d }
  else { num = Ligo.div_int_int n g; den = Ligo.div_int_int d g; }

(* make and normalize n/d, assuming d > 0 *)
let make_real n d =
  if n = Ligo.int_from_literal 0 || d = Ligo.int_from_literal 1
  then { num = n; den = Ligo.int_from_literal 1 }
  else normalize n d

(* make and normalize any fraction *)
let make n d =
  let sd = Ligo.sign_int d in
  if sd = 0 then
    failwith "Ratio.make: division by zero"
  else
  if sd > 0 then make_real n d else
    make_real (Ligo.neg_int n) (Ligo.neg_int d)

(* Conversions to/from other types. *)
let of_bigint n = { num = n; den = Ligo.int_from_literal 1; }
let of_int n = { num = Ligo.int_from_literal n; den = Ligo.int_from_literal 1; }

let of_nat n = { num = Ligo.int n; den = Ligo.int_from_literal 1 }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_nat_floor x =
  if Ligo.sign_int x.num = -1 then
    failwith "Ratio.to_nat_floor: negative"
  else
    Ligo.abs (Ligo.fdiv_int_int x.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_nat_ceil x =
  if Ligo.sign_int x.num = -1 then
    failwith "Ratio.to_nat_ceil: negative"
  else
    Ligo.abs (Ligo.cdiv_int_int x.num x.den)

let of_tez x = { num = Common.tez_to_mutez x; den = Ligo.int_from_literal 1_000_000; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_tez_floor x =
  match Ligo.is_nat x.num with
  | None -> failwith "Ratio.to_tez_floor: negative"
  | Some n ->
    let n = Ligo.mul_nat_tez n (Ligo.tez_from_mutez_literal 1_000_000) in
    let d = Ligo.abs x.den in
    match Ligo.ediv_tez_nat n d with
    | None -> (failwith "Ratio.to_tez_floor: zero denominator" : Ligo.tez)
    | Some (quot, _rem) -> quot (* ignore the remainder; we floor towards zero here *)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let to_tez_ceil x =
  match Ligo.is_nat x.num with
  | None -> failwith "Ratio.to_tez_ceil: negative"
  | Some n ->
    let n = Ligo.mul_nat_tez n (Ligo.tez_from_mutez_literal 1_000_000) in
    let d = Ligo.abs x.den in
    match Ligo.ediv_tez_nat n d with
    | None -> (failwith "Ratio.to_tez_ceil: zero denominator" : Ligo.tez)
    | Some (quot, rem) ->
      if Ligo.eq_tez_tez rem (Ligo.tez_from_mutez_literal 0)
      then quot
      else Ligo.add_tez_tez quot (Ligo.tez_from_mutez_literal 1)

(* Predefined values *)
let zero = { num = Ligo.int_from_literal 0; den = Ligo.int_from_literal 1; }
let one = { num = Ligo.int_from_literal 1; den = Ligo.int_from_literal 1; }
let minus_one = { num = Ligo.int_from_literal (-1); den = Ligo.int_from_literal 1; }

(* NOTE: this implementation relies on the fact that the rationals are normalized. *)
let sign x = Ligo.sign_int x.num

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let compare x y =
  if x.den = y.den
  then Ligo.compare_int x.num y.num (* avoid multiplication, for performance. *)
  else
    Ligo.compare_int
      (Ligo.mul_int_int x.num y.den)
      (Ligo.mul_int_int y.num x.den)

(* If we wish to rely on the fact that the rationals are normalized, we could
 * instead implement equality very efficiently as
 *
 *   x.num = y.num && x.den = y.den
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
  then Ligo.leq_int_int x.num y.num
  else
    Ligo.leq_int_int
      (Ligo.mul_int_int x.num y.den)
      (Ligo.mul_int_int y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let lt x y =
  if x.den = y.den (* avoid multiplication, for performance. *)
  then Ligo.lt_int_int x.num y.num
  else
    Ligo.lt_int_int
      (Ligo.mul_int_int x.num y.den)
      (Ligo.mul_int_int y.num x.den)

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let geq x y = leq y x

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let gt x y = lt y x

let to_bigint x = Ligo.div_int_int x.num x.den

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let neg x = { num = Ligo.neg_int x.num; den = x.den; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let abs x = { num = Ligo.abs_int x.num; den = x.den; }

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let add x y =
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
let sub x y =
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
let mul x y = make_real (Ligo.mul_int_int x.num y.num) (Ligo.mul_int_int x.den y.den)

(* NOTE: this implementation does not rely on the fact that the denominator is
 * always positive, but it definitely preserves it. *)
let inv x =
  match Ligo.sign_int x.num with
  | 0 -> failwith "Ratio.inv: division by zero"
  | 1 -> { num = x.den; den = x.num; }
  | _ -> { num = Ligo.neg_int x.den; den = Ligo.neg_int x.num; }

(* NOTE: this implementation relies on the fact that the denominator is always positive. *)
let div x y =
  if y.num = Ligo.int_from_literal 0 then
    failwith "Ratio.div: division by zero"
  else if Ligo.sign_int y.num >= 0
  then mul x { num = y.den; den = y.num; }
  else mul x { num = Ligo.neg_int y.den; den = Ligo.neg_int y.num; }

(* Pretty printing functions *)
let show n = (Ligo.string_of_int n.num) ^ "/" ^ (Ligo.string_of_int n.den)
let pp f x = Format.pp_print_string f (show x)
