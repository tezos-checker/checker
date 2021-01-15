type t = Ligo.nat
let scaling_factor = Ligo.int_from_literal 1_000_000

(* Basic arithmetic operations. *)
let add x y = Ligo.add_nat_nat x y
let sub x y =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | None -> failwith "Tez.sub: negative"
  | Some z -> z

let compare x y = Ligo.compare_nat x y

let zero = Ligo.nat_from_literal 0
let one = Ligo.nat_from_literal 1_000_000

(* Conversions to/from other types. *)
let of_mutez amount =
  match Ligo.is_nat amount with
  | None -> failwith "Tez.of_mutez: negative"
  | Some z -> z

let to_mutez amount = Ligo.int amount
let to_ratio amount = Ratio.make (Ligo.int amount) scaling_factor
let of_ratio_ceil  amount = Ratio.to_nat_ceil  (Ratio.make (Ligo.mul_int_int (Ratio.num amount) scaling_factor) (Ratio.den amount))
let of_ratio_floor amount = Ratio.to_nat_floor (Ratio.make (Ligo.mul_int_int (Ratio.num amount) scaling_factor) (Ratio.den amount))

(* Pretty printing functions *)
let show amount = Ligo.string_of_nat amount ^ "mutez"
let pp ppf amount = Format.fprintf ppf "%s" (show amount)

(* Tez payments *)
type payment = {destination: Ligo.address; amount: t;}
[@@deriving show]
