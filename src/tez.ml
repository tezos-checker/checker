type t = Nat.t
let scaling_factor = Z.of_int 1_000_000

(* Basic arithmetic operations. *)
let add x y = Nat.add x y
let sub x y =
  match Nat.is_nat (Nat.sub x y) with
  | None -> failwith "Tez.sub: negative"
  | Some z -> z

let compare x y = Nat.compare x y

let zero = Nat.from_literal 0
let one = Nat.from_literal 1_000_000

(* Conversions to/from other types. *)
let of_mutez amount =
  match Nat.is_nat amount with
  | None -> failwith "Tez.of_mutez: negative"
  | Some z -> z

let to_mutez amount = Nat.int amount
let to_ratio amount = Ratio.make (Nat.int amount) scaling_factor
let of_ratio_ceil  amount = Ratio.to_nat_ceil  (Ratio.make (Z.mul (Ratio.num amount) scaling_factor) (Ratio.den amount))
let of_ratio_floor amount = Ratio.to_nat_floor (Ratio.make (Z.mul (Ratio.num amount) scaling_factor) (Ratio.den amount))

(* Pretty printing functions *)
let show amount = Nat.show amount ^ "mutez"
let pp ppf amount = Format.fprintf ppf "%s" (show amount)

(* Tez payments *)
type payment = {destination: Address.t; amount: t;}
[@@deriving show]
