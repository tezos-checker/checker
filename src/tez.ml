type t = Nat.t
let scaling_factor = Z.of_int64 1000000L

(* Basic arithmetic operations. *)
let ( + ) x y = Nat.(x + y)
let ( - ) x y =
  match Nat.of_int (Nat.sub x y) with
  | None -> failwith "Tez.(-): negative"
  | Some z -> z

let compare x y = Nat.compare x y

let zero = Nat.zero
let one = Nat.abs scaling_factor

(* Conversions to/from other types. *)
let of_mutez amount =
  match Nat.of_int (Z.of_int amount) with
  | None -> failwith "Tez.of_mutez: negative"
  | Some z -> z

let to_q amount = Q.make (Nat.to_int amount) scaling_factor
let of_q_ceil amount = Nat.of_q_ceil (Q.make Z.(Q.num amount * scaling_factor) (Q.den amount))
let of_q_floor amount = Nat.of_q_floor (Q.make Z.(Q.num amount * scaling_factor) (Q.den amount))

(* Pretty printing functions *)
let show amount = Nat.show amount ^ "mutez"
let pp ppf amount = Format.fprintf ppf "%s" (show amount)

(* Tez payments *)
type payment = {destination: Address.t ; amount: t;}
[@@deriving show]
