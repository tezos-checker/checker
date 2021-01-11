type t = Z.t

let show = Z.to_string
let pp = Z.pp_print

let compare = Z.compare
let ( + ) x y = Z.(x + y)
let sub x y = Z.sub x y

let zero = Z.zero
let one = Z.one

let to_int x = x

let abs x = Z.abs x (* for now use this for creation also, but at the end an "n" suffix should do. *)

let of_int x =
  match Z.geq x Z.zero with
  | true -> Some x
  | false -> None

let to_q x = Q.make x Z.one

let of_q_floor x =
  if not (Q.is_real x) then
    failwith "Nat.of_q_floor: infinity/undef"
  else if Q.lt x Q.zero then
    failwith "Nat.of_q_floor: negative"
  else
    Z.(fdiv (Q.num x) (Q.den x))

let of_q_ceil x =
  if not (Q.is_real x) then
    failwith "Nat.of_q_ceil: infinity/undef"
  else if Q.lt x Q.zero then
    failwith "Nat.of_q_ceil: negative"
  else
    Z.(cdiv (Q.num x) (Q.den x))
