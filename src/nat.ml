type t = Z.t

let show = Z.to_string
let pp = Z.pp_print

let compare = Z.compare
let add x y = Z.(x + y)
let sub x y = Z.sub x y

let zero = Z.zero
let one = Z.one

let to_int x = x

let abs x = Z.abs x (* for now use this for creation also, but at the end an "n" suffix should do. *)

let of_int x =
  match Z.geq x Z.zero with
  | true -> Some x
  | false -> None

let to_ratio x = Ratio.make x Z.one

let of_ratio_floor x =
  if Ratio.lt x Ratio.zero then
    failwith "Nat.of_ratio_floor: negative"
  else
    Z.(fdiv (Ratio.num x) (Ratio.den x))

let of_ratio_ceil x =
  if Ratio.lt x Ratio.zero then
    failwith "Nat.of_ratio_ceil: negative"
  else
    Z.(cdiv (Ratio.num x) (Ratio.den x))
