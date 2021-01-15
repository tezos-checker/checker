type t = Z.t

let show = Z.to_string
let pp = Z.pp_print

let from_literal x =
  if x < 0 then
    failwith "Nat.from_literal: negative"
  else
    Z.of_int x

let compare = Z.compare
let add x y = Z.add x y
let sub x y = Z.sub x y

let int x = x

let abs x = Z.abs x

let is_nat x =
  match Z.geq x Z.zero with
  | true -> Some x
  | false -> None
