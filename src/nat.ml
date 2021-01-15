type t = Ligo.int

let show = Ligo.string_of_int
let pp = Ligo.pp_int

let from_literal x =
  if x < 0 then
    failwith "Nat.from_literal: negative"
  else
    Ligo.int_from_literal x

let compare = Ligo.compare_int
let add x y = Ligo.add_int_int x y
let sub x y = Ligo.sub_int_int x y

let int x = x

let abs x = Ligo.abs_int x

let is_nat x =
  match Ligo.geq_int_int x (Ligo.int_from_literal 0) with
  | true -> Some x
  | false -> None
