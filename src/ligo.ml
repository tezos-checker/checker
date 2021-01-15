(* address *)

type address = string

let address_from_literal s = s

let compare_address = String.compare

let string_of_address s = s

let pp_address = Format.pp_print_string

(* int *)

type int = Z.t

let int_from_literal = Z.of_int

let compare_int = Z.compare

let string_of_int = Z.to_string

let pp_int fmt z = Format.pp_print_string fmt (string_of_int z)

let add_int_int = Z.add

let sub_int_int = Z.sub

let mul_int_int = Z.mul

let eq_int_int = Z.equal

let lt_int_int = Z.lt

let leq_int_int = Z.leq

let geq_int_int = Z.geq

let div_int_int = Z.div

let cdiv_int_int = Z.cdiv

let fdiv_int_int = Z.fdiv

let pow_int_nat b e =
  assert (e >= Z.zero);
  assert (e <= Z.of_int max_int);
  Z.pow b (Z.to_int e)

let shift_right_trunc_int_nat = Z.shift_right_trunc

let shift_left_int_nat = Z.shift_left

let gcd_int_int = Z.gcd

let sign_int = Z.sign

let neg_int = Z.neg

let abs_int = Z.abs

let of_string_base_int = Z.of_string_base

let div_rem_int_int = Z.div_rem

let format_int = Z.format

(* nat *)

type nat = Z.t

let add_nat_nat = Z.add

let sub_nat_nat = Z.sub

let int x = x

let abs = Z.abs

let is_nat x = if Z.lt x Z.zero then None else Some x

let string_of_nat = Z.to_string

let pp_nat fmt z = Format.pp_print_string fmt (string_of_int z)

let compare_nat = Z.compare

let nat_from_literal x =
  if x < 0 then
    failwith "Ligo.nat_from_literal: negative"
  else
    Z.of_int x

(* timestamp *)

type timestamp = Z.t

let add_timestamp_int = Z.add

let sub_timestamp_timestamp = Z.sub

let string_of_timestamp = Z.to_string

let pp_timestamp fmt z = Format.pp_print_string fmt (string_of_int z)

let compare_timestamp = Z.compare

let timestamp_from_seconds_literal s =
  if s < 0 then
    failwith "Ligo.timestamp_from_seconds_literal: negative"
  else
    Z.of_int s
