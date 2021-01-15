type int = Z.t

let int_from_literal = Z.of_int

let compare_int = Z.compare

let string_of_int = Z.to_string

let pp_int fmt z = Format.pp_print_string fmt (string_of_int z)

let add_int_int = Z.add

let sub_int_int = Z.sub

let mul_int_int = Z.mul

let lt_int_int = Z.lt

let leq_int_int = Z.leq

let geq_int_int = Z.geq

let div_int_int = Z.div

let cdiv_int_int = Z.cdiv

let fdiv_int_int = Z.fdiv

let pow_int_nat = Z.pow

let shift_right_trunc_int_nat = Z.shift_right_trunc

let shift_left_int_nat = Z.shift_left

let gcd_int_int = Z.gcd

let sign_int = Z.sign

let neg_int = Z.neg

let abs_int = Z.abs

let of_string_base_int = Z.of_string_base

let div_rem_int_int = Z.div_rem

let format_int = Z.format
