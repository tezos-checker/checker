type int = Z.t

let int_from_literal = Z.of_int

let compare_int = Z.compare

let string_of_int = Z.to_string

let pp_int fmt z = Format.pp_print_string fmt (string_of_int z)

let add_int_int = Z.add
