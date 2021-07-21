[@@@coverage off]

type ptr = Ligo.nat [@@deriving show]

[@@@coverage on]

let[@inline] ptr_null = Ligo.nat_from_literal "0n"

let[@inline] ptr_init = Ligo.nat_from_literal "1n"

let[@inline] ptr_next (t : ptr) =
  Ligo.add_nat_nat t (Ligo.nat_from_literal "1n")

(* BEGIN_OCAML *)
[@@@coverage off]

let compare_ptr = Common.compare_nat

let random_ptr () = Ligo.nat_from_int64 (Random.int64 Int64.max_int)

[@@@coverage on]

(* END_OCAML *)
