type ptr = Ligo.int
(* BEGIN_OCAML *) [@@deriving show] (* END_OCAML *)

let[@inline] ptr_null = Ligo.int_from_literal "0"
let[@inline] ptr_init = Ligo.int_from_literal "1"
let[@inline] ptr_next (t: ptr) = Ligo.add_int_int t (Ligo.int_from_literal "1")

(* BEGIN_OCAML *)
type t = ptr
[@@deriving show]
let compare = Common.compare_int
let random_ptr () = Ligo.int_from_int64 (Random.int64 Int64.max_int)
(* END_OCAML *)
