type ptr (* George: perhaps I'd prefer a phantom parameter to not mix up pointers. *)

val ptr_null : ptr
val ptr_init : ptr
val ptr_next : ptr -> ptr
val ptr_compare : ptr -> ptr -> int

type t = ptr
val compare : ptr -> ptr -> int
val show : ptr -> string
val pp : Format.formatter -> ptr -> unit
val pp_ptr : Format.formatter -> ptr -> unit
val random_ptr : unit -> ptr
