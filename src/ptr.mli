type ptr (* George: perhaps I'd prefer a phantom parameter to not mix up pointers. *)

val nat_of_ptr : ptr -> Ligo.nat
val ptr_null : ptr
val ptr_init : ptr
val ptr_next : ptr -> ptr

val compare_ptr : ptr -> ptr -> int
val show_ptr : ptr -> string
val pp_ptr : Format.formatter -> ptr -> unit
val random_ptr : unit -> ptr
