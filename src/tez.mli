(* Tez payments *)
type payment = {destination: Ligo.address ; amount: Ligo.tez;}

val pp_payment : Format.formatter -> payment -> unit
val show_payment : payment -> string
