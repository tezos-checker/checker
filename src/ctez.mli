open Common

type ctez

(* Basic operations. *)
val ctez_add : ctez -> ctez -> ctez
val ctez_sub : ctez -> ctez -> ctez

val ctez_zero : ctez
val ctez_scaling_factor_int : Ligo.int

(* Conversions to/from other types. *)
val ctez_of_muctez : Ligo.nat -> ctez
val ctez_to_muctez_nat : ctez -> Ligo.nat

val ctez_of_fraction_ceil : Ligo.int -> Ligo.int -> ctez
val ctez_of_fraction_floor : Ligo.int -> Ligo.int -> ctez

val eq_ctez_ctez : ctez -> ctez -> bool
val lt_ctez_ctez : ctez -> ctez -> bool
val gt_ctez_ctez : ctez -> ctez -> bool

(* BEGIN_OCAML *)
val ctez_to_muctez_int : ctez -> Ligo.int
val ctez_to_ratio : ctez -> Common.ratio
val ratio_of_ctez: ctez -> ratio
val ctez_from_tez : Ligo.tez -> ctez
val ctez_compare : ctez -> ctez -> int

val pp_ctez : Format.formatter -> ctez -> unit
val show_ctez : ctez -> string
(* END_OCAML *)
