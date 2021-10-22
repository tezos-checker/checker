open Common

type ctok

(* Basic operations. *)
val ctok_add : ctok -> ctok -> ctok
val ctok_sub : ctok -> ctok -> ctok

val ctok_zero : ctok
val ctok_scaling_factor_int : Ligo.int

(* Conversions to/from other types. *)
val ctok_of_muctok : Ligo.nat -> ctok
val ctok_to_muctok_nat : ctok -> Ligo.nat

val ctok_of_fraction_ceil : Ligo.int -> Ligo.int -> ctok
val ctok_of_fraction_floor : Ligo.int -> Ligo.int -> ctok

val eq_ctok_ctok : ctok -> ctok -> bool
val lt_ctok_ctok : ctok -> ctok -> bool
val gt_ctok_ctok : ctok -> ctok -> bool

(* BEGIN_OCAML *)
val ctok_to_muctok_int : ctok -> Ligo.int
val ctok_to_ratio : ctok -> Common.ratio
val ratio_of_ctok: ctok -> ratio
val ctok_from_tez : Ligo.tez -> ctok
val ctok_compare : ctok -> ctok -> int

val pp_ctok : Format.formatter -> ctok -> unit
val show_ctok : ctok -> string
(* END_OCAML *)
