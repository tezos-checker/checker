open Ratio
(*
open FixedPoint
*)

type ctez

(* Basic operations. *)
val ctez_add : ctez -> ctez -> ctez
val ctez_sub : ctez -> ctez -> ctez
(*
val ctez_scale : ctez -> fixedpoint -> ctez

val ctez_min : ctez -> ctez -> ctez
val ctez_max : ctez -> ctez -> ctez
*)

val ctez_zero : ctez
val ctez_scaling_factor_int : Ligo.int
(*
val ctez_one : ctez
*)

(* Conversions to/from other types. *)
val ctez_of_muctez : Ligo.nat -> ctez
val ctez_to_muctez_int : ctez -> Ligo.int
(*
val ctez_to_muctez_nat : ctez -> Ligo.nat
*)

val ctez_of_fraction_ceil : Ligo.int -> Ligo.int -> ctez
val ctez_of_fraction_floor : Ligo.int -> Ligo.int -> ctez

(* BEGIN_OCAML *)
val ctez_compare : ctez -> ctez -> int

val ctez_to_ratio : ctez -> Ratio.ratio

val pp_ctez : Format.formatter -> ctez -> unit
val show_ctez : ctez -> string
(* END_OCAML *)

val ctez_from_tez : Ligo.tez -> ctez
val tez_from_ctez : ctez -> Ligo.tez
val ratio_of_ctez: ctez -> ratio
