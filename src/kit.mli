type t

(* Basic operations. *)
val add : t -> t -> t
val sub : t -> t -> t
val scale : t -> FixedPoint.t -> t

val min : t -> t -> t
val max : t -> t -> t

val zero : t
val one : t

(* Conversions to/from other types. *)
val of_mukit : Ligo.int -> t
val to_mukit : t -> Ligo.int
val to_ratio : t -> Ratio.t
val of_ratio_ceil : Ratio.t -> t
val of_ratio_floor : Ratio.t -> t

(* Kit are really tickets. *)
type kit_token_content = Kit
type token = kit_token_content Ligo.ticket

val issue : t -> token

val with_valid_kit_token : token -> (token -> 'a) -> 'a

(* George: I really didn't want to have these here, but the clutter without
 * them was unbearable. They should be inlined, eventually. *)
val read_kit : token -> t * token
val split_or_fail : token -> t -> t -> token * token
val join_or_fail : token -> token -> token

(* BEGIN_OCAML *)
val compare : t -> t -> int

val pp : Format.formatter -> t -> unit
val show : t -> string

val pp_token : Format.formatter -> token -> unit
val show_token : token -> string
(* END_OCAML *)
