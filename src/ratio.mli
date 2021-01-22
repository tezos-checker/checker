
type t

(* Construction/deconstruction. *)
val make: Ligo.int -> Ligo.int -> t
val num: t -> Ligo.int
val den: t -> Ligo.int

(* Predefined values *)
val zero: t
val one: t
val minus_one:t

(* Conversions to/from other types. *)
val of_int: Ligo.int -> t
val to_int: t -> Ligo.int

val of_nat: Ligo.nat -> t
val to_nat_floor: t -> Ligo.nat
val to_nat_ceil: t -> Ligo.nat

val of_tez: Ligo.tez -> t
val to_tez_floor: t -> Ligo.tez
val to_tez_ceil: t -> Ligo.tez

(* Relational operations. *)
val equal: t -> t -> bool
val lt: t -> t -> bool
val gt: t -> t -> bool
val leq: t -> t -> bool
val geq: t -> t -> bool

(* Other operations *)

(* NOTE: OCaml's polymorphic comparison will NOT return a result consistent
 * with the ordering of rationals we provide here (OCaml's comparison will use
 * lexicographic ordering which is incorrect in this instance). *)
val min: t -> t -> t
val max: t -> t -> t

(* Basic unary operations. *)
val neg: t -> t
val abs_ratio: t -> t
val inv: t -> t

(* Basic binary operations. *)
val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t
val div: t -> t -> t

(* BEGIN_OCAML *)
val pp: Format.formatter -> t -> unit
val show: t -> string

val sign: t -> int
(* END_OCAML *)
