type t (* INVARIANT: NON-NEGATIVE *)

val show : t -> string
val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
val ( + ) : t -> t -> t
val sub : t -> t -> Z.t

val zero : t
val one : t

val to_int : t -> Z.t        (* In LIGO: int *)
val abs    : Z.t -> t        (* In LIGO: abs *)
val of_int : Z.t -> t option (* In LIGO: Michelson.is_nat *)

val to_ratio : t -> Ratio.t (* Always succeeds. *)
val of_ratio_floor : Ratio.t -> t (* May fail, if the input is negative. Rounds down. *)
val of_ratio_ceil : Ratio.t -> t  (* May fail, if the input is negative. Rounds up *)
