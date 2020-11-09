
open FixedPoint

(* ************************************************************************* *)
(*                                   Kit                                     *)
(* ************************************************************************* *)
module Kit : sig
  type t (* Invariant: >= zero *)

  val scaling_factor : Z.t

  (* Basic arithmetic operations. *)
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> FixedPoint.t

  val zero : t
  val one : t

  val compare : t -> t -> int

  (* Conversions to/from other types. *)
  val of_mukit : int -> t

  val to_fp : t -> FixedPoint.t

  val scale : t -> FixedPoint.t -> t

  (* Pretty printing functions *)
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end =
struct
  type t = Z.t
  let scaling_factor = Z.of_int64 1000000L
  let scaling_exponent = 6

  (* Basic arithmetic operations. *)
  let ( + ) x y = Z.(x + y)
  let ( - ) x y = Z.(x - y)

  let compare x y = Z.compare x y

  let zero = Z.zero
  let one = scaling_factor

  (* Conversions to/from other types. *)
  let of_mukit = Z.of_int

  let to_fp t = (* TODO: overflow check? *)
    FixedPoint.of_rep Z.(t * (FixedPoint.scaling_factor / scaling_factor))

  let ( / ) x y = (* TODO: lossy *)
    FixedPoint.(to_fp x / to_fp y)

  let scale amount fp = (* TODO: Over/Under- flow checks *)
    Z.(FixedPoint.(to_rep (to_fp amount * fp)) * scaling_factor / FixedPoint.scaling_factor)

  (* Pretty printing functions *)
  let show amount =
    let zfill s width =
      let to_fill = Stdlib.(width - (String.length s)) in
      if to_fill <= 0
      then s
      else (String.make to_fill '0') ^ s in

    let (upper, lower) = Z.div_rem amount scaling_factor in
    Format.sprintf "%s.%s"
      (Z.to_string upper)
      (zfill (Z.to_string lower) scaling_exponent)

  let pp ppf amount =
    Format.fprintf ppf "%s" (show amount)
end

