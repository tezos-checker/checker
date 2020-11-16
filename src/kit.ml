(* ************************************************************************* *)
(*                                   Kit                                     *)
(* ************************************************************************* *)
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

let to_q amount = Q.make amount scaling_factor
let of_q_ceil amount = Z.(cdiv (Q.num amount * scaling_factor) (Q.den amount))
let of_q_floor amount = Z.(fdiv (Q.num amount * scaling_factor) (Q.den amount))
(* George: do we need flooring-division or truncating-division? more thought is needed *)

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

  let sign = if amount < Z.zero then "-" else "" in
  let (upper, lower) = Z.div_rem (Z.abs amount) scaling_factor in
  Format.sprintf "%s%s.%s"
    sign
    (Z.to_string upper)
    (zfill (Z.to_string lower) scaling_exponent)

let pp ppf amount = Format.fprintf ppf "%s" (show amount)

