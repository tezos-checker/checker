type t

(* Basic operations. *)
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val scale : t -> FixedPoint.t -> t
val compare : t -> t -> int

val zero : t
val one : t

(* Conversions to/from other types. *)
val of_mukit : Z.t -> t
val to_mukit : t -> Z.t
val to_ratio : t -> Ratio.t
val of_ratio_ceil : Ratio.t -> t
val of_ratio_floor : Ratio.t -> t

(* Pretty printing functions *)
val pp : Format.formatter -> t -> unit
val show : t -> string

type Error.error +=
  | InvalidKitToken

(* Kit are really tickets. *)
type kit_token_content = Kit
type token = kit_token_content Ticket.t
val pp_token : Format.formatter -> token -> unit
val show_token : token -> string

val issue : tezos:Tezos.t -> t -> token

val with_valid_kit_token :
  tezos:Tezos.t ->
  token ->
  (token -> ('a, Error.error) result) ->
  ('a, Error.error) result

(* George: I really didn't want to have these here, but the clutter without
 * them was unbearable. They should be inlined, eventually. *)
val read_kit : token -> t * token
val split_or_fail : token -> t -> t -> token * token
val join_or_fail : token -> token -> token

