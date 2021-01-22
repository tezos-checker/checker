type bid_ticket

type t

val empty : t

val touch : t -> t

(** Retrieve the delegate for this cycle *)
val delegate : t -> Ligo.address option

val cycle : t -> Ligo.nat

val winning_amount : t -> Ligo.tez option

(* TODO: can we bid to nominate someone else as a baker? *)
val place_bid : t -> Ligo.address -> Ligo.tez -> bid_ticket * t

val claim_win : t -> bid_ticket -> t

val reclaim_bid : t -> bid_ticket -> Ligo.tez * t

val show : t -> string
val pp : Format.formatter -> t -> unit
