type bid_ticket

type t

val empty : Tezos.t -> t

(** Retrieve the delegate for this cycle *)
val delegate : t -> Tezos.t -> Address.t option * t

(* TODO: can we bid to nominate someone else as a baker? *)
val place_bid : t -> Tezos.t -> sender:Address.t -> amount:Tez.t -> (bid_ticket * t, Error.error) result

val claim_win : t -> Tezos.t -> bid_ticket:bid_ticket
  -> (t, Error.error) result

val reclaim_bid : t -> Tezos.t -> address:Address.t -> bid_ticket:bid_ticket
  -> (Tez.t * t, Error.error) result

val show : t -> string
val pp : Format.formatter -> t -> unit
