open FixedPoint
open Tez

(* ************************************************************************* *)
(*                               Parameters                                  *)
(* ************************************************************************* *)
module Parameters : sig
  type parameters =
    { q : FixedPoint.t [@printer FixedPoint.pp]; (* 1/kit, really *)
      index: Tez.t [@printer Tez.pp];
      protected_index: Tez.t [@printer Tez.pp];
      target: FixedPoint.t [@printer FixedPoint.pp];
      drift': FixedPoint.t [@printer FixedPoint.pp];
      drift: FixedPoint.t [@printer FixedPoint.pp];
    }

  val show_parameters : parameters -> string
  val pp_parameters : Format.formatter -> parameters -> unit

  (* tez. To get tez/kit must multiply with q. *)
  val tz_minting : parameters -> Tez.t

  (* tez. To get tez/kit must multiply with q. *)
  val tz_liquidation : parameters -> Tez.t
end =
struct
  type parameters =
    { q : FixedPoint.t [@printer FixedPoint.pp]; (* 1/kit, really *)
      index: Tez.t [@printer Tez.pp];
      protected_index: Tez.t [@printer Tez.pp];
      target: FixedPoint.t [@printer FixedPoint.pp];
      drift': FixedPoint.t [@printer FixedPoint.pp];
      drift: FixedPoint.t [@printer FixedPoint.pp];
    }
  [@@deriving show]

  (* tez. To get tez/kit must multiply with q. *)
  let tz_minting (p: parameters) : Tez.t =
    max p.index p.protected_index

  (* tez. To get tez/kit must multiply with q. *)
  let tz_liquidation (p: parameters) : Tez.t =
    min p.index p.protected_index
end

