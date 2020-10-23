open FixedPoint
open Tez

(* ************************************************************************* *)
(*                               Parameters                                   *)
(* ************************************************************************* *)
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

