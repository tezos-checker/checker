open OUnit2

(*
Properties we expect to hold
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
General
* is_liquidatable ==> is_overburrowed (not the other way around)
* No interaction with the burrow has any effect if it's inactive.
* Liquidation of an active burrow with collateral < creation_deposit should "close" it

If a liquidation was deemed Unnecessary:
* is_liquidatable is false for the given burrow

If a liquidation was deemed Partial:
* is_liquidatable is true for the given burrow
* is_optimistically_overburrowed is false for the resulting burrow

If a liquidation was deemed Complete:
* is_liquidatable is true for the given burrow
* is_optimistically_overburrowed is true for the resulting burrow
* the resulting burrow has no collateral

If a liquidation was deemed Close:
* is_liquidatable is true for the given burrow
* the resulting burrow is zeroed and inactive
*)

let suite =
  "LiquidationTests" >::: [
    (* TODO: Add tests here *)
  ]
