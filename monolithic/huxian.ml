open Address
open Burrow
open Parameters
open Uniswap

(* TODO: Things to consider / action items:
 *
 * * What if compute_tez_to_auction returns something positive?
 *   => Create a kit UTXO for the burrow owner.
 *
 * * Implement auctioning logic.
*)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

type checker =
  { burrows : Burrow.t Map.Make(Address).t;
    uniswap : Uniswap.t;
    parameters : Parameters.t;
  }

(* ************************************************************************* *)
(* ************************************************************************* *)

