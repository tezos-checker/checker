open Error
open Address
open Burrow
open Parameters
open Uniswap
open Tez
open Kit

(* TODO: Things to consider / action items:
 *
 * * What if compute_tez_to_auction returns something positive?
 *   => Create a kit UTXO for the burrow owner.
 *
 * * Implement auctioning logic.
 *
 * * George: Do we need >>= for type result?
*)

module AddressMap = Map.Make(Address)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

module Checker : sig
  type t =
    { burrows : Burrow.t AddressMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      (* TODO: add auction-related data here. *)
    }

  (** Perform housekeeping tasks on the contract state. This includes:
    * - Updating the parameters. TODO: We have to find a way to represent
    *   external inputs here; the inputs that Parameters.step requires.
    * - NOTE: Are there any other tasks to put in this list?
  *)
  val touch : t -> t

  (** Create and return a new burrow owned by the given owner, containing the
    * given tez as collateral, minus the creation deposit. Fail if the tez is
    * not enough to cover the creation deposit.
    * NOTE: Call Checker.touch too. *)
  val create_burrow : t -> owner:Address.t -> tez:Tez.t -> (Address.t * t, Error.error) result

  (** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
    * someone else owns the burrow, or if the burrow does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val deposit_tez : t -> owner:Address.t -> address:Address.t -> tez:Tez.t -> (t, Error.error) result

  (** Withdraw a non-negative amount of tez from a burrow. Fail if someone else
    * owns this burrow, if this action would overburrow it, or if the burrow
    * does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val withdraw_tez : t -> owner:Address.t -> address:Address.t -> tez:Tez.t -> (Tez.t * t, Error.error) result

  (** Mint kits from a specific burrow. Fail if there is not enough collateral,
    * if the burrow owner does not match, or if the burrow does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val mint_kit : t -> owner:Address.t -> address:Address.t -> amount:Kit.t -> (Kit.t * t, Error.error) result

  (** Deposit/burn a non-negative amount of kit in the burrow; return any
    * excess kit balance. Fail if the burrow owner does not match, or if the
    * burrow does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val burn_kit : t -> owner:Address.t -> address:Address.t -> amount:Kit.t -> (Kit.t * t, Error.error) result
end =
struct
  type t =
    { burrows : Burrow.t AddressMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
    }

  (* Utility function to give us burrow addresses *)
  let mk_next_burrow_address (burrows: Burrow.t AddressMap.t) : Address.t =
    match AddressMap.max_binding_opt burrows with
    | None -> Address.initial_address
    | Some (a, _) -> Address.next a

  let touch = failwith "Not implemented yet"

  let create_burrow (state: t) ~(owner:Address.t) ~(tez:Tez.t) =
    (* TODO: Call Checker.touch. *)
    let address = mk_next_burrow_address state.burrows in
    match Burrow.create state.parameters owner tez with
    | Ok burrow -> Ok (address, {state with burrows = AddressMap.add address burrow state.burrows})
    | Error err -> Error err

  let deposit_tez = failwith "Not implemented yet"

  let mint_kit = failwith "Not implemented yet"

  let withdraw_tez = failwith "Not implemented yet"

  let burn_kit = failwith "Not implemented yet"
end

