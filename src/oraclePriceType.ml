open CheckerTypes
open Error
open Common
open FixedPoint
open Tok

(* FIXME: We should be able to change this into (Ligo.nat * Ligo.nat) at build
 * time when the tracking type is set to token. *)
type oracle_price_type = Ligo.nat

(* FIXME: This should also be tracking-type-dependent. Not only the type, but also the entrypoint name. *)
let[@inline] get_oracle_entrypoint (external_contracts: external_contracts): (oracle_price_type Ligo.contract) Ligo.contract =
  match (LigoOp.Tezos.get_entrypoint_opt "%getPrice" external_contracts.oracle: (oracle_price_type Ligo.contract) Ligo.contract option) with
  | Some c -> c
  | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint: (oracle_price_type Ligo.contract) Ligo.contract)

let[@inline] call_the_oracle (state_external_contracts: external_contracts) : LigoOp.operation =
  let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receive_price" !Ligo.Tezos.self_address : (oracle_price_type Ligo.contract) option) with
    | Some cb -> cb
    | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : oracle_price_type Ligo.contract) in
  (* FIXME: nat won't do here. we need to be able to switch between nat and (nat * nat) *)
  LigoOp.Tezos.nat_contract_transaction
    cb
    (Ligo.tez_from_literal "0mutez")
    (get_oracle_entrypoint state_external_contracts)

(* FIXME: this also needs a type-dependent implementation to work *)
let[@inline] oracle_price_to_fixedpoint (oracle_price: oracle_price_type) : fixedpoint =
  fixedpoint_of_ratio_floor (make_ratio (Ligo.int oracle_price) tok_scaling_factor_int)
