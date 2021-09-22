open Fa2Interface
open Error
open Common

type vault_map = (Ligo.address, Ligo.address) Ligo.big_map

type tez_wrapper_state =
  { fa2_state : fa2_state;
    vaults : vault_map;
  }

type tez_wrapper_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Deposit of Ligo.tez
  | Withdraw of Ligo.tez
  | Set_delegate of (Ligo.key_hash option)

(** Find the address of the vault of given user, or fail. *)
let[@inline] find_vault_address (vaults: vault_map) (user: Ligo.address) : Ligo.address =
  match Ligo.Big_map.find_opt user vaults with
  | None -> (Ligo.failwith error_NonExistentVault : Ligo.address)
  | Some vault_address -> vault_address

let wrapper_main (op, state: tez_wrapper_params * tez_wrapper_state): LigoOp.operation list * tez_wrapper_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param ->
    let _ = ensure_no_tez_given () in
    let { requests = requests; callback = callback; } = param in
    let response = fa2_run_balance_of (state.fa2_state, requests) in
    let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
    ([op], state)
  | Transfer xs ->
    (* TODO: fa2_transfers always come in a list, so it's not very easy to emit
     * all the additional operations for moving tez around (from vault to
     * vault) at the same time.  Either we should do a second pass to transfer
     * the tez amounts, or we have to change our current abstractions. *)
    let _ = ensure_no_tez_given () in
    let state = { state with fa2_state = fa2_run_transfer (state.fa2_state, xs) } in
    (* TODO: Move tez from vault to vault here *)
    (([]: LigoOp.operation list), state)
  | Update_operators xs ->
    let _ = ensure_no_tez_given () in
    let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in
    (([]: LigoOp.operation list), state)
  (* Wrapper-specific entrypoints *)
  | Deposit _amnt ->
    failwith "not implemented yet"
  | Withdraw _amnt ->
    failwith "not implemented yet"
  | Set_delegate _kho ->
    failwith "not implemented yet"
