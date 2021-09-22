open Fa2Interface
open Error
open Common

(* TODO: I think we must be more eager to create vaults if they don't already
 * exist. Even with plain FA2 entrypoints we might need to transfer some tez
 * from vault to vault (e.g., due to a %transfer), which would fail if there
 * were no vault.
*)

(* TODO: Do we want this to become "2n" and join the other two in fa2Interface.ml? *)
let[@inline] tez_token_id = Ligo.nat_from_literal "0n"

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
  | Deposit of unit (* TODO: not nice, having a unit type. Perhaps pass the tez as a number too? *)
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
  | Deposit () ->
    (* 1. Update the balance on the ledger *)
    let amnt = Ligo.div_tez_tez !Ligo.Tezos.amount (Ligo.tez_from_literal "1mutez") in
    let state_fa2_state = ledger_issue (state.fa2_state, tez_token_id, !Ligo.Tezos.sender, amnt) in
    let state = { state with fa2_state = state_fa2_state } in
    (* 2. Create a vault, if it does not exist already. *)
    let vault_address, state =
      match Ligo.Big_map.find_opt !Ligo.Tezos.sender state.vaults with
      | None -> failwith "not implemented yet" (* TODO: Create the vault here and update the maps *)
      | Some vault_address -> vault_address, state in
    (* 3. Transfer the actual tez to the vault of the sender. *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%receive_tez" vault_address : unit Ligo.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
      | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *)
    in
    ([op], state)
  | Withdraw _amnt ->
    failwith "not implemented yet"
  | Set_delegate _kho ->
    failwith "not implemented yet"
