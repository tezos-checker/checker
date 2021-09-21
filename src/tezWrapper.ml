open Fa2Interface

(* TODO: fa2_transfers always come in a list, so it's not very easy to emit all
 * the additional operations for moving tez around at the same time. Either we
 * should do a second pass to transfer the tez amounts, or we have to change
 * our current abstractions. *)

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
(* TODO: Add error_NonExistentVault to error.ml. *)
let[@inline] find_vault_address (vaults: vault_map) (user: Ligo.address) : Ligo.address =
  match Ligo.Big_map.find_opt user vaults with
  | None -> (Ligo.failwith error_NonExistentVault : Ligo.address)
  | Some vault_address -> vault_address


let wrapper_main (op, state: tez_wrapper_params * tez_wrapper_state): LigoOp.operation list * state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param ->
    failwith "not implemented yet"
  | Transfer xs ->
    failwith "not implemented yet"
  | Update_operators xs ->
    failwith "not implemented yet"
  (* Wrapper-specific entrypoints *)
  | Deposit amnt ->
    failwith "not implemented yet"
  | Withdraw amnt ->
    failwith "not implemented yet"
  | Set_delegate kho ->
    failwith "not implemented yet"


