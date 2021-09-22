open Fa2Interface
open Common
open VaultTypes

(*****************************************************************************)
(**                             {1 VAULTS}                                   *)
(*****************************************************************************)

(* NOTE: We always start with no delegate and no tez. Why? So that we can fire
 * up vaults on the fly. The only thing we need is the address of the "owner".
*)

let[@inline] originate_vault (owner: Ligo.address) : LigoOp.operation * Ligo.address =
  LigoOp.Tezos.vault_create_contract
    (fun (p, storage : vault_parameter * vault_storage) ->
       match p with
       | Vault_set_delegate kho ->
         let _ = ensure_no_tez_given () in
         if !Ligo.Tezos.sender <> storage.owner then
           failwith "unauthorized" (* TODO: Add new error in error.ml *)
         else
           ([LigoOp.Tezos.set_delegate kho], storage)
       | Vault_receive_tez () ->
         (* TODO: allowed from everyone? *)
         ([], storage)
       | Vault_send_tez tz_recipient ->
         let _ = ensure_no_tez_given () in
         if !Ligo.Tezos.sender <> storage.owner then
           failwith "unauthorized" (* TODO: Add new error in error.ml *)
         else
           let tz, recipient = tz_recipient in
           (* TODO: Consider whether we want to have this behavior conflated into
            * Vault_send_tez, or if it's better to have two separate entrypoints. *)
           let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_receive_tez" recipient : unit Ligo.contract option) with
             | Some c ->
               LigoOp.Tezos.unit_transaction () tz c
             | None ->
               begin match (LigoOp.Tezos.get_contract_opt recipient : unit Ligo.contract option) with
                 | Some c -> LigoOp.Tezos.unit_transaction () tz c
                 | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *)
               end
           in
           ([op], storage)
    )
    (None: Ligo.key_hash option)
    (Ligo.tez_from_literal "0mutea")
    {owner = owner}

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

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
  | Withdraw of Ligo.tez (* TODO: the docs said nat, but I think it should be tez *)
  | Set_delegate of (Ligo.key_hash option)

(** Find the address of the vault of given user, or originate it on the fly,
  * with Tezos.self_address as the owner. *)
let[@inline] find_vault_address (state: tez_wrapper_state) (user: Ligo.address) : LigoOp.operation option * tez_wrapper_state * Ligo.address =
  match Ligo.Big_map.find_opt user state.vaults with
  | Some vault_address -> None, state, vault_address
  | None ->
    let op, vault_address = originate_vault !Ligo.Tezos.self_address in
    Some op, {state with vaults = Ligo.Big_map.update user (Some vault_address) state.vaults}, vault_address

(* TODO: I think we must be more eager to create vaults if they don't already
 * exist. Even with plain FA2 entrypoints we might need to transfer some tez
 * from vault to vault (e.g., due to a %transfer), which would fail if there
 * were no vault.
*)

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

(* TODO: For the FA2 entrypoints we internally check that the tokens transfered
 * are valid. We have to include tez_tokens in the list or deal with this in a
 * different way. I think the safest and easiest way is to use "2n" for tez
 * tokens and included them in the list. This would implicitly make
 * fa2_run_transfer behave correctly too (since it depends on
 * ensure_valid_fa2_token internally). Also we'll need FA2 metadata for
 * tez_tokens.
*)

(* TODO: Do we want this to become "2n" and join the other two in fa2Interface.ml? *)
let[@inline] tez_token_id = Ligo.nat_from_literal "0n"

let[@inline] ledger_issue_tez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.tez) : fa2_state =
  ledger_issue (st, tez_token_id, addr, tez_to_mutez_nat amnt)

let[@inline] ledger_withdraw_tez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.tez) : fa2_state =
  ledger_withdraw (st, kit_token_id, addr, tez_to_mutez_nat amnt)

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

let[@inline] balance_of (state: tez_wrapper_state) (param: fa2_balance_of_param) : LigoOp.operation list * tez_wrapper_state =
  let _ = ensure_no_tez_given () in
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  ([op], state)

let[@inline] transfer (state: tez_wrapper_state) (xs: fa2_transfer list) : LigoOp.operation list * tez_wrapper_state =
  (* TODO: fa2_transfers always come in a list, so it's not very easy to emit
   * all the additional operations for moving tez around (from vault to
   * vault) at the same time.  Either we should do a second pass to transfer
   * the tez amounts, or we have to change our current abstractions. *)
  let _ = ensure_no_tez_given () in
  let state = { state with fa2_state = fa2_run_transfer (state.fa2_state, xs) } in
  (* TODO: Move tez from vault to vault here *)
  (([]: LigoOp.operation list), state)

let[@inline] update_operators (state: tez_wrapper_state) (xs: fa2_update_operator list) : LigoOp.operation list * tez_wrapper_state =
  let _ = ensure_no_tez_given () in
  let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in
  (([]: LigoOp.operation list), state)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

let[@inline] deposit (state: tez_wrapper_state) (_: unit) : LigoOp.operation list * tez_wrapper_state =
  (* 1. Update the balance on the ledger *)
  let state_fa2_state = ledger_issue_tez_token (state.fa2_state, !Ligo.Tezos.sender, !Ligo.Tezos.amount) in
  let state = { state with fa2_state = state_fa2_state } in
  (* 2. Create a vault, if it does not exist already, and update the map. *)
  let op_opt, state, vault_address = find_vault_address state !Ligo.Tezos.sender in
  (* 3. Transfer the actual tez to the vault of the sender. *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_receive_tez" vault_address : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
    | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *) in
  match op_opt with
  | None -> ([op], state)
  | Some origination -> ([origination; op], state)

let[@inline] withdraw (state: tez_wrapper_state) (amnt: Ligo.tez) : LigoOp.operation list * tez_wrapper_state =
  (* 1. Ensure no tez given *)
  let _ = ensure_no_tez_given () in
  (* 2. Reduce the balance of the tez owner *)
  let state_fa2_state = ledger_withdraw_tez_token (state.fa2_state, !Ligo.Tezos.sender, amnt) in
  let state = { state with fa2_state = state_fa2_state; } in
  (* 3. Create a vault, if it does not exist already, and update the map. *)
  let op_opt, state, vault_address = find_vault_address state !Ligo.Tezos.sender in
  (* 4. Instruct the vault to send the actual tez to the owner *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez" vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
    | Some c -> LigoOp.Tezos.tez_address_transaction (amnt, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
    | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *) in
  match op_opt with
  | None -> ([op], state)
  | Some origination -> ([origination; op], state)

let[@inline] set_delegate (state: tez_wrapper_state) (kho: Ligo.key_hash option) : LigoOp.operation list * tez_wrapper_state =
  (* 1. Ensure no tez given *)
  let _ = ensure_no_tez_given () in
  (* 2. Create a vault, if it does not exist already, and update the map. *)
  let op_opt, state, vault_address = find_vault_address state !Ligo.Tezos.sender in
  (* 3. Instruct the vault to set its own delegate *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_set_delegate" vault_address : Ligo.key_hash option Ligo.contract option) with
    | Some c -> LigoOp.Tezos.opt_key_hash_transaction kho (Ligo.tez_from_literal "0mutez") c
    | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *) in
  match op_opt with
  | None -> ([op], state)
  | Some origination -> ([origination; op], state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

let tez_wrapper_main (op, state: tez_wrapper_params * tez_wrapper_state): LigoOp.operation list * tez_wrapper_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Wrapper-specific entrypoints *)
  | Deposit () -> deposit state ()
  | Withdraw amnt -> withdraw state amnt
  | Set_delegate kho -> set_delegate state kho
