open Fa2Interface
open Fa2Ledger
open Common
open VaultTypes
open Error
open TokenMetadata

(* TODO: Expose FA2 metadata for tez_tokens. *)

(** Originate a vault contract with no delegate and zero tez. This way we can
  * originate vaults pretty easily, everytime we look one up: if it's not
  * there, just originate it now. *)
let[@inline] originate_vault (owner: Ligo.address) : LigoOp.operation * Ligo.address =
  LigoOp.Tezos.vault_create_contract
    (fun (p, storage : vault_parameter * vault_storage) ->
       match p with
       | Vault_set_delegate kho ->
         if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez" then
           (Ligo.failwith (Ligo.int_from_literal "-1") : LigoOp.operation list * vault_storage) (* unwanted tez *)
         else if !Ligo.Tezos.sender <> storage.owner then
           (Ligo.failwith (Ligo.int_from_literal "-2") : LigoOp.operation list * vault_storage) (* unauthorized *)
         else
           ([LigoOp.Tezos.set_delegate kho], storage)
       | Vault_receive_tez () ->
         (* NOTE: allowed from everyone. *)
         (([]: LigoOp.operation list), storage)
       | Vault_send_tez_to_vault tz_recipient ->
         if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez" then
           (Ligo.failwith (Ligo.int_from_literal "-3") : LigoOp.operation list * vault_storage) (* unwanted tez *)
         else if !Ligo.Tezos.sender <> storage.owner then
           (Ligo.failwith (Ligo.int_from_literal "-4") : LigoOp.operation list * vault_storage) (* unauthorized *)
         else
           let tz, recipient = tz_recipient in
           let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_receive_tez" recipient : unit Ligo.contract option) with
             | Some c -> LigoOp.Tezos.unit_transaction () tz c
             | None -> (Ligo.failwith (Ligo.int_from_literal "-8") : LigoOp.operation) in
           ([op], storage)
       | Vault_send_tez_to_contract tz_recipient ->
         if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez" then
           (Ligo.failwith (Ligo.int_from_literal "-5") : LigoOp.operation list * vault_storage) (* unwanted tez *)
         else if !Ligo.Tezos.sender <> storage.owner then
           (Ligo.failwith (Ligo.int_from_literal "-6") : LigoOp.operation list * vault_storage) (* unauthorized *)
         else
           let tz, recipient = tz_recipient in
           let op = match (LigoOp.Tezos.get_contract_opt recipient : unit Ligo.contract option) with
             | Some c -> LigoOp.Tezos.unit_transaction () tz c
             | None -> (Ligo.failwith (Ligo.int_from_literal "-7") : LigoOp.operation) in
           ([op], storage)
    )
    (None: Ligo.key_hash option)
    (Ligo.tez_from_literal "0mutez")
    {owner = owner}

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

(** Map from vault owner addresses to vault addresses. *)
type vault_map = (Ligo.address, Ligo.address) Ligo.big_map

type wtez_state =
  { fa2_state : fa2_state;
    vaults : vault_map;
  }

type wtez_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Deposit of unit (* TODO: not nice, having a unit type. Perhaps pass the tez as a number too? *)
  | Withdraw of Ligo.tez
  | Set_delegate of (Ligo.key_hash option)
  (* Internal entrypoints *)
  | Call_vault_receive_tez of (Ligo.address * Ligo.tez)
  | Call_vault_send_tez_to_contract of (Ligo.address * Ligo.tez * Ligo.address)
  | Call_vault_send_tez_to_vault of (Ligo.address * Ligo.tez * Ligo.address)
  | Call_vault_set_delegate of (Ligo.address * Ligo.key_hash option)

type vault_found = VaultFound | VaultNotFound

(** Find the address of the vault of given user, or originate it on the fly,
  * with Tezos.self_address as the owner. *)
let[@inline] find_vault_address_append (vaults: vault_map) (user: Ligo.address) (ops: LigoOp.operation list) : vault_found * LigoOp.operation list * vault_map * Ligo.address =
  match Ligo.Big_map.find_opt user vaults with
  | Some vault_address -> VaultFound, ops, vaults, vault_address
  | None ->
    let op, vault_address = originate_vault !Ligo.Tezos.self_address in
    VaultNotFound, (op :: ops), (Ligo.Big_map.update user (Some vault_address) vaults), vault_address

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

let[@inline] ledger_issue_tez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.tez) : fa2_state =
  ledger_issue (st, wtez_token_id, addr, tez_to_mutez_nat amnt)

let[@inline] ledger_withdraw_tez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.tez) : fa2_state =
  ledger_withdraw (st, wtez_token_id, addr, tez_to_mutez_nat amnt)

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

let[@inline] fa2_get_balance (st, owner, token_id: fa2_state * Ligo.address * fa2_token_id): Ligo.nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = wtez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
  get_fa2_ledger_value ledger key

let[@inline] fa2_run_balance_of (st, xs: fa2_state * fa2_balance_of_request list)
  : fa2_balance_of_response list =
  List.map
    (fun (req: fa2_balance_of_request) ->
       let { owner = owner; token_id = token_id; } : fa2_balance_of_request = req in
       let blnc = fa2_get_balance (st, owner, token_id) in
       { request=req; balance = blnc; }
    )
    xs

let[@inline] balance_of (state: wtez_state) (param: fa2_balance_of_param) : LigoOp.operation list * wtez_state =
  let _ = ensure_no_tez_given () in
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  ([op], state) (* unchanged state; no attempt to originate vaults either *)

let[@inline] reverse_op_list (ops: LigoOp.operation list) : LigoOp.operation list =
  Ligo.List.fold_left
    (fun ((ops, op) : LigoOp.operation list * LigoOp.operation) -> (op :: ops))
    ([] : LigoOp.operation list)
    ops

let[@inline] fa2_run_transfer (st, xs: wtez_state * fa2_transfer list) : wtez_state * LigoOp.operation list =
  let state, rev_ops =
    Ligo.List.fold_left
      (fun (((st, ops), tx): (wtez_state * LigoOp.operation list) * fa2_transfer) ->
         let { fa2_state = fa2_state; vaults = vaults; } = st in (* deconstruct *)
         let { from_ = from_; txs = txs; } = tx in

         (* Origination of the from_ vault, if needed *)
         let from_vault_found, ops, vaults, from_vault_address = find_vault_address_append vaults from_ ops in
         let st = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)

         Ligo.List.fold_left
           (fun (((st, ops), x): (wtez_state * LigoOp.operation list) * fa2_transfer_destination) ->
              let { fa2_state = fa2_state; vaults = vaults; } = st in (* deconstruct *)
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in

              if fa2_is_operator (fa2_state, !Ligo.Tezos.sender, from_, token_id)
              then
                (* FA2-related changes *)
                let () = if token_id = wtez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let fa2_state = ledger_withdraw (fa2_state, token_id, from_, amnt) in
                let fa2_state = ledger_issue (fa2_state, token_id, to_, amnt) in
                (* Origination of the to_ vault, if needed *)
                let _to_vault_found, ops, vaults, to_vault_address = find_vault_address_append vaults to_ ops in
                (* Instruct the from_ vault to send the actual tez to the to_ vault *)
                let op = match from_vault_found with
                  | VaultFound -> begin
                      (* Case 1: the vault for from_ exists already; make a direct call *)
                      match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez_to_vault" from_vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
                      | Some c -> LigoOp.Tezos.tez_address_transaction (tez_of_mutez_nat amnt, to_vault_address) (Ligo.tez_from_literal "0mutez") c
                      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSendTezToVault : LigoOp.operation)
                    end
                  | VaultNotFound -> begin
                      (* Case 2: the vault for from_ does not exist already; make an indirect call (more expensive) *)
                      match (LigoOp.Tezos.get_entrypoint_opt "%call_vault_send_tez_to_vault" !Ligo.Tezos.self_address : (Ligo.address * Ligo.tez * Ligo.address) Ligo.contract option) with
                      | Some c -> LigoOp.Tezos.address_tez_address_transaction (from_vault_address, tez_of_mutez_nat amnt, to_vault_address) (Ligo.tez_from_literal "0mutez") c
                      | None -> (Ligo.failwith error_GetEntrypointOptFailureCallVaultSendTezToVault : LigoOp.operation)
                    end in
                let ops = (op :: ops) in

                let st = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)
                (st, ops)
              else
                (failwith "FA2_NOT_OPERATOR" : wtez_state * LigoOp.operation list)
           )
           (st, ops)
           txs
      )
      (st, ([]: LigoOp.operation list))
      xs in
  (state, reverse_op_list rev_ops)

let[@inline] transfer (state: wtez_state) (xs: fa2_transfer list) : LigoOp.operation list * wtez_state =
  let _ = ensure_no_tez_given () in
  let state, ops = fa2_run_transfer (state, xs) in
  (ops, state)

let[@inline] fa2_run_update_operators
    (st, xs: fa2_state * fa2_update_operator list) : fa2_state =
  Ligo.List.fold_left
    (fun ((st : fa2_state), (x : fa2_update_operator)) ->
       match x with
       | Add_operator op ->
         let { owner = owner;
               operator = operator;
               token_id = token_id;
             } = op in
         (* The standard does not specify who is permitted to update operators. We restrict
            it only to the owner. *)
         if owner <> !Ligo.Tezos.sender
         then (failwith "FA2_NOT_OWNER" : fa2_state)
         else
           { st  with
             operators =
               Ligo.Big_map.add
                 (operator, owner, token_id)
                 ()
                 st.operators;
           }
       | Remove_operator op ->
         let { owner = owner;
               operator = operator;
               token_id = token_id;
             } = op in
         if owner <> !Ligo.Tezos.sender
         then (failwith "FA2_NOT_OWNER" : fa2_state)
         else
           { st  with
             operators =
               Ligo.Big_map.remove
                 (operator, owner, token_id)
                 st.operators;
           }
    )
    st
    xs

let[@inline] update_operators (state: wtez_state) (xs: fa2_update_operator list) : LigoOp.operation list * wtez_state =
  let _ = ensure_no_tez_given () in
  let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in
  (([]: LigoOp.operation list), state) (* no need to originate vaults *)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

let[@inline] deposit (state: wtez_state) (_: unit) : LigoOp.operation list * wtez_state =
  let { fa2_state = fa2_state; vaults = vaults; } = state in (* deconstruct *)
  let fa2_state = ledger_issue_tez_token (fa2_state, !Ligo.Tezos.sender, !Ligo.Tezos.amount) in
  match Ligo.Big_map.find_opt !Ligo.Tezos.sender vaults with
  | Some vault_address ->
    (* Case 1: The vault already exists. We can just deposit the tez into it
     * via a direct call. Cheaper than Case 2 below. *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_receive_tez" vault_address : unit Ligo.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c (* !!! *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultReceiveTez : LigoOp.operation) in
    let state = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)
    ([op], state)
  | None ->
    (* Case 2: The vault does not exist yet. We need to create it, and then
     * deposit the tez into it via an indirect call. This can be expensive the
     * first time. *)
    let origination, vault_address = originate_vault !Ligo.Tezos.self_address in
    let vaults = Ligo.Big_map.update !Ligo.Tezos.sender (Some vault_address) vaults in
    let state = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%call_vault_receive_tez" !Ligo.Tezos.self_address : (Ligo.address * Ligo.tez) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.address_tez_transaction (vault_address, !Ligo.Tezos.amount) (Ligo.tez_from_literal "0mutez") c (* !!! *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureCallVaultReceiveTez : LigoOp.operation) in
    ([origination; op], state)

let[@inline] withdraw (state: wtez_state) (amnt: Ligo.tez) : LigoOp.operation list * wtez_state =
  let { fa2_state = fa2_state; vaults = vaults; } = state in (* deconstruct *)
  let _ = ensure_no_tez_given () in
  let fa2_state = ledger_withdraw_tez_token (fa2_state, !Ligo.Tezos.sender, amnt) in
  match Ligo.Big_map.find_opt !Ligo.Tezos.sender vaults with
  | Some vault_address ->
    (* Case 1: The vault already exists. We can just instruct it to send the
     * actual tez to the owner via a direct call. Cheaper than Case 2 below. *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez_to_contract" vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (amnt, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSendTezToContract : LigoOp.operation) in
    let state = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)
    ([op], state)
  | None ->
    (* Case 2: The vault does not exist yet. We need to create it, and then
     * instruct it to send the actual tez to the owner via an indirect call.
     * This can be expensive the first time. *)
    let origination, vault_address = originate_vault !Ligo.Tezos.self_address in
    let vaults = Ligo.Big_map.update !Ligo.Tezos.sender (Some vault_address) vaults in
    let state = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%call_vault_send_tez_to_contract" !Ligo.Tezos.self_address : (Ligo.address * Ligo.tez * Ligo.address) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.address_tez_address_transaction (vault_address, amnt, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureCallVaultSendTezToContract : LigoOp.operation) in
    ([origination; op], state)

let[@inline] set_delegate (state: wtez_state) (kho: Ligo.key_hash option) : LigoOp.operation list * wtez_state =
  let { fa2_state = fa2_state; vaults = vaults; } = state in (* deconstruct *)
  let _ = ensure_no_tez_given () in
  match Ligo.Big_map.find_opt !Ligo.Tezos.sender vaults with
  | Some vault_address ->
    (* Case 1: The vault already exists. We can just instruct it to set its own
     * delegate via a direct call. Cheaper than Case 2 below. *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_set_delegate" vault_address : Ligo.key_hash option Ligo.contract option) with
      | Some c -> LigoOp.Tezos.opt_key_hash_transaction kho (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSetDelegate : LigoOp.operation) in
    ([op], state)
  | None ->
    (* Case 2: The vault does not exist yet. We need to create it, and then
     * instruct it to set its own delegate via an indirect call. This can be
     * expensive the first time. *)
    let origination, vault_address = originate_vault !Ligo.Tezos.self_address in
    let vaults = Ligo.Big_map.update !Ligo.Tezos.sender (Some vault_address) vaults in
    let state = { fa2_state = fa2_state; vaults = vaults; } in (* reconstruct *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%call_vault_set_delegate" !Ligo.Tezos.self_address : (Ligo.address * Ligo.key_hash option) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.address_opt_key_hash_transaction (vault_address, kho) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureCallVaultSetDelegate : LigoOp.operation) in
    ([origination; op], state)

(*****************************************************************************)
(**                      {1 INTERNAL ENTRYPOINTS}                            *)
(*****************************************************************************)

let[@inline] call_vault_receive_tez (state: wtez_state) (vault_address, amnt : Ligo.address * Ligo.tez) : LigoOp.operation list * wtez_state =
  if !Ligo.Tezos.sender <> !Ligo.Tezos.self_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * wtez_state)
  else
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_receive_tez" vault_address : unit Ligo.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () amnt c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultReceiveTez : LigoOp.operation) in
    ([op], state)

let[@inline] call_vault_send_tez_to_contract (state: wtez_state) (vault_address, amnt, recipient : Ligo.address * Ligo.tez * Ligo.address) : LigoOp.operation list * wtez_state =
  if !Ligo.Tezos.sender <> !Ligo.Tezos.self_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * wtez_state)
  else
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez_to_contract" vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (amnt, recipient) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSendTezToContract : LigoOp.operation) in
    ([op], state)

let[@inline] call_vault_send_tez_to_vault (state: wtez_state) (vault_address, amnt, recipient : Ligo.address * Ligo.tez * Ligo.address) : LigoOp.operation list * wtez_state =
  if !Ligo.Tezos.sender <> !Ligo.Tezos.self_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * wtez_state)
  else
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez_to_vault" vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (amnt, recipient) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSendTezToVault : LigoOp.operation) in
    ([op], state)

let[@inline] call_vault_set_delegate (state: wtez_state) (vault_address, kho : Ligo.address * Ligo.key_hash option) : LigoOp.operation list * wtez_state =
  if !Ligo.Tezos.sender <> !Ligo.Tezos.self_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * wtez_state)
  else
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_set_delegate" vault_address : Ligo.key_hash option Ligo.contract option) with
      | Some c -> LigoOp.Tezos.opt_key_hash_transaction kho (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSetDelegate : LigoOp.operation) in
    ([op], state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

let main (op, state: wtez_params * wtez_state): LigoOp.operation list * wtez_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Wrapper-specific entrypoints *)
  | Deposit () -> deposit state ()
  | Withdraw amnt -> withdraw state amnt
  | Set_delegate kho -> set_delegate state kho
  (* Internal entrypoints *)
  | Call_vault_receive_tez p -> call_vault_receive_tez state p
  | Call_vault_send_tez_to_contract p -> call_vault_send_tez_to_contract state p
  | Call_vault_send_tez_to_vault p -> call_vault_send_tez_to_vault state p
  | Call_vault_set_delegate p -> call_vault_set_delegate state p
