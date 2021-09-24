open Fa2Interface
open Common
open VaultTypes
open Error

(* TODO: Expose FA2 metadata for tez_tokens. *)

(** Originate a vault contract with no delegate and zero tez. This way we can
  * originate vaults pretty easily, everytime we look one up: if it's not
  * there, just originate it now. *)
(* TODO: Investigate whether we should or should not inline this one. *)
let originate_vault (owner: Ligo.address) : LigoOp.operation * Ligo.address =
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
         (* TODO: allowed from everyone? *)
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

type tez_wrapper_state =
  { fa2_state : fa2_state;
    vaults : vault_map;
  }

(** Token id for tez tokens. Note that this could have been 0n instead, but I'd
  * like to think of checker as a family of contracts, in which case I'd like
  * all valid tokens to be distinct. Either way, the choice of token_id is
  * arbitrary, so 2n is just as good as 0n. *)
let[@inline] tez_token_id : fa2_token_id = Ligo.nat_from_literal "2n"

(*
(** Number of decimal digits for tez tokens, identical to that for tez. *)
(* NOTE: Currently unused. *)
let[@inline] tez_token_decimal_digits = Ligo.nat_from_literal "6n"
*)

type tez_wrapper_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Deposit of unit (* TODO: not nice, having a unit type. Perhaps pass the tez as a number too? *)
  | Withdraw of Ligo.tez
  | Set_delegate of (Ligo.key_hash option)

(** Find the address of the vault of given user, or originate it on the fly,
  * with Tezos.self_address as the owner. *)
let[@inline] find_vault_address (state: tez_wrapper_state) (user: Ligo.address) : LigoOp.operation option * tez_wrapper_state * Ligo.address =
  match Ligo.Big_map.find_opt user state.vaults with
  | Some vault_address -> (None: LigoOp.operation option), state, vault_address
  | None ->
    let op, vault_address = originate_vault !Ligo.Tezos.self_address in
    Some op, {state with vaults = Ligo.Big_map.update user (Some vault_address) state.vaults}, vault_address

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

let[@inline] ledger_issue_tez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.tez) : fa2_state =
  ledger_issue (st, tez_token_id, addr, tez_to_mutez_nat amnt)

let[@inline] ledger_withdraw_tez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.tez) : fa2_state =
  ledger_withdraw (st, tez_token_id, addr, tez_to_mutez_nat amnt)

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

(* TODO: START COPY-PASTE AND MANUALLY EDIT *)
let[@inline] tez_wrapper_fa2_get_balance (st, owner, token_id: fa2_state * Ligo.address * fa2_token_id): Ligo.nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = tez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
  get_fa2_ledger_value ledger key

let[@inline] tez_wrapper_fa2_run_balance_of (st, xs: fa2_state * fa2_balance_of_request list)
  : fa2_balance_of_response list =
  List.map
    (fun (req: fa2_balance_of_request) ->
       let { owner = owner; token_id = token_id; } : fa2_balance_of_request = req in
       let blnc = tez_wrapper_fa2_get_balance (st, owner, token_id) in
       { request=req; balance = blnc; }
    )
    xs
(* TODO: END COPY-PASTE AND MANUALLY EDIT *)

let[@inline] balance_of (state: tez_wrapper_state) (param: fa2_balance_of_param) : LigoOp.operation list * tez_wrapper_state =
  let _ = ensure_no_tez_given () in
  let { requests = requests; callback = callback; } = param in
  let response = tez_wrapper_fa2_run_balance_of (state.fa2_state, requests) in (* TODO: Uses specialized tez_wrapper_fa2_run_balance_of *)
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  ([op], state) (* unchanged state; no attempt to originate vaults either *)

(* TODO: START COPY-PASTE AND MANUALLY EDIT *)
let[@inline] reverse_op_list (ops: LigoOp.operation list) : LigoOp.operation list =
  Ligo.List.fold_left
    (fun ((ops, op) : LigoOp.operation list * LigoOp.operation) -> (op :: ops))
    ([] : LigoOp.operation list)
    ops

let[@inline] tez_wrapper_fa2_run_transfer (st, xs: tez_wrapper_state * fa2_transfer list) : tez_wrapper_state * LigoOp.operation list =
  let state, rev_ops =
    Ligo.List.fold_left
      (fun (((st, ops), tx): (tez_wrapper_state * LigoOp.operation list) * fa2_transfer) ->
         let { from_ = from_; txs = txs; } = tx in

         (* Origination of the from_ vault, if needed *)
         let op_opt, st, from_vault_address = find_vault_address st from_ in
         let ops = match op_opt with
           | None -> ops (* vault is already originated *)
           | Some origination -> (origination :: ops) in (* originate now *)

         (* TODO: Do we really need to restrict the to_ and from_ here? *)
         Ligo.List.fold_left
           (fun (((st, ops), x): (tez_wrapper_state * LigoOp.operation list) * fa2_transfer_destination) ->
              let fa2_st = st.fa2_state in
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in
              if fa2_is_operator (fa2_st, !Ligo.Tezos.sender, from_, token_id)
              then
                (* FA2-related changes *)
                let () = if token_id = tez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let fa2_st = ledger_withdraw (fa2_st, token_id, from_, amnt) in
                let fa2_st = ledger_issue (fa2_st, token_id, to_, amnt) in
                let st = { st with fa2_state = fa2_st } in
                (* Origination of the to_ vault, if needed *)
                let op_opt, st, to_vault_address = find_vault_address st to_ in
                let ops = match op_opt with
                  | None -> ops (* vault is already originated *)
                  | Some origination -> (origination :: ops) in (* originate now *)
                (* Instruct the from_ vault to send the actual tez to the to_ vault *)
                let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez_to_vault" from_vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
                  | Some c -> LigoOp.Tezos.tez_address_transaction (tez_of_mutez_nat amnt, to_vault_address) (Ligo.tez_from_literal "0mutez") c
                  | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSendTezToVault : LigoOp.operation) in
                let ops = (op :: ops) in

                (st, ops)
              else
                (failwith "FA2_NOT_OPERATOR" : tez_wrapper_state * LigoOp.operation list)
           )
           (st, ops)
           txs
      )
      (st, ([]: LigoOp.operation list))
      xs in
  (state, reverse_op_list rev_ops)
(* TODO: END COPY-PASTE AND MANUALLY EDIT *)

let[@inline] transfer (state: tez_wrapper_state) (xs: fa2_transfer list) : LigoOp.operation list * tez_wrapper_state =
  let _ = ensure_no_tez_given () in
  let state, ops = tez_wrapper_fa2_run_transfer (state, xs) in (* TODO: Uses specialized tez_wrapper_fa2_run_transfer *)
  (ops, state)

let[@inline] update_operators (state: tez_wrapper_state) (xs: fa2_update_operator list) : LigoOp.operation list * tez_wrapper_state =
  let _ = ensure_no_tez_given () in
  let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in (* NOTE: No need for specialized calls, since the spec does not require checking the token_ids here. *)
  (([]: LigoOp.operation list), state) (* no need to originate vaults *)

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
    | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultReceiveTez : LigoOp.operation) in
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
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_send_tez_to_contract" vault_address : (Ligo.tez * Ligo.address) Ligo.contract option) with
    | Some c -> LigoOp.Tezos.tez_address_transaction (amnt, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSendTezToContract : LigoOp.operation) in
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
    | None -> (Ligo.failwith error_GetEntrypointOptFailureVaultSetDelegate : LigoOp.operation) in
  match op_opt with
  | None -> ([op], state)
  | Some origination -> ([origination; op], state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

let main (op, state: tez_wrapper_params * tez_wrapper_state): LigoOp.operation list * tez_wrapper_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Wrapper-specific entrypoints *)
  | Deposit () -> deposit state ()
  | Withdraw amnt -> withdraw state amnt
  | Set_delegate kho -> set_delegate state kho
