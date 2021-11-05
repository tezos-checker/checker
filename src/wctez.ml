(* FA2 wrapper for ctez to allow it to integrate with Checker *)
open Fa2Interface
open Fa2Ledger
open Fa12Interface
open Common
open Error

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

type wctez_state =
  { fa2_state : fa2_state;
    ctez_fa12_address : Ligo.address;
  }

(** Token id for wrapped ctez tokens. *)
let[@inline] wctez_token_id : fa2_token_id = Ligo.nat_from_literal "0n"

(*
(** Number of decimal digits for wctez tokens, identical to that for ctez. *)
(* NOTE: Currently unused. *)
let[@inline] wctez_token_decimal_digits = Ligo.nat_from_literal "6n"
*)

type wctez_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Mint of Ligo.nat
  | Redeem of Ligo.nat

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

let[@inline] ledger_issue_wctez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.nat) : fa2_state =
  ledger_issue (st, wctez_token_id, addr, amnt)

(* TODO: Might name this redeem instead of withdraw *)
let[@inline] ledger_withdraw_wctez_token
    (st, addr, amnt: fa2_state * Ligo.address * Ligo.nat) : fa2_state =
  ledger_withdraw (st, wctez_token_id, addr, amnt)

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

let[@inline] fa2_get_balance (st, owner, token_id: fa2_state * Ligo.address * fa2_token_id): Ligo.nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = wctez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
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

let[@inline] balance_of (state: wctez_state) (param: fa2_balance_of_param) : LigoOp.operation list * wctez_state =
  let _ = ensure_no_tez_given () in
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  ([op], state) (* unchanged state *)


let[@inline] fa2_run_transfer (initial_state, xs: wctez_state * fa2_transfer list) : wctez_state * LigoOp.operation list =
  let state =
    (* Fold over FA2 Transfers *)
    Ligo.List.fold_left
      (fun ((st, tx): (wctez_state * fa2_transfer)) ->
         let { from_ = from_; txs = txs; } = tx in
         (* Fold over the transactions in each FA2 Transfer *)
         Ligo.List.fold_left
           (fun ((st, x): (wctez_state * fa2_transfer_destination)) ->
              let { fa2_state = fa2_state; ctez_fa12_address = _;  } = st in (* deconstruct *)
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in

              if fa2_is_operator (fa2_state, !Ligo.Tezos.sender, from_, token_id)
              then
                (* Update FA2 Ledger *)
                let () = if token_id = wctez_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let fa2_state = ledger_withdraw (fa2_state, token_id, from_, amnt) in
                let fa2_state = ledger_issue (fa2_state, token_id, to_, amnt) in
                { st with fa2_state = fa2_state;} (* reconstruct *)
              else
                (failwith "FA2_NOT_OPERATOR" : wctez_state)
           )
           st
           txs
      )
      initial_state
      xs in
  (state, ([]: LigoOp.operation list))

let[@inline] transfer (state: wctez_state) (xs: fa2_transfer list) : LigoOp.operation list * wctez_state =
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

let[@inline] update_operators (state: wctez_state) (xs: fa2_update_operator list) : LigoOp.operation list * wctez_state =
  let _ = ensure_no_tez_given () in
  let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in
  (([]: LigoOp.operation list), state)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

let[@inline] mint (state: wctez_state) (amnt: Ligo.nat) : LigoOp.operation list * wctez_state =
  let { fa2_state = fa2_state; ctez_fa12_address = ctez_fa12_address; } = state in (* deconstruct *)
  (* Emit an operation to ctez transfering amnt of the caller's ctez to this contract *)
  let ctez_fa12_contract = match (LigoOp.Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : fa12_transfer Ligo.contract option) with
    | Some c -> c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : fa12_transfer Ligo.contract)
  in
  let op = LigoOp.Tezos.fa12_transfer_transaction
      {address_from = !Ligo.Tezos.sender; address_to = !Ligo.Tezos.self_address; value = amnt;}
      (Ligo.tez_from_literal "0mutez")
      ctez_fa12_contract
  in
  (* Issue the specified amount of tokens to the caller *)
  let fa2_state = ledger_issue_wctez_token (fa2_state, !Ligo.Tezos.sender, amnt) in
  let state = {state with fa2_state = fa2_state;} in
  ([op], state)

let[@inline] redeem (state: wctez_state) (amnt: Ligo.nat) : LigoOp.operation list * wctez_state =
  let { fa2_state = fa2_state; ctez_fa12_address = ctez_fa12_address; } = state in (* deconstruct *)
  (* Emit an operation to ctez transfering amnt of the caller's ctez to this contract *)
  let ctez_fa12_contract = match (LigoOp.Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : fa12_transfer Ligo.contract option) with
    | Some c -> c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : fa12_transfer Ligo.contract)
  in
  let op = LigoOp.Tezos.fa12_transfer_transaction
      {address_from = !Ligo.Tezos.self_address; address_to = !Ligo.Tezos.sender; value = amnt;}
      (Ligo.tez_from_literal "0mutez")
      ctez_fa12_contract
  in
  (* Remove the specified amount of tokens for the caller *)
  let fa2_state = ledger_withdraw_wctez_token (fa2_state, !Ligo.Tezos.sender, amnt) in
  let state = {state with fa2_state = fa2_state;} in
  ([op], state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

let main (op, state: wctez_params * wctez_state): LigoOp.operation list * wctez_state =
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Wrapper-specific entrypoints *)
  | Mint amnt -> mint state amnt
  | Redeem amnt -> redeem state amnt