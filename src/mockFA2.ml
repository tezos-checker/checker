(* Just a plain FA2 contract in which we can make tokens out of thin air. *)
(* Mainly to be used in tests. *)

open Fa2Interface
open Fa2Ledger
open Common
open TokenMetadata

type mock_fa2_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Contract-specific entrypoints *)
  | Mint of Ligo.nat
  | Redeem of Ligo.nat

type mock_fa2_state =
  { fa2_state : fa2_state;
    total_token : Ligo.nat;
    metadata: (string, Ligo.bytes) Ligo.big_map;
  }

let[@inline] fa2_get_balance (st, owner, token_id: fa2_state * Ligo.address * fa2_token_id): Ligo.nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = if token_id = mock_fa2_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
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

let[@inline] balance_of (state: mock_fa2_state) (param: fa2_balance_of_param) : LigoOp.operation list * mock_fa2_state =
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  ([op], state) (* unchanged state *)

let[@inline] fa2_run_transfer (state, xs: fa2_state * fa2_transfer list) : fa2_state * LigoOp.operation list =
  let state =
    (* Fold over FA2 Transfers *)
    Ligo.List.fold_left
      (fun ((state, tx): (fa2_state * fa2_transfer)) ->
         let { from_ = from_; txs = txs; } = tx in
         (* Fold over the transactions in each FA2 Transfer *)
         Ligo.List.fold_left
           (fun ((state, x): (fa2_state * fa2_transfer_destination)) ->
              let { to_ = to_; token_id = token_id; amount = amnt; } = x in

              if fa2_is_operator (state, !Ligo.Tezos.sender, from_, token_id)
              then
                (* Update FA2 Ledger *)
                let () = if token_id = mock_fa2_token_id then () else failwith "FA2_TOKEN_UNDEFINED" in
                let state = ledger_withdraw (state, token_id, from_, amnt) in
                let state = ledger_issue (state, token_id, to_, amnt) in
                state
              else
                (failwith "FA2_NOT_OPERATOR" : fa2_state)
           )
           state
           txs
      )
      state
      xs in
  (state, ([]: LigoOp.operation list))

let[@inline] transfer (state: mock_fa2_state) (xs: fa2_transfer list) : LigoOp.operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state, ops = fa2_run_transfer (fa2_state, xs) in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
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

let[@inline] update_operators (state: mock_fa2_state) (xs: fa2_update_operator list) : LigoOp.operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = fa2_run_update_operators (fa2_state, xs) in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (([]: LigoOp.operation list), state)

let[@inline] mint (state: mock_fa2_state) (amnt: Ligo.nat) : LigoOp.operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = ledger_issue (fa2_state, mock_fa2_token_id, !Ligo.Tezos.sender, amnt) in
  let total_token = Ligo.add_nat_nat total_token amnt in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (([]: LigoOp.operation list), state)

let[@inline] redeem (state: mock_fa2_state) (amnt: Ligo.nat) : LigoOp.operation list * mock_fa2_state =
  let { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } = state in (* deconstruct *)
  let fa2_state = ledger_withdraw (fa2_state, mock_fa2_token_id, !Ligo.Tezos.sender, amnt) in
  let total_token =
    match Ligo.is_nat (Ligo.sub_nat_nat total_token amnt) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : Ligo.nat)
    | Some tt -> tt in
  let state = { fa2_state = fa2_state; total_token = total_token; metadata = metadata; } in (* reconstruct *)
  (([]: LigoOp.operation list), state)

let main (op, state: mock_fa2_params * mock_fa2_state): LigoOp.operation list * mock_fa2_state =
  let _ = ensure_no_tez_given () in
  match op with
  (* FA2 entrypoints *)
  | Balance_of param -> balance_of state param
  | Transfer xs -> transfer state xs
  | Update_operators xs -> update_operators state xs
  (* Contract-specific entrypoints *)
  | Mint amnt -> mint state amnt
  | Redeem amnt -> redeem state amnt

let view_get_balance ((owner, token_id), state: (Ligo.address * fa2_token_id) * mock_fa2_state) : Ligo.nat =
  fa2_get_balance (state.fa2_state, owner, token_id)

let view_total_supply (token_id, state: fa2_token_id * mock_fa2_state) : Ligo.nat =
  if token_id = mock_fa2_token_id then
    state.total_token
  else
    failwith "FA2_TOKEN_UNDEFINED"

let view_all_tokens ((), _state: unit * mock_fa2_state) : fa2_token_id list =
  [ mock_fa2_token_id ]

let view_is_operator ((owner, (operator, token_id)), state: (Ligo.address * (Ligo.address * fa2_token_id)) * mock_fa2_state) : bool =
  fa2_is_operator (state.fa2_state, operator, owner, token_id)
