open Kit
open Lqt
open Fa2Interface
open Fa2Ledger
open TokenMetadata

let[@inline] fa2_all_tokens : Ligo.nat list =
  [ kit_token_id; lqt_token_id ]

let[@inline] ensure_valid_fa2_token (n: fa2_token_id): unit =
  if n = kit_token_id || n = lqt_token_id
  then ()
  else failwith "FA2_TOKEN_UNDEFINED"

let[@inline] ledger_issue_kit
    (st, addr, amnt: fa2_state * Ligo.address * kit) : fa2_state =
  ledger_issue (st, kit_token_id, addr, kit_to_denomination_nat amnt)

let[@inline] ledger_withdraw_kit
    (st, addr, amnt: fa2_state * Ligo.address * kit) : fa2_state =
  ledger_withdraw (st, kit_token_id, addr, kit_to_denomination_nat amnt)

let[@inline] ledger_issue_then_withdraw_kit
    (st, addr, amnt_to_issue, amnt_to_withdraw: fa2_state * Ligo.address * kit * kit) : fa2_state =
  ledger_issue_then_withdraw (st, kit_token_id, addr, kit_to_denomination_nat amnt_to_issue, kit_to_denomination_nat amnt_to_withdraw)

let[@inline] ledger_issue_lqt
    (st, addr, amnt: fa2_state * Ligo.address * lqt) : fa2_state =
  ledger_issue (st, lqt_token_id, addr, lqt_to_denomination_nat amnt)

let[@inline] ledger_withdraw_lqt
    (st, addr, amnt: fa2_state * Ligo.address * lqt) : fa2_state =
  ledger_withdraw (st, lqt_token_id, addr, lqt_to_denomination_nat amnt)

let[@inline] fa2_get_balance (st, owner, token_id: fa2_state * Ligo.address * fa2_token_id): Ligo.nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = ensure_valid_fa2_token token_id in
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

let[@inline] fa2_run_transfer
    (st, xs: fa2_state * fa2_transfer list) : fa2_state =
  Ligo.List.fold_left
    (fun ((st, tx): fa2_state * fa2_transfer) ->
       let { from_ = from_; txs = txs; } = tx in

       Ligo.List.fold_left
         (fun ((st, x): fa2_state * fa2_transfer_destination) ->
            let { to_ = to_; token_id = token_id; amount = amnt; } = x in
            if fa2_is_operator (st, !Ligo.Tezos.sender, from_, token_id)
            then
              let () = ensure_valid_fa2_token token_id in
              let st = ledger_withdraw (st, token_id, from_, amnt) in
              let st = ledger_issue (st, token_id, to_, amnt) in
              st
            else
              (failwith "FA2_NOT_OPERATOR" : fa2_state)
         )
         st
         txs
    )
    st
    xs

(* BEGIN_OCAML *)
[@@@coverage off]

let fa2_get_token_balance (st: fa2_state) (token_id: fa2_token_id): Ligo.nat =
  Ligo.Big_map.bindings st.ledger
  |> List.filter (fun ((id, _owner), _amnt) -> id = token_id)
  |> List.map (fun ((_id, _owner), amnt) -> amnt)
  |> List.fold_left (fun x y -> Ligo.add_nat_nat x y) (Ligo.nat_from_literal "0n")

let fa2_get_total_kit_balance (st: fa2_state) : kit = kit_of_denomination (fa2_get_token_balance st kit_token_id)
let fa2_get_total_lqt_balance (st: fa2_state) : lqt = lqt_of_denomination (fa2_get_token_balance st lqt_token_id)

let get_kit_credits_from_fa2_state (st: fa2_state) : ((Ligo.address * Ligo.nat) list) =
  (* Note: for now let's just focus on the kit on the ledger. *)
  let kit_map =
    Ligo.Big_map.bindings st.ledger
    |> List.filter (fun ((id, _owner), _amnt) -> id = kit_token_id)
    |> List.map (fun ((_id, owner), amnt) -> (owner, amnt))
    |> List.stable_sort compare
  in
  kit_map

[@@@coverage on]
(* END_OCAML *)
