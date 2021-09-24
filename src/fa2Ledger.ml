open Fa2Interface

(*
Reference:
https://gitlab.com/tzip/tzip/-/blob/4b3c67aad5abbf04ec36caea4a1809e7b6e55bb8/proposals/tzip-12/tzip-12.md
*)

type fa2_state =
  { ledger : (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map;
    operators : ( Ligo.address (* operator *)
                  * Ligo.address (* owner *)
                  * fa2_token_id
                , unit
                ) Ligo.big_map;
  }

let initial_fa2_state =
  { ledger = (Ligo.Big_map.empty: (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map);
    operators = (Ligo.Big_map.empty: (Ligo.address * Ligo.address * fa2_token_id, unit) Ligo.big_map);
  }

let[@inline] get_fa2_ledger_value
    (ledger: (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map)
    (key: fa2_token_id * Ligo.address)
  : Ligo.nat =
  match Ligo.Big_map.find_opt key ledger with
  | Some i -> i
  | None -> Ligo.nat_from_literal "0n"

let set_fa2_ledger_value
    (ledger: (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map)
    (key: fa2_token_id * Ligo.address)
    (value: Ligo.nat)
  : (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map =
  if value = Ligo.nat_from_literal "0n"
  then Ligo.Big_map.remove key ledger
  else Ligo.Big_map.add key value ledger

let ledger_issue
    (st, tok, addr, amnt: fa2_state * fa2_token_id * Ligo.address * Ligo.nat) : fa2_state =
  let ledger = st.ledger in
  let key = (tok , addr) in
  let prev_balance = get_fa2_ledger_value ledger key in
  let new_balance = Ligo.add_nat_nat prev_balance amnt in
  let ledger = set_fa2_ledger_value ledger key new_balance in
  { st with ledger = ledger }

let ledger_withdraw
    (st, tok, addr, amnt: fa2_state * fa2_token_id * Ligo.address * Ligo.nat) : fa2_state =
  let ledger = st.ledger in
  let key = (tok, addr) in
  let prev_balance = get_fa2_ledger_value ledger key in
  let new_balance =
    match Ligo.is_nat (Ligo.sub_nat_nat prev_balance amnt) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : fa2_token_id)
    | Some b -> b in
  let ledger = set_fa2_ledger_value ledger key new_balance in
  { st with ledger = ledger }

(* NOTE: Checker-specific, this one. Needed to save on gas costs. *)
let[@inline] ledger_issue_then_withdraw
    (st, tok, addr, amnt_to_issue, amnt_to_withdraw: fa2_state * fa2_token_id * Ligo.address * Ligo.nat * Ligo.nat) : fa2_state =
  let ledger = st.ledger in
  let key = (tok , addr) in
  let balance_ = get_fa2_ledger_value ledger key in
  (* ISSUE *)
  let balance_ = Ligo.add_nat_nat balance_ amnt_to_issue in
  (* WITHDRAW *)
  let balance_ =
    match Ligo.is_nat (Ligo.sub_nat_nat balance_ amnt_to_withdraw) with
    | None -> (failwith "FA2_INSUFFICIENT_BALANCE" : fa2_token_id)
    | Some b -> b in
  (* UPDATE STATE *)
  let ledger = set_fa2_ledger_value ledger key balance_ in
  { st with ledger = ledger }

let[@inline] fa2_is_operator (st, operator, owner, token_id: fa2_state * Ligo.address * Ligo.address * fa2_token_id) =
  owner = operator || Ligo.Big_map.mem (operator, owner, token_id) st.operators

