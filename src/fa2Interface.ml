open Kit
open Lqt

(*
 * INTERFACE
 *)

(*
Adapted from:
https://gitlab.com/tzip/tzip/-/blob/4b3c67aad5abbf04ec36caea4a1809e7b6e55bb8/proposals/tzip-12/fa2_interface.mligo

Currently we only implement the absolute requirements of the interface. We also:
  * TODO should consider implementing permission policies
*)

[@@@coverage off]

type fa2_token_id = Ligo.nat
[@@deriving show]

type fa2_transfer_destination =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    to_ : Ligo.address;
    token_id : fa2_token_id;
    amount : Ligo.nat;
  }

type fa2_transfer =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    from_ : Ligo.address;
    txs : fa2_transfer_destination list;
  }

type fa2_balance_of_request =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    owner : Ligo.address;
    token_id : fa2_token_id;
  }
[@@deriving show]

type fa2_balance_of_response =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    request : fa2_balance_of_request;
    balance : Ligo.nat;
  }
[@@deriving show]

type fa2_balance_of_param =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    requests : fa2_balance_of_request list;
    callback : (fa2_balance_of_response list) Ligo.contract;
  }

type fa2_operator_param =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    owner : Ligo.address;
    operator : Ligo.address;
    token_id: fa2_token_id;
  }

type fa2_update_operator =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  | Add_operator of fa2_operator_param
  | Remove_operator of fa2_operator_param

type token_metadata =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    token_id : fa2_token_id;
    token_info : (string, Ligo.bytes) Ligo.map;
  }

(*
One of the options to make token metadata discoverable is to declare
`token_metadata : token_metadata_storage` field inside the FA2 contract storage
*)
type token_metadata_storage = (fa2_token_id, token_metadata) Ligo.big_map

(**
   Optional type to define view entry point to expose token_metadata on chain or
   as an external view
*)
type token_metadata_param =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    token_ids : fa2_token_id list;
    handler : (token_metadata list) -> unit;
  }

(* Each FA2-compliant contrat should implement the following entrypoints:
   type fa2_entry_points =
   | Transfer of fa2_transfer list
   | Balance_of of fa2_balance_of_param
   | Update_operators of fa2_update_operator list
*)

(*
 TZIP-16 contract metadata storage field type.
 The contract storage MUST have a field
 `metadata : contract_metadata`
*)
type contract_metadata = (string, Ligo.bytes) Ligo.big_map

(*
(* FA2 hooks interface *)

type fa2_transfer_destination_descriptor =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    to_ : Ligo.address option;
    token_id : fa2_token_id;
    amount : Ligo.nat;
  }

type fa2_transfer_descriptor =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    from_ : Ligo.address option;
    txs : fa2_transfer_destination_descriptor list
  }

type transfer_descriptor_param =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    batch : fa2_transfer_descriptor list;
    operator : Ligo.address;
  }

Entrypoints for sender/receiver hooks

type fa2_token_receiver =
  ...
  | Tokens_received of transfer_descriptor_param

type fa2_token_sender =
  ...
  | Tokens_sent of transfer_descriptor_param
*)


(*
 * IMPLEMENTATION
 *)

(*
Reference:
https://gitlab.com/tzip/tzip/-/blob/4b3c67aad5abbf04ec36caea4a1809e7b6e55bb8/proposals/tzip-12/tzip-12.md
*)

[@@@coverage on]

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

let[@inline] kit_token_id : fa2_token_id = Ligo.nat_from_literal "0n"
let[@inline] lqt_token_id : fa2_token_id = Ligo.nat_from_literal "1n"

let ensure_valid_fa2_token (n: fa2_token_id): unit =
  if n = kit_token_id || n = lqt_token_id
  then ()
  else failwith "FA2_TOKEN_UNDEFINED"

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

let[@inline] fa2_get_balance (st, owner, token_id: fa2_state * Ligo.address * fa2_token_id): Ligo.nat =
  let ledger = st.ledger in
  let key = (token_id, owner) in
  let () = ensure_valid_fa2_token token_id in
  get_fa2_ledger_value ledger key

let[@inline] fa2_all_tokens : Ligo.nat list =
  [ kit_token_id; lqt_token_id ]

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

(* BEGIN_OCAML *)
[@@@coverage off]

type fa2_balance_of_response_list = fa2_balance_of_response list
[@@deriving show]

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
