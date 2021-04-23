(*
 * INTERFACE
 *)

(*
Adapted from:
https://gitlab.com/tzip/tzip/-/blob/4b3c67aad5abbf04ec36caea4a1809e7b6e55bb8/proposals/tzip-12/fa2_interface.mligo
*)

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

type fa2_entry_points =
  | Transfer of fa2_transfer list
  | Balance_of of fa2_balance_of_param
  | Update_operators of fa2_update_operator list

(*
 TZIP-16 contract metadata storage field type.
 The contract storage MUST have a field
 `metadata : contract_metadata`
*)
type contract_metadata = (string, Ligo.bytes) Ligo.big_map

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

(*
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

let assert_valid_fa2_token (n: fa2_token_id): unit =
  if n == (Ligo.nat_from_literal "0n") || n == (Ligo.nat_from_literal "1n")
  then ()
  else failwith "FA2_TOKEN_UNDEFINED" (* FIXME: error message *)

type fa2_state =
  { ledger : (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map;
    operators : (Ligo.address * Ligo.address, unit) Ligo.big_map;
  }

let initial_fa2_state =
  { ledger = (Ligo.Big_map.empty: (fa2_token_id * Ligo.address, Ligo.nat) Ligo.big_map);
    operators = (Ligo.Big_map.empty: (Ligo.address * Ligo.address, unit) Ligo.big_map);
  }

let[@inline] fa2_run_update_operators
  (st, xs: fa2_state * fa2_update_operator list) : fa2_state =
  Ligo.List.fold_left
    (fun (st, x) ->
      match x with
      | Add_operator op ->
        (* The standard does not specify who is permitted to update operators. We restrict
           it only to the owner. *)
        if op.owner <> !Ligo.Tezos.sender
        then failwith "UNAUTHORIZED" (* FIXME: error message *)
        else
          { st  with
            operators =
              Ligo.Big_map.add
                (op.operator, op.owner)
                ()
                st.operators;
          }
      | Remove_operator op ->
        if op.owner <> !Ligo.Tezos.sender
        then failwith "UNAUTHORIZED" (* FIXME: error message *)
        else
          { st  with
            operators =
              Ligo.Big_map.remove
                (op.operator, op.owner)
                st.operators;
          }
    )
    st
    xs

(* FIXME
If one of the specified token_ids is not defined within the FA2 contract, the
entrypoint MUST fail with the error mnemonic "FA2_TOKEN_UNDEFINED".
*)
let[@inline] fa2_run_balance_of (st, xs: fa2_state * fa2_balance_of_request list)
  : fa2_balance_of_response list =
  let ledger = st.ledger in
  List.map
    (fun (req: fa2_balance_of_request) ->
      let key = (req.token_id, req.owner) in
      let balance = match Ligo.Big_map.find_opt key ledger with
                    | Some i -> i
                    | None -> Ligo.nat_from_literal "0n" in
      { request=req; balance = balance; }
    )
    xs

let[@inline] fa2_run_transfer
  (st, xs: fa2_state * fa2_transfer list) : fa2_state =
  Ligo.List.fold_left
    (fun ((st, tx): fa2_state * fa2_transfer) ->
      let from_ = tx.from_ in
      let is_authorized =
             from_ = !Ligo.Tezos.sender
                || Ligo.Big_map.mem (!Ligo.Tezos.sender, from_) st.operators in
      if not is_authorized
      then failwith "UNAUTHORIZED"
      else
        Ligo.List.fold_left
          (fun ((st, x): fa2_state * fa2_transfer_destination) ->
            let ledger = st.ledger in

            let from_key = (x.token_id, from_) in
            let to_key = (x.token_id, x.to_) in
            let amount = x.amount in

            let from_balance =
              match Ligo.Big_map.find_opt from_key ledger with
              | Some i -> i
              | None -> Ligo.nat_from_literal "0n" in

            let to_balance =
              match Ligo.Big_map.find_opt to_key ledger with
              | Some i -> i
              | None -> Ligo.nat_from_literal "0n" in

            let ledger =
              match Ligo.is_nat (Ligo.sub_nat_nat from_balance amount) with
              | None -> failwith "INSUFFICENT_BALANCE"
              | Some from_remaining -> Ligo.Big_map.add from_key from_remaining ledger in

            let ledger =
              Ligo.Big_map.add to_key (Ligo.add_nat_nat to_balance amount) ledger in

            { st with ledger = ledger }
          )
          st
          tx.txs
    )
    st
    xs

(* BEGIN_OCAML *)
type fa2_balance_of_response_list = fa2_balance_of_response list
[@@deriving show]
(* END_OCAML *)

