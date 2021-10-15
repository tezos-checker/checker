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
[@@deriving show]

type fa2_transfer =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  {
    from_ : Ligo.address;
    txs : fa2_transfer_destination list;
  }
[@@deriving show]

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

(* Each FA2-compliant contract should implement the following entrypoints:
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
[@@@coverage on]

(* BEGIN_OCAML *)
[@@@coverage off]

type fa2_balance_of_response_list = fa2_balance_of_response list
[@@deriving show]

type fa2_transfer_list = fa2_transfer list
[@@deriving show]

[@@@coverage on]
(* END_OCAML *)
