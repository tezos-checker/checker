open Ligo
open TokenTypes

(* contract *)

type 'parameter contract = Contract of address

type 'parameter transaction_value = (* GADT *)
  | UnitTransactionValue : unit transaction_value
  | KitTransactionValue : kit_token_content ticket -> kit_token_content ticket transaction_value
  | LqtTransactionValue : liquidity_token_content ticket -> liquidity_token_content ticket transaction_value
  | DaBidTransactionValue : delegation_auction_bid ticket -> delegation_auction_bid ticket transaction_value
  | LaBidTransactionValue : liquidation_auction_bid_details ticket -> liquidation_auction_bid_details ticket transaction_value
  | PermTransactionValue : permission_content ticket -> permission_content ticket transaction_value

(* operation *)

type operation =
  | SetDelegate of key_hash option
  | Transaction : 'a transaction_value * tez * 'a contract -> operation (* For inspection (in tests) pattern match on the transaction_value ;-) *)
  | NotImplementedYet : operation

module Tezos = struct
  let set_delegate hash_option = SetDelegate hash_option

  let unit_transaction () tez contract = Transaction (UnitTransactionValue, tez, contract)
  let kit_transaction value tez contract = Transaction (KitTransactionValue value, tez, contract)
  let lqt_transaction value tez contract = Transaction (LqtTransactionValue value, tez, contract)
  let da_bid_transaction value tez contract = Transaction (DaBidTransactionValue value, tez, contract)
  let la_bid_transaction value tez contract = Transaction (LaBidTransactionValue value, tez, contract)
  let perm_transaction value tez contract = Transaction (PermTransactionValue value, tez, contract)

  let get_entrypoint_opt ep address = (* Sad, giving always Some, I know, but I know of no other way. *)
    Some (Contract (address_of_string (string_of_address address ^ "%" ^ ep)))

  let get_contract_opt address = Some (Contract address)
end
