open Ligo
open BurrowTypes
open Tickets

(* contract *)

type 'parameter contract = Contract of address
(**
   A typed contract.

   Use unit as parameter to indicate an implicit account.
*)

val pp_contract : Format.formatter -> 'parameter contract -> unit
val show_contract : 'parameter contract -> String.t

type 'parameter transaction_value = (* GADT *)
  | UnitTransactionValue : unit transaction_value
  | AddressTransactionValue : address -> address transaction_value
  | KitTransactionValue : kit_token_content ticket -> kit_token_content ticket transaction_value
  | LqtTransactionValue : liquidity_token_content ticket -> liquidity_token_content ticket transaction_value
  | DaBidTransactionValue : delegation_auction_bid ticket -> delegation_auction_bid ticket transaction_value
  | LaBidTransactionValue : liquidation_auction_bid ticket -> liquidation_auction_bid ticket transaction_value
  | PermTransactionValue : permission_content ticket -> permission_content ticket transaction_value
  | TezAddressTransactionValue : (tez * address) -> (tez * address) transaction_value
  | OptKeyHashTransactionValue : key_hash option -> key_hash option transaction_value
  | TezTransactionValue : tez -> tez transaction_value
  | NatContractTransactionValue : nat contract -> nat contract transaction_value

(* operation *)

type operation =
  | SetDelegate of key_hash option
  | Transaction : 'a transaction_value * tez * 'a contract -> operation (* For inspection (in tests) pattern match on the transaction_value ;-) *)
  | CreateContract of
      ((burrow_parameter * burrow_storage) -> (operation list * burrow_storage)) *
      key_hash option *
      tez *
      address
  (**
      An operation emitted by the contract
  *)

val pp_operation : Format.formatter -> operation -> unit
val show_operation : operation -> String.t

module Tezos : sig
  val address_counter: nat ref
  val get_next_address : unit -> address

  val set_delegate : key_hash option -> operation

  val unit_transaction : unit -> tez -> unit contract -> operation
  val address_transaction : address -> tez -> address contract -> operation
  val kit_transaction : kit_token_content ticket -> tez -> kit_token_content ticket contract -> operation
  val lqt_transaction : liquidity_token_content ticket -> tez -> liquidity_token_content ticket contract -> operation
  val da_bid_transaction : delegation_auction_bid ticket -> tez -> delegation_auction_bid ticket contract -> operation
  val la_bid_transaction : liquidation_auction_bid ticket -> tez -> liquidation_auction_bid ticket contract -> operation
  val perm_transaction : permission_content ticket -> tez -> permission_content ticket contract -> operation
  val tez_address_transaction : (tez * address) -> tez -> (tez * address) contract -> operation
  val opt_key_hash_transaction : key_hash option -> tez -> key_hash option contract -> operation
  val tez_transaction : tez -> tez -> tez contract -> operation
  val nat_contract_transaction : nat contract -> tez -> nat contract contract -> operation

  val get_entrypoint_opt : string -> address -> 'parameter contract option
  val get_contract_opt : address -> unit contract option (* could also leave it as a parameter *)

  val create_contract :
    ((burrow_parameter * burrow_storage) -> (operation list * burrow_storage)) ->
    key_hash option ->
    tez ->
    address ->
    (operation * address)
end
