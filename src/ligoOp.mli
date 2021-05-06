open Ligo
open BurrowTypes

type 'parameter transaction_value = (* GADT *)
  | UnitTransactionValue : unit transaction_value
  | AddressNatTransactionValue : (address * nat) -> (address * nat) transaction_value
  | TezAddressTransactionValue : (tez * address) -> (tez * address) transaction_value
  | OptKeyHashTransactionValue : key_hash option -> key_hash option transaction_value
  | TezTransactionValue : tez -> tez transaction_value
  | NatContractTransactionValue : nat contract -> nat contract transaction_value
  | FA12TransferTransactionValue : Fa12Types.fa12_transfer -> Fa12Types.fa12_transfer transaction_value
  | FA2BalanceOfResponseTransactionValue : Fa2Interface.fa2_balance_of_response list -> Fa2Interface.fa2_balance_of_response list transaction_value

(* operation *)

type operation =
  | SetDelegate of key_hash option
  | Transaction : 'a transaction_value * tez * 'a contract -> operation (* For inspection (in tests) pattern match on the transaction_value ;-) *)
  | CreateContract of
      ((burrow_parameter * burrow_storage) -> (operation list * burrow_storage)) *
      key_hash option *
      tez *
      burrow_storage
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
  val address_nat_transaction : address * nat -> tez -> (address * nat) contract -> operation
  val tez_address_transaction : (tez * address) -> tez -> (tez * address) contract -> operation
  val opt_key_hash_transaction : key_hash option -> tez -> key_hash option contract -> operation
  val tez_transaction : tez -> tez -> tez contract -> operation
  val nat_contract_transaction : nat contract -> tez -> nat contract contract -> operation
  val fa12_transfer_transaction : Fa12Types.fa12_transfer -> tez -> Fa12Types.fa12_transfer contract -> operation
  val fa2_balance_of_response_transaction : Fa2Interface.fa2_balance_of_response list -> tez-> Fa2Interface.fa2_balance_of_response list contract -> operation

  val get_entrypoint_opt : string -> address -> 'parameter contract option
  val get_contract_opt : address -> unit contract option (* could also leave it as a parameter *)

  val create_contract :
    ((burrow_parameter * burrow_storage) -> (operation list * burrow_storage)) ->
    key_hash option ->
    tez ->
    burrow_storage ->
    (operation * address)
end
