[@@@coverage exclude_file]

open Ligo
open BurrowTypes
open VaultTypes

(* contract *)

type 'parameter transaction_value = (* GADT *)
  | UnitTransactionValue : unit transaction_value
  | AddressNatTransactionValue : (address * nat) -> (address * nat) transaction_value
  | TezAddressTransactionValue : (tez * address) -> (tez * address) transaction_value
  | OptKeyHashTransactionValue : key_hash option -> key_hash option transaction_value
  | NatContractTransactionValue : nat contract -> nat contract transaction_value
  | FA12TransferTransactionValue : Fa12Interface.fa12_transfer -> Fa12Interface.fa12_transfer transaction_value
  | FA2TransferTransactionValue : Fa2Interface.fa2_transfer list -> Fa2Interface.fa2_transfer list transaction_value
  | FA2BalanceOfResponseTransactionValue : Fa2Interface.fa2_balance_of_response list -> Fa2Interface.fa2_balance_of_response list transaction_value
  | AddressTezTransactionValue : (address * tez) -> (address * tez) transaction_value
  | AddressTezAddressTransactionValue : (address * tez * address) -> (address * tez * address) transaction_value
  | AddressOptKeyHashTransactionValue : (address * key_hash option) -> (address * key_hash option) transaction_value
  | NatNatContractTransactionValue : (nat * nat) contract -> (nat * nat) contract transaction_value

type address_and_nat = (address * nat)
[@@deriving show]

type address_and_tez = (address * tez)
[@@deriving show]

type address_and_tez_and_address = (address * tez * address)
[@@deriving show]

type address_and_key_hash_option = (address * key_hash option)
[@@deriving show]

type tez_and_address = (tez * address)
[@@deriving show]

type key_hash_option = key_hash option
[@@deriving show]

let show_transaction_value : type parameter. parameter transaction_value -> String.t =
  fun tv ->
  match tv with
  | UnitTransactionValue -> "()"
  | AddressNatTransactionValue p -> show_address_and_nat p
  | TezAddressTransactionValue ta -> show_tez_and_address ta
  | OptKeyHashTransactionValue kho -> show_key_hash_option kho
  | NatContractTransactionValue c -> show_contract c
  | FA12TransferTransactionValue t -> Fa12Interface.show_fa12_transfer t
  | FA2TransferTransactionValue t -> Fa2Interface.show_fa2_transfer_list t
  | FA2BalanceOfResponseTransactionValue xs -> Fa2Interface.show_fa2_balance_of_response_list xs
  | AddressTezTransactionValue at -> show_address_and_tez at
  | AddressTezAddressTransactionValue ata -> show_address_and_tez_and_address ata
  | AddressOptKeyHashTransactionValue akho -> show_address_and_key_hash_option akho
  | NatNatContractTransactionValue c -> show_contract c

(* operation *)

type operation =
  | SetDelegate of key_hash option
  | Transaction : 'a transaction_value * tez * 'a contract -> operation (* For inspection (in tests) pattern match on the transaction_value ;-) *)
  | CreateBurrowContract of
      ((burrow_parameter * burrow_storage) -> (operation list * burrow_storage)) *
      key_hash option *
      tez *
      burrow_storage
  | CreateVaultContract of
      ((vault_parameter * vault_storage) -> (operation list * vault_storage)) *
      key_hash option *
      tez *
      vault_storage

let show_operation (op: operation) : String.t =
  match op with
  | SetDelegate kho -> "SetDelegate (" ^ show_key_hash_option kho ^ ")"
  | Transaction (tv,tz,c) ->
    "Transaction " ^ "(" ^ show_transaction_value tv ^ ", " ^ string_of_tez tz ^ ", " ^ show_contract c ^ ")"
  | CreateBurrowContract (_code, kho, init_tez, init_store) ->
    "CreateBurrowContract (<code>, " ^ show_key_hash_option kho ^ ", " ^ string_of_tez init_tez ^ ", " ^ show_burrow_storage init_store ^ ")"
  | CreateVaultContract (_code, kho, init_tez, init_store) ->
    "CreateVaultContract (<code>, " ^ show_key_hash_option kho ^ ", " ^ string_of_tez init_tez ^ ", " ^ show_vault_storage init_store ^ ")"

let pp_operation fmt op = Format.pp_print_string fmt (show_operation op)

module Tezos = struct
  let address_counter = ref (nat_from_literal "0n")

  let get_next_address () =
    let cnt = !address_counter in
    address_counter := add_nat_nat !address_counter (nat_from_literal "1n");
    address_of_string ("tz1" ^ string_of_nat cnt)

  let set_delegate hash_option = SetDelegate hash_option

  let unit_transaction () tez contract = Transaction (UnitTransactionValue, tez, contract)
  let address_nat_transaction p tez contract = Transaction (AddressNatTransactionValue p, tez, contract)
  let tez_address_transaction value tez contract = Transaction (TezAddressTransactionValue value, tez, contract)
  let opt_key_hash_transaction value tez contract = Transaction (OptKeyHashTransactionValue value, tez, contract)
  let nat_contract_transaction value tez contract = Transaction (NatContractTransactionValue value, tez, contract)
  let fa12_transfer_transaction value tez contract = Transaction (FA12TransferTransactionValue value, tez, contract)
  let fa2_transfer_transaction value tez contract = Transaction (FA2TransferTransactionValue value, tez, contract)
  let fa2_balance_of_response_transaction value tez contract = Transaction (FA2BalanceOfResponseTransactionValue value, tez, contract)
  let address_tez_transaction value tez contract = Transaction (AddressTezTransactionValue value, tez, contract)
  let address_tez_address_transaction value tez contract = Transaction (AddressTezAddressTransactionValue value, tez, contract)
  let address_opt_key_hash_transaction value tez contract = Transaction (AddressOptKeyHashTransactionValue value, tez, contract)
  let nat_nat_contract_transaction value tez contract = Transaction (NatNatContractTransactionValue value, tez, contract)

  let get_entrypoint_opt ep address = (* Sad, giving always Some, I know, but I know of no other way. *)
    Some (contract_of_address (address_of_string (string_of_address address ^ ep))) (* ep includes the % character *)

  let get_contract_opt address = Some (contract_of_address address)

  let burrow_create_contract code delegate tez store =
    let contract = CreateBurrowContract (code, delegate, tez, store) in
    let address = get_next_address () in
    (contract, address)

  let vault_create_contract code delegate tez store =
    let contract = CreateVaultContract (code, delegate, tez, store) in
    let address = get_next_address () in
    (contract, address)
end
