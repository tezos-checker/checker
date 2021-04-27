open Ligo
open BurrowTypes
open Tickets

(* contract *)

type 'parameter transaction_value = (* GADT *)
  | UnitTransactionValue : unit transaction_value
  | AddressTransactionValue : address -> address transaction_value
  | LaBidTransactionValue : liquidation_auction_bid_content ticket -> liquidation_auction_bid_content ticket transaction_value
  | TezAddressTransactionValue : (tez * address) -> (tez * address) transaction_value
  | OptKeyHashTransactionValue : key_hash option -> key_hash option transaction_value
  | TezTransactionValue : tez -> tez transaction_value
  | NatContractTransactionValue : nat contract -> nat contract transaction_value
  | FA12TransferTransactionValue : Fa12Types.transfer -> Fa12Types.transfer transaction_value
  | FA2BalanceOfResponseTransactionValue : Fa2Interface.fa2_balance_of_response list -> Fa2Interface.fa2_balance_of_response list transaction_value

type tez_and_address = (tez * address)
[@@deriving show]

type key_hash_option = key_hash option
[@@deriving show]

let show_transaction_value : type parameter. parameter transaction_value -> String.t =
  fun tv ->
  match tv with
  | UnitTransactionValue -> "()"
  | AddressTransactionValue a -> string_of_address a
  | LaBidTransactionValue c -> show_ticket pp_liquidation_auction_bid_content c
  | TezAddressTransactionValue ta -> show_tez_and_address ta
  | OptKeyHashTransactionValue kho -> show_key_hash_option kho
  | TezTransactionValue tz -> string_of_tez tz
  | NatContractTransactionValue c -> show_contract c
  | FA12TransferTransactionValue t -> Fa12Types.show_transfer t
  | FA2BalanceOfResponseTransactionValue xs -> Fa2Interface.show_fa2_balance_of_response_list xs

(* operation *)

type operation =
  | SetDelegate of key_hash option
  | Transaction : 'a transaction_value * tez * 'a contract -> operation (* For inspection (in tests) pattern match on the transaction_value ;-) *)
  | CreateContract of
      ((burrow_parameter * burrow_storage) -> (operation list * burrow_storage)) *
      key_hash option *
      tez *
      address

let show_operation (op: operation) : String.t =
  match op with
  | SetDelegate kho -> "SetDelegate (" ^ show_key_hash_option kho ^ ")"
  | Transaction (tv,tz,c) ->
    "Transaction " ^ "(" ^ show_transaction_value tv ^ ", " ^ string_of_tez tz ^ ", " ^ show_contract c ^ ")"
  | CreateContract (_code, kho, init_tez, init_store) ->
    "CreateContract (<code>, " ^ show_key_hash_option kho ^ ", " ^ string_of_tez init_tez ^ ", " ^ show_burrow_storage init_store ^ ")"

let pp_operation fmt op = Format.pp_print_string fmt (show_operation op)

module Tezos = struct
  let address_counter = ref (nat_from_literal "0n")

  let get_next_address () =
    let cnt = !address_counter in
    address_counter := add_nat_nat !address_counter (nat_from_literal "1n");
    address_of_string ("tz1" ^ string_of_nat cnt)

  let set_delegate hash_option = SetDelegate hash_option

  let unit_transaction () tez contract = Transaction (UnitTransactionValue, tez, contract)
  let address_transaction address tez contract = Transaction (AddressTransactionValue address, tez, contract)
  let la_bid_transaction value tez contract = Transaction (LaBidTransactionValue value, tez, contract)
  let tez_address_transaction value tez contract = Transaction (TezAddressTransactionValue value, tez, contract)
  let opt_key_hash_transaction value tez contract = Transaction (OptKeyHashTransactionValue value, tez, contract)
  let tez_transaction value tez contract = Transaction (TezTransactionValue value, tez, contract)
  let nat_contract_transaction value tez contract = Transaction (NatContractTransactionValue value, tez, contract)
  let fa12_transfer_transaction value tez contract = Transaction (FA12TransferTransactionValue value, tez, contract)
  let fa2_balance_of_response_transaction value tez contract = Transaction (FA2BalanceOfResponseTransactionValue value, tez, contract)

  let get_entrypoint_opt ep address = (* Sad, giving always Some, I know, but I know of no other way. *)
    Some (contract_of_address (address_of_string (string_of_address address ^ ep))) (* ep includes the % character *)

  let get_contract_opt address = Some (contract_of_address address)

  let create_contract code delegate tez store =
    let contract = CreateContract (code, delegate, tez, store) in
    let address = get_next_address () in
    (contract, address)
end
