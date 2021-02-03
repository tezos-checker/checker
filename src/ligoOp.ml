open Ligo
open TokenTypes
open BurrowTypes

(* contract *)

type 'parameter contract = Contract of address

let show_contract (Contract address) = "Contract " ^ string_of_address address
let pp_contract fmt contract = Format.pp_print_string fmt (show_contract contract)

type 'parameter transaction_value = (* GADT *)
  | UnitTransactionValue : unit transaction_value
  | AddressTransactionValue : address -> address transaction_value
  | KitTransactionValue : kit_token_content ticket -> kit_token_content ticket transaction_value
  | LqtTransactionValue : liquidity_token_content ticket -> liquidity_token_content ticket transaction_value
  | DaBidTransactionValue : delegation_auction_bid ticket -> delegation_auction_bid ticket transaction_value
  | LaBidTransactionValue : liquidation_auction_bid_details ticket -> liquidation_auction_bid_details ticket transaction_value
  | PermTransactionValue : permission_content ticket -> permission_content ticket transaction_value
  | TezAddressTransactionValue : (tez * address) -> (tez * address) transaction_value
  | OptKeyHashTransactionValue : key_hash option -> key_hash option transaction_value

type tez_and_address = (tez * address)
[@@deriving show]

type key_hash_option = key_hash option
[@@deriving show]

let show_transaction_value : type parameter. parameter transaction_value -> String.t =
  fun tv ->
    match tv with
    | UnitTransactionValue -> "()"
    | AddressTransactionValue a -> string_of_address a
    | KitTransactionValue c -> show_ticket pp_kit_token_content c
    | LqtTransactionValue c -> show_ticket pp_liquidity_token_content c
    | DaBidTransactionValue c -> show_ticket pp_delegation_auction_bid c
    | LaBidTransactionValue c -> show_ticket pp_liquidation_auction_bid_details c
    | PermTransactionValue c -> show_ticket pp_permission_content c
    | TezAddressTransactionValue ta -> show_tez_and_address ta
    | OptKeyHashTransactionValue kho -> show_key_hash_option kho

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
  let kit_transaction value tez contract = Transaction (KitTransactionValue value, tez, contract)
  let lqt_transaction value tez contract = Transaction (LqtTransactionValue value, tez, contract)
  let da_bid_transaction value tez contract = Transaction (DaBidTransactionValue value, tez, contract)
  let la_bid_transaction value tez contract = Transaction (LaBidTransactionValue value, tez, contract)
  let perm_transaction value tez contract = Transaction (PermTransactionValue value, tez, contract)
  let tez_address_transaction value tez contract = Transaction (TezAddressTransactionValue value, tez, contract)
  let opt_key_hash_transaction value tez contract = Transaction (OptKeyHashTransactionValue value, tez, contract)

  let get_entrypoint_opt ep address = (* Sad, giving always Some, I know, but I know of no other way. *)
    Some (Contract (address_of_string (string_of_address address ^ ep))) (* ep includes the % character *)

  let get_contract_opt address = Some (Contract address)

  let create_contract code delegate tez store =
    let contract = CreateContract (code, delegate, tez, store) in
    let address = get_next_address () in
    (contract, address)
end
