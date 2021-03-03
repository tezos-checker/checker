open LiquidationAuctionPrimitiveTypes
open Kit
open Common
open Error

(*
Ticket-based entitities in checker and their expected value/mechanics:

| Ticket           | Multiplicity | (Usably) Splittable              |
|------------------|--------------|----------------------------------|
| liquidity        | non-negative | Yes (finitely, zero is useless)  |
| kit              | non-negative | Yes (finitely, zero is useless)  |
| permission       | always zero  | Yes (infinitely, always zero)    |
| del. auction bid | always one   | No  (zero is useless)            |
| col. auction bid | always one   | No  (zero is useless)            |
*)

(* KIT TOKENS *)

(* Kit are really tickets. *)
type kit_token_content = Kit
[@@deriving show]

type kit_token = kit_token_content Ligo.ticket
[@@deriving show]

let[@inline] kit_issue (kit: kit) : kit_token = Ligo.Tezos.create_ticket (Kit) (kit_to_mukit kit)

let[@inline] read_kit (token: kit_token) : kit * kit_token =
  let (_issuer, (_content, mukit)), same_token = Ligo.Tezos.read_ticket token in
  (kit_of_mukit mukit, same_token)

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let[@inline] ensure_valid_kit_token (token: kit_token) : kit_token =
  let (issuer, (_content, _amnt)), same_ticket = Ligo.Tezos.read_ticket token in
  let is_valid = issuer = checker_address in (* TODO: amnt > Nat.zero perhaps? *)
  if is_valid
  then same_ticket
  else (Ligo.failwith error_InvalidKitToken : kit_token)

(* LIQUIDITY TOKENS *)

type liquidity_token_content = Lqt
[@@deriving show]

type liquidity = liquidity_token_content Ligo.ticket
[@@deriving show]

let[@inline] issue_liquidity_tokens (n: Ligo.nat) : liquidity = Ligo.Tezos.create_ticket (Lqt) n

(** Check whether a liquidity token is valid. A liquidity token is valid if it
  * is issued by checker, and it is tagged appropriately (this is already
  * enforced by its type). *)
let[@inline] ensure_valid_liquidity_token (liquidity: liquidity) : liquidity =
  let (issuer, (_content, _lqt)), liquidity = Ligo.Tezos.read_ticket liquidity in
  if issuer = checker_address
  then liquidity
  else (Ligo.failwith error_InvalidLiquidityToken : liquidity)

(* DELEGATION AUCTION BID TICKETS *)

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

let[@inline] issue_delegation_auction_bid_ticket (bid: delegation_auction_bid) : delegation_auction_bid Ligo.ticket =
  Ligo.Tezos.create_ticket bid (Ligo.nat_from_literal "1n")

(** Ensure that a delegation auction bid ticket is valid. A delegation bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. TODO: (c) is not
  * implemented yet. Perhaps it can be avoided, if all checker-issued tickets
  * end up having contents clearly distinguished by type. *)
let[@inline] ensure_valid_delegation_auction_bid_ticket
      (bid_ticket: delegation_auction_bid Ligo.ticket)
  : delegation_auction_bid Ligo.ticket =
  let (issuer, (_, amt)), same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid = issuer = checker_address && amt = Ligo.nat_from_literal "1n" in
  if is_valid
  then same_ticket
  else (Ligo.failwith error_InvalidDelegationAuctionTicket : delegation_auction_bid Ligo.ticket)

(* LIQUIDATION AUCTION BID TICKETS *)

type liquidation_auction_bid_details = { auction_id: liquidation_auction_id; bid: bid; }
[@@deriving show]

type liquidation_auction_bid_ticket = liquidation_auction_bid_details Ligo.ticket

let[@inline] issue_liquidation_auction_bid_ticket (bid_details: liquidation_auction_bid_details) =
  Ligo.Tezos.create_ticket bid_details (Ligo.nat_from_literal "1n")

(** Check whether a liquidation auction bid ticket is valid. An auction bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. TODO: (c) is not
  * implemented yet. Perhaps it can be avoided, if all checker-issued tickets
  * end up having contents clearly distinguished by type. *)
let[@inline] liquidation_auction_ensure_valid_bid_ticket (bid_ticket: liquidation_auction_bid_ticket) : liquidation_auction_bid_ticket =
  let (issuer, (_bid_details, amnt)), same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid = issuer = checker_address && amnt = Ligo.nat_from_literal "1n" in
  if is_valid
  then same_ticket
  else (Ligo.failwith error_InvalidLiquidationAuctionTicket : liquidation_auction_bid_ticket)

(* PERMISSION TICKETS *)

type specific_rights =
  { deposit_tez: bool;
    withdraw_tez: bool;
    mint_kit: bool;
    burn_kit: bool;
    set_delegate: bool;
    cancel_liquidation: bool;
  }
[@@deriving show]

(** A right can be an admin right (which implies all right), or a user right,
  * which can include depositing/withdrawing tez, minting/burning kit, setting
  * the delegate, and/or canceling liquidations. *)
type rights =
  | Admin
  | User of specific_rights
[@@deriving show]

type permission_content = rights * Ligo.address * Ligo.nat
[@@deriving show]

(** A permission is a ticket containing a right. *)
type permission = permission_content Ligo.ticket
[@@deriving show]

(* NOTE: It totally consumes the ticket. It's the caller's responsibility to
 * replicate the permission ticket if they don't want to lose it. *)
let[@inline] ensure_valid_permission
      (permission: permission)
      (burrow_id: Ligo.address)
      (burrow_permission_version: Ligo.nat)
  : rights =
  let (issuer, ((right, id, version), amnt)), _ = Ligo.Tezos.read_ticket permission in
  let validity_condition =
    issuer = checker_address
    && amnt = Ligo.nat_from_literal "0n"
    && version = burrow_permission_version
    && id = burrow_id in
  if validity_condition
  then right
  else (Ligo.failwith error_InvalidPermission : rights)

let ensure_permission_is_present (permission: permission option) : permission =
  match permission with
  | None -> (Ligo.failwith error_MissingPermission : permission)
  | Some permission -> permission
