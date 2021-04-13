open LiquidationAuctionPrimitiveTypes
open Kit
open Common
open Error

(*
Ticket-based entitities in checker and their expected value/mechanics:

| Ticket           | Multiplicity | (Usably) Splittable              | Tag |
|------------------|--------------|----------------------------------|-----|
| kit              | non-negative | Yes (finitely, zero is useless)  |  1n |
| liquidity        | non-negative | Yes (finitely, zero is useless)  |  2n |
| del. auction bid | always one   | No  (zero is useless)            |  3n |
| liq. auction bid | always one   | No  (zero is useless)            |  4n |
| permission       | always zero  | Yes (infinitely, always zero)    |  5n |
*)

type token_tag = Ligo.nat
[@@deriving show]

let[@inline] kit_token_tag       = Ligo.nat_from_literal "1n"
let[@inline] lqt_token_tag       = Ligo.nat_from_literal "2n"
let[@inline] del_auction_bid_tag = Ligo.nat_from_literal "3n"
let[@inline] liq_auction_bid_tag = Ligo.nat_from_literal "4n"
let[@inline] permission_tag      = Ligo.nat_from_literal "5n"

(* ************************************************************************* *)
(**                               GENERAL                                    *)
(* ************************************************************************* *)

(* ************************************************************************* *)
(**                              KIT TOKENS                                  *)
(* ************************************************************************* *)

type kit_content = Kit (* NOTE: No need for real content. Unit in Michelson. *)
[@@deriving show]

type kit_token_content = token_tag * kit_content
[@@deriving show]

type kit_token = kit_token_content Ligo.ticket
[@@deriving show]

let[@inline] kit_issue (kit: kit) : kit_token =
  Ligo.Tezos.create_ticket (kit_token_tag, Kit) (kit_to_mukit_nat kit)

(** Check whether a kit token is valid and return the amount of kit stored in
  * it if it is. A kit token is valid if (a) it is issued by checker, and (b)
  * is tagged appropriately. In OCaml/LIGO the type ensures (b), but in
  * Michelson this is not strictly necessary, hence the runtime check of the
  * tag. *)
let[@inline] ensure_valid_kit_token (token: kit_token) : kit =
  let (issuer, ((tag, _content), mukit)), _same_ticket = Ligo.Tezos.read_ticket token in
  let is_valid = issuer = checker_address && tag = kit_token_tag in
  if is_valid
  then kit_of_mukit mukit
  else (Ligo.failwith error_InvalidKitToken : kit)

(* ************************************************************************* *)
(**                           LIQUIDITY TOKENS                               *)
(* ************************************************************************* *)

type liquidity_content = Lqt (* NOTE: No need for real content. Unit in Michelson. *)
[@@deriving show]

type liquidity_token_content = token_tag * liquidity_content
[@@deriving show]

type liquidity = liquidity_token_content Ligo.ticket
[@@deriving show]

let[@inline] issue_liquidity_tokens (n: Ligo.nat) : liquidity =
  Ligo.Tezos.create_ticket (lqt_token_tag, Lqt) n

(** Check whether a liquidity token is valid. A liquidity token is valid if (a)
  * it is issued by checker, and (b) it is tagged appropriately. In OCaml/LIGO
  * the type ensures (b), but in Michelson this is not strictly necessary,
  * hence the runtime check of the tag. *)
let[@inline] ensure_valid_liquidity_token (liquidity: liquidity) : Ligo.nat =
  let (issuer, ((tag, _content), lqt)), _liquidity = Ligo.Tezos.read_ticket liquidity in
  let is_valid = issuer = checker_address && tag = lqt_token_tag in
  if is_valid
  then lqt
  else (Ligo.failwith error_InvalidLiquidityToken : Ligo.nat)

(* ************************************************************************* *)
(**                    DELEGATION AUCTION BID TICKETS                        *)
(* ************************************************************************* *)

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

type delegation_auction_bid_content = token_tag * delegation_auction_bid
[@@deriving show]

type delegation_auction_bid_ticket = delegation_auction_bid_content Ligo.ticket

let[@inline] issue_delegation_auction_bid_ticket (bid: delegation_auction_bid) : delegation_auction_bid_ticket =
  Ligo.Tezos.create_ticket (del_auction_bid_tag, bid) (Ligo.nat_from_literal "1n")

(** Ensure that a delegation auction bid ticket is valid. A delegation bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. In OCaml/LIGO the
  * type ensures (c), but in Michelson this is not strictly necessary
  * (currently is, but the content might change in the future), hence the
  * runtime check of the tag. *)
let[@inline] ensure_valid_delegation_auction_bid_ticket
    (bid_ticket: delegation_auction_bid_ticket)
  : delegation_auction_bid =
  let (issuer, ((tag, bid), amt)), _same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid =
    issuer = checker_address
    && tag = del_auction_bid_tag
    && amt = Ligo.nat_from_literal "1n" in
  if is_valid
  then bid
  else (Ligo.failwith error_InvalidDelegationAuctionTicket : delegation_auction_bid)

(* ************************************************************************* *)
(**                    LIQUIDATION AUCTION BID TICKETS                       *)
(* ************************************************************************* *)

type liquidation_auction_bid = { auction_id: liquidation_auction_id; bid: bid; }
[@@deriving show]

type liquidation_auction_bid_content = token_tag * liquidation_auction_bid
[@@deriving show]

type liquidation_auction_bid_ticket = liquidation_auction_bid_content Ligo.ticket

let[@inline] issue_liquidation_auction_bid_ticket (bid_details: liquidation_auction_bid) =
  Ligo.Tezos.create_ticket (liq_auction_bid_tag, bid_details) (Ligo.nat_from_literal "1n")

(** Check whether a liquidation auction bid ticket is valid. An auction bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. In OCaml/LIGO the
  * type ensures (c), but in Michelson this is not strictly necessary
  * (currently is, but the content might change in the future), hence the
  * runtime check of the tag. *)
let[@inline] ensure_valid_liquidation_auction_bid_ticket (bid_ticket: liquidation_auction_bid_ticket) : liquidation_auction_bid =
  let (issuer, ((tag, bid_details), amnt)), _same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid =
    issuer = checker_address
    && tag = liq_auction_bid_tag
    && amnt = Ligo.nat_from_literal "1n" in
  if is_valid
  then bid_details
  else (Ligo.failwith error_InvalidLiquidationAuctionTicket : liquidation_auction_bid)

(* ************************************************************************* *)
(**                          PERMISSION TICKETS                              *)
(* ************************************************************************* *)

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

type permission_content = token_tag * rights * Ligo.address * Ligo.nat
[@@deriving show]

type permission_redacted_content = rights * Ligo.address * Ligo.nat
[@@deriving show]

(** A permission is a ticket containing a right. *)
type permission = permission_content Ligo.ticket
[@@deriving show]

let[@inline] issue_permission_ticket (r: rights) (burrow_id: Ligo.address) (perm_version: Ligo.nat) =
  Ligo.Tezos.create_ticket (permission_tag, r, burrow_id, perm_version) (Ligo.nat_from_literal "0n")

(** Check whether a permission ticket is valid. A permission ticket is valid if
  * (a) it is issued by checker, (b) its amount is exactly zero (infinitely
  * splittable), and (c) it is tagged appropriately. In OCaml/LIGO the type
  * ensures (c), but in Michelson this is not strictly necessary (currently is,
  * but the content might change in the future), hence the runtime check of the
  * tag. *)
let[@inline] ensure_valid_permission (permission: permission) : permission_redacted_content =
  let (issuer, ((tag, rights, id, version), amnt)), _ = Ligo.Tezos.read_ticket permission in
  let is_valid =
    issuer = checker_address
    && tag = permission_tag
    && amnt = Ligo.nat_from_literal "0n" in
  if is_valid
  then (rights, id, version)
  else (Ligo.failwith error_InvalidPermission : permission_redacted_content)

let[@inline] ensure_valid_optional_permission (permission: permission option) : permission_redacted_content option =
  match permission with
  | None -> (None: permission_redacted_content option)
  | Some permission -> Some (ensure_valid_permission permission)

(** Check whether a permission (redacted content) matches the burrow it's
  * supposed to match. That is, check whether (a) it matches the given burrow
  * id, and (b) it matches the permission version specified by the burrow. Note
  * that this function does not check the validity of the permission itself;
  * ensure_valid_permission does that. *)
let[@inline] ensure_matching_permission
    (expected_id: Ligo.address)
    (expected_version: Ligo.nat)
    (rights, id, version: permission_redacted_content)
  : rights =
  if id = expected_id && version = expected_version
  then rights
  else (Ligo.failwith error_InvalidPermission : rights)

let ensure_permission_is_present (permission: permission_redacted_content option) : permission_redacted_content =
  match permission with
  | None -> (Ligo.failwith error_MissingPermission : permission_redacted_content)
  | Some permission -> permission
