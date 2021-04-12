open Kit
open LiquidationAuctionPrimitiveTypes

type specific_rights =
  { deposit_tez: bool;
    withdraw_tez: bool;
    mint_kit: bool;
    burn_kit: bool;
    set_delegate: bool;
    cancel_liquidation: bool;
  }

type rights =
  | Admin
  | User of specific_rights

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }

type liquidation_auction_bid = { auction_id: liquidation_auction_id; bid: bid; }

type kit_token_content (* opaque *)
type kit_token = kit_token_content Ligo.ticket
val kit_issue : kit -> kit_token
val ensure_valid_kit_token : kit_token -> kit

type liquidity_token_content (* opaque *)
type liquidity = liquidity_token_content Ligo.ticket
val issue_liquidity_tokens : Ligo.nat -> liquidity
val ensure_valid_liquidity_token : liquidity -> Ligo.nat

type delegation_auction_bid_content (* opaque *)
type delegation_auction_bid_ticket = delegation_auction_bid_content Ligo.ticket
val issue_delegation_auction_bid_ticket : delegation_auction_bid -> delegation_auction_bid_ticket
val ensure_valid_delegation_auction_bid_ticket : delegation_auction_bid_ticket -> delegation_auction_bid

type liquidation_auction_bid_content (* opaque *)
type liquidation_auction_bid_ticket = liquidation_auction_bid_content Ligo.ticket
val issue_liquidation_auction_bid_ticket : liquidation_auction_bid -> liquidation_auction_bid_ticket
val ensure_valid_liquidation_auction_bid_ticket : liquidation_auction_bid_ticket -> liquidation_auction_bid

type permission_content (* opaque *)
type permission_redacted_content = rights * Ligo.address * Ligo.nat
type permission = permission_content Ligo.ticket
val issue_permission_ticket : rights -> Ligo.address -> Ligo.nat -> permission
val ensure_valid_permission : permission -> permission_redacted_content
val ensure_matching_permission : Ligo.address -> Ligo.nat -> permission_redacted_content -> rights
val ensure_permission_is_present : permission option -> permission

(* BEGIN_OCAML *)
val show_kit_token_content : kit_token_content -> string
val pp_kit_token_content : Format.formatter -> kit_token_content -> unit

val show_kit_token : kit_token -> string
val pp_kit_token : Format.formatter -> kit_token -> unit

val show_liquidity_token_content : liquidity_token_content -> string
val pp_liquidity_token_content : Format.formatter -> liquidity_token_content -> unit

val show_liquidity : liquidity -> string
val pp_liquidity : Format.formatter -> liquidity -> unit

val show_delegation_auction_bid_content : delegation_auction_bid_content -> string
val pp_delegation_auction_bid_content : Format.formatter -> delegation_auction_bid_content -> unit

val show_delegation_auction_bid : delegation_auction_bid -> string
val pp_delegation_auction_bid : Format.formatter -> delegation_auction_bid -> unit

val show_liquidation_auction_bid_content : liquidation_auction_bid_content -> string
val pp_liquidation_auction_bid_content : Format.formatter -> liquidation_auction_bid_content -> unit

val show_liquidation_auction_bid : liquidation_auction_bid -> string
val pp_liquidation_auction_bid : Format.formatter -> liquidation_auction_bid -> unit

val show_permission_content : permission_content -> string
val pp_permission_content : Format.formatter -> permission_content -> unit

val show_permission_redacted_content : permission_redacted_content -> string
val pp_permission_redacted_content : Format.formatter -> permission_redacted_content -> unit

val show_permission : permission -> string
val pp_permission : Format.formatter -> permission -> unit
(* END_OCAML *)
