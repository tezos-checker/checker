open LiquidationAuctionPrimitiveTypes
open Kit

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

(* LIQUIDITY TOKENS *)

type liquidity_token_content = Lqt
[@@deriving show]

type liquidity = liquidity_token_content Ligo.ticket
[@@deriving show]

let[@inline] issue_liquidity_tokens (n: Ligo.nat) : liquidity = Ligo.Tezos.create_ticket (Lqt) n

(* DELEGATION AUCTION BID TICKETS *)

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

let[@inline] issue_delegation_auction_bid_ticket (bid: delegation_auction_bid) : delegation_auction_bid Ligo.ticket =
  Ligo.Tezos.create_ticket bid (Ligo.nat_from_literal "1n")

(* LIQUIDATION AUCTION BID TICKETS *)

type liquidation_auction_bid_details = { auction_id: liquidation_auction_id; bid: bid; }
[@@deriving show]

type liquidation_auction_bid_ticket = liquidation_auction_bid_details Ligo.ticket

let[@inline] issue_liquidation_auction_bid_ticket (bid_details: liquidation_auction_bid_details) =
  Ligo.Tezos.create_ticket bid_details (Ligo.nat_from_literal "1n")

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
