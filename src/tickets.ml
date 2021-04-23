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
| liq. auction bid | always one   | No  (zero is useless)            |  4n |
*)

type token_tag = Ligo.nat
[@@deriving show]

let[@inline] kit_token_tag       = Ligo.nat_from_literal "1n"
let[@inline] lqt_token_tag       = Ligo.nat_from_literal "2n"
let[@inline] liq_auction_bid_tag = Ligo.nat_from_literal "3n"

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
