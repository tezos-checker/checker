open LiquidationAuctionPrimitiveTypes
open Common
open Error

(*
Ticket-based entitities in checker and their expected value/mechanics:

| Ticket           | Multiplicity | (Usably) Splittable              | Tag |
|------------------|--------------|----------------------------------|-----|
| liq. auction bid | always one   | No  (zero is useless)            |  4n |
*)

type token_tag = Ligo.nat
[@@deriving show]

let[@inline] liq_auction_bid_tag = Ligo.nat_from_literal "3n"

(* ************************************************************************* *)
(**                               GENERAL                                    *)
(* ************************************************************************* *)

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
