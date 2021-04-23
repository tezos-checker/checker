open Kit
open LiquidationAuctionPrimitiveTypes

type liquidation_auction_bid = { auction_id: liquidation_auction_id; bid: bid; }

type liquidation_auction_bid_content (* opaque *)
type liquidation_auction_bid_ticket = liquidation_auction_bid_content Ligo.ticket
val issue_liquidation_auction_bid_ticket : liquidation_auction_bid -> liquidation_auction_bid_ticket
val ensure_valid_liquidation_auction_bid_ticket : liquidation_auction_bid_ticket -> liquidation_auction_bid

(* BEGIN_OCAML *)
val show_liquidation_auction_bid_content : liquidation_auction_bid_content -> string
val pp_liquidation_auction_bid_content : Format.formatter -> liquidation_auction_bid_content -> unit

val show_liquidation_auction_bid : liquidation_auction_bid -> string
val pp_liquidation_auction_bid : Format.formatter -> liquidation_auction_bid -> unit
(* END_OCAML *)
