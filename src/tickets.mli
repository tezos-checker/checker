open Kit
open LiquidationAuctionPrimitiveTypes

type liquidation_auction_bid = { auction_id: liquidation_auction_id; bid: bid; }

type kit_token_content (* opaque *)
type kit_token = kit_token_content Ligo.ticket
val kit_issue : kit -> kit_token
val ensure_valid_kit_token : kit_token -> kit

type liquidity_token_content (* opaque *)
type liquidity = liquidity_token_content Ligo.ticket
val issue_liquidity_tokens : Ligo.nat -> liquidity
val ensure_valid_liquidity_token : liquidity -> Ligo.nat

type liquidation_auction_bid_content (* opaque *)
type liquidation_auction_bid_ticket = liquidation_auction_bid_content Ligo.ticket
val issue_liquidation_auction_bid_ticket : liquidation_auction_bid -> liquidation_auction_bid_ticket
val ensure_valid_liquidation_auction_bid_ticket : liquidation_auction_bid_ticket -> liquidation_auction_bid

(* BEGIN_OCAML *)
val show_kit_token_content : kit_token_content -> string
val pp_kit_token_content : Format.formatter -> kit_token_content -> unit

val show_kit_token : kit_token -> string
val pp_kit_token : Format.formatter -> kit_token -> unit

val show_liquidity_token_content : liquidity_token_content -> string
val pp_liquidity_token_content : Format.formatter -> liquidity_token_content -> unit

val show_liquidity : liquidity -> string
val pp_liquidity : Format.formatter -> liquidity -> unit

val show_liquidation_auction_bid_content : liquidation_auction_bid_content -> string
val pp_liquidation_auction_bid_content : Format.formatter -> liquidation_auction_bid_content -> unit

val show_liquidation_auction_bid : liquidation_auction_bid -> string
val pp_liquidation_auction_bid : Format.formatter -> liquidation_auction_bid -> unit
(* END_OCAML *)
