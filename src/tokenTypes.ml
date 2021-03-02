open LiquidationAuctionPrimitiveTypes

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

(* LIQUIDITY TOKENS *)

(* Note that we add an int, just to make sure that each token_content has a
 * different Michelson representation than the others. Otherwise one would not
 * be able to tell them apart, given that they are all issued by the same
 * contract (checker). The actual value of the int does not really matter, but
 * let's choose 63, arbitrarily. *)
type liquidity_token_content = Lqt of Ligo.int
[@@deriving show]

(* DELEGATION AUCTION BID TICKETS *)

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

(* LIQUIDATION AUCTION BID TICKETS *)

type liquidation_auction_bid_details = { auction_id: liquidation_auction_id; bid: bid; }
[@@deriving show]

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
