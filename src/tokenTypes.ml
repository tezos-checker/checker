open Kit
open LiquidationAuctionTypes
open Ptr
open Common

(* KIT TOKENS *)

(* Kit are really tickets. *)
type kit_token_content = Kit [@@deriving show]
type kit_token = kit_token_content Ligo.ticket [@@deriving show]

let kit_issue (kit: kit) : kit_token = Ligo.Tezos.create_ticket (Kit) (kit_to_mukit kit)

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let assert_valid_kit_token (token: kit_token) : kit_token =
  let (issuer, (_content, _amnt)), same_ticket = Ligo.Tezos.read_ticket token in
  let is_valid = issuer = checker_address in (* TODO: amnt > Nat.zero perhaps? *)
  if is_valid
  then same_ticket
  else (failwith "InvalidKitToken": kit_token)

let read_kit (token: kit_token) : kit * kit_token =
  let (_issuer, (_content, mukit)), same_token = Ligo.Tezos.read_ticket token in
  (kit_of_mukit mukit, same_token)

let kit_split_or_fail (token: kit_token) (left: kit) (right: kit) : kit_token * kit_token =
  match Ligo.Tezos.split_ticket token (kit_to_mukit left, kit_to_mukit right) with
  | Some a -> a
  | None -> (failwith "split_or_fail: failed": kit_token * kit_token)

let kit_join_or_fail (left: kit_token) (right: kit_token) : kit_token =
  match Ligo.Tezos.join_tickets (left, right) with
  | Some a -> a
  | None -> (failwith "join_or_fail: failed": kit_token)

(* LIQUIDITY TOKENS *)

type liquidity_token_content = Lqt
[@@deriving show]

(* DELEGATION AUCTION BID TICKETS *)

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

(* LIQUIDATION AUCTION BID TICKETS *)

type liquidation_auction_bid_details = { auction_id: liquidation_auction_id; bid: bid; }
[@@deriving show]

(* PERMISSION TICKETS *)

(* todo: please find a better name for this. *)
type right_collection =
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
type right =
  | Admin
  | User of right_collection
[@@deriving show]

type permission_content = right * ptr * Ligo.nat
[@@deriving show]

