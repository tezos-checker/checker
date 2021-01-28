open Common

type delegation_auction_bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

type delegation_auction_bid_ticket = delegation_auction_bid Ligo.ticket

let issue_delegation_auction_bid_ticket (bid: delegation_auction_bid) =
  Ligo.Tezos.create_ticket bid (Ligo.nat_from_literal "1n")

(** Ensure that a delegation auction bid ticket is valid. A delegation bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. TODO: (c) is not
  * implemented yet. Perhaps it can be avoided, if all checker-issued tickets
  * end up having contents clearly distinguished by type. *)
let assert_valid_delegation_auction_bid_ticket
    (bid_ticket: delegation_auction_bid_ticket)
  : delegation_auction_bid_ticket =
  let (issuer, (_, amt)), same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid = issuer = checker_address && amt = Ligo.nat_from_literal "1n" in
  if is_valid
  then same_ticket
  else (failwith "InvalidDelegationAuctionTicket": delegation_auction_bid_ticket)

type delegation_auction =
  { cycle: Ligo.nat;
    winner: delegation_auction_bid option;
    leading_bid: delegation_auction_bid option;
    delegate: Ligo.key_hash option;
  }
[@@deriving show]

let delegation_auction_empty = {
  cycle = level_to_cycle !Ligo.tezos_level;
  winner = (None: delegation_auction_bid option);
  leading_bid = (None: delegation_auction_bid option);
  delegate = (None: Ligo.key_hash option);
}

let delegation_auction_cycle (t: delegation_auction) : Ligo.nat = t.cycle

let delegation_auction_winning_amount (t: delegation_auction) : Ligo.tez option =
  match t.winner with
  | None -> (None: Ligo.tez option)
  | Some bid -> Some bid.amount

let delegation_auction_touch (t: delegation_auction) =
  let current_cycle = level_to_cycle !Ligo.tezos_level in
  let cycles_elapsed = Ligo.sub_nat_nat current_cycle t.cycle in
  if cycles_elapsed = Ligo.int_from_literal "1" then
    (* We're on a new cycle, so reset state, and save the winner pending their claim. *)
    { cycle = current_cycle; winner = t.leading_bid;
      leading_bid = (None: delegation_auction_bid option);
      delegate = (None: Ligo.key_hash option); }
    (* TODO what if we somehow skip a level? *)
  else
    { t with cycle = current_cycle; }

let delegation_auction_delegate (t: delegation_auction) = t.delegate

let delegation_auction_place_bid (t: delegation_auction) (sender_address: Ligo.address) (amt: Ligo.tez) =
  let t = delegation_auction_touch t in
  let _ = match t.leading_bid with
    | Some current ->
      if Ligo.leq_tez_tez amt current.amount
      then failwith "BidTooLow"
      else ()
    | None -> () in
  (* Either there is no bid or this is the highest *)
  let bid = {bidder=sender_address; cycle=t.cycle; amount=amt} in
  let ticket = issue_delegation_auction_bid_ticket bid in
  (ticket, {t with leading_bid = Some bid;})

let same_delegation_auction_bid (t1: delegation_auction_bid option) (t2: delegation_auction_bid) =
  match t1 with
  | None -> false
  | Some t1 ->
    let { bidder=b1; cycle=c1; amount=a1 } = t1 in
    let { bidder=b2; cycle=c2; amount=a2 } = t2 in
    b1 = b2 && c1 = c2 && a1 = a2

(* If successful, it consumes the ticket. *)
let delegation_auction_claim_win (t: delegation_auction) (bid_ticket: delegation_auction_bid_ticket) (for_delegate: Ligo.key_hash) =
  let t = delegation_auction_touch t in
  let bid_ticket = assert_valid_delegation_auction_bid_ticket bid_ticket in
  let (_, (bid, _)), _ = Ligo.Tezos.read_ticket bid_ticket in
  if same_delegation_auction_bid t.winner bid
  then { t with delegate = Some for_delegate }
  else (failwith "NotAWinningBid": delegation_auction)

(* If successful, it consumes the ticket. *)
let delegation_auction_reclaim_bid (t: delegation_auction) (bid_ticket: delegation_auction_bid_ticket) =
  let t = delegation_auction_touch t in
  let bid_ticket = assert_valid_delegation_auction_bid_ticket bid_ticket in
  let (_, (bid, _)), _ = Ligo.Tezos.read_ticket bid_ticket in
  if same_delegation_auction_bid t.leading_bid bid then
    (failwith "CannotReclaimLeadingBid": Ligo.tez * delegation_auction)
  else if same_delegation_auction_bid t.winner bid then
    (failwith "CannotReclaimWinningBid": Ligo.tez * delegation_auction)
  else if Ligo.gt_int_int (Ligo.sub_nat_nat t.cycle bid.cycle) (Ligo.int_from_literal "1") then
    (failwith "BidTicketExpired": Ligo.tez * delegation_auction)
  else
    (bid.amount, t)
