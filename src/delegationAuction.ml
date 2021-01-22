open Common

type bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

type bid_ticket = bid Ligo.ticket

let issue_bid_ticket (bid: bid) =
  Ligo.Tezos.create_ticket bid (Ligo.nat_from_literal "1n")

(** Check whether a delegation auction bid ticket is valid. A delegation bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. TODO: (c) is not
  * implemented yet. Perhaps it can be avoided, if all checker-issued tickets
  * end up having contents clearly distinguished by type. *)
let is_bid_ticket_valid
    (bid_ticket: bid_ticket)
  : bid_ticket option =
  let (issuer, (_, amt)), same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid = issuer = Ligo.Tezos.self_address && amt = Ligo.nat_from_literal "1n" in
  if is_valid then Some same_ticket else (None: bid_ticket option)

let assert_valid_bid_ticket
    (bid_ticket: bid_ticket)
  : bid_ticket =
  match is_bid_ticket_valid bid_ticket with
  | None -> (failwith "InvalidDelegationAuctionTicket": bid_ticket)
  | Some ticket -> ticket

type t = { cycle: Ligo.nat; winner: bid option; leading_bid: bid option; delegate: Ligo.address option; }
[@@deriving show]

let empty = {
  cycle = level_to_cycle !Ligo.tezos_level;
  winner = (None: bid option);
  leading_bid = (None: bid option);
  delegate = (None: Ligo.address option);
}

let cycle (t: t) : Ligo.nat = t.cycle

let winning_amount (t: t) = match t.winner with
  | None -> (None: Ligo.tez option)
  | Some bid -> Some bid.amount

let touch (t: t) =
  let current_cycle = level_to_cycle !Ligo.tezos_level in
  let cycles_elapsed = Ligo.sub_nat_nat current_cycle t.cycle in
  if cycles_elapsed = Ligo.int_from_literal "1" then
    (* We're on a new cycle, so reset state, and save the winner pending their claim. *)
    { cycle = current_cycle; winner = t.leading_bid;
      leading_bid = (None: bid option);
      delegate = (None: Ligo.address option); }
    (* TODO what if we somehow skip a level? *)
  else
    { t with cycle = current_cycle; }

let delegate (t: t)  =
  t.delegate

let place_bid (t: t) (sender_address: Ligo.address) (amt: Ligo.tez) =
  let t = touch t in
  let _ = match t.leading_bid with
    | Some current ->
      if Ligo.leq_tez_tez amt current.amount
      then failwith "BidTooLow"
      else ()
    | None -> () in
  (* Either there is no bid or this is the highest *)
  let bid = {bidder=sender_address; cycle=t.cycle; amount=amt} in
  let ticket = issue_bid_ticket bid in
  (ticket, {t with leading_bid = Some bid;})

let same_bid (t1: bid option) (t2: bid) =
  match t1 with
  | None -> false
  | Some t1 ->
    let { bidder=b1; cycle=c1; amount=a1 } = t1 in
    let { bidder=b2; cycle=c2; amount=a2 } = t2 in
    b1 = b2 && c1 = c2 && a1 = a2

(* If successful, it consumes the ticket. *)
(* TODO: allow winner to nominate a different address as the delegate? *)
let claim_win (t: t) (bid_ticket: bid_ticket) =
  let t = touch t in
  let bid_ticket = assert_valid_bid_ticket bid_ticket in
  let (_, (bid, _)), _ = Ligo.Tezos.read_ticket bid_ticket in
  if same_bid t.winner bid
  then { t with delegate = Some bid.bidder }
  else (failwith "NotAWinningBid": t)

(* If successful, it consumes the ticket. *)
let reclaim_bid (t: t) (bid_ticket: bid_ticket) =
  let t = touch t in
  let bid_ticket = assert_valid_bid_ticket bid_ticket in
  let (_, (bid, _)), _ = Ligo.Tezos.read_ticket bid_ticket in
  if same_bid t.leading_bid bid then
    (failwith "CannotReclaimLeadingBid": Ligo.tez * t)
  else if same_bid t.winner bid then
    (failwith "CannotReclaimWinningBid": Ligo.tez * t)
  else if Ligo.gt_int_int (Ligo.sub_nat_nat t.cycle bid.cycle) (Ligo.int_from_literal "1") then
    (failwith "BidTicketExpired": Ligo.tez * t)
  else
    (bid.amount, t)
