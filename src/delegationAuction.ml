open Common

type bid = { bidder: Ligo.address; cycle: Ligo.nat; amount: Ligo.tez }
[@@deriving show]

type bid_ticket = bid Ligo.ticket

let issue_bid_ticket (bid: bid) =
  Ligo.Tezos.create_ticket bid (Ligo.nat_from_literal 1)

type Error.error +=
  | BidTooLow
  | BidTicketExpired
  | CannotReclaimLeadingBid
  | CannotReclaimWinningBid
  | NotAWinningBid
  | InvalidDelegationAuctionTicket

(** Check whether a delegation auction bid ticket is valid. A delegation bid
  * ticket is valid if (a) it is issued by checker, (b) its amount is exactly 1
  * (avoids splitting it), and (c) is tagged appropriately. TODO: (c) is not
  * implemented yet. Perhaps it can be avoided, if all checker-issued tickets
  * end up having contents clearly distinguished by type. *)
let is_bid_ticket_valid
    ~(bid_ticket: bid_ticket)
  : (bid_ticket, Error.error) result =
  let (issuer, _bid_details, amount), same_ticket = Ligo.Tezos.read_ticket bid_ticket in
  let is_valid = issuer = Ligo.Tezos.self && amount = Ligo.nat_from_literal 1 in
  if is_valid then Ok same_ticket else Error InvalidDelegationAuctionTicket

let with_valid_bid_ticket
    ~(bid_ticket: bid_ticket)
    (f: bid_ticket -> ('a, Error.error) result)
  : ('a, Error.error) result =
  match is_bid_ticket_valid ~bid_ticket with
  | Error err -> Error err
  | Ok ticket -> f ticket

type t = { cycle: Ligo.nat; winner: bid option; leading_bid: bid option; delegate: Ligo.address option; }
[@@deriving show]

let empty = { cycle = level_to_cycle !Ligo.Tezos.level; winner = None; leading_bid = None; delegate = None; }

let cycle t = t.cycle

let winning_amount t = match t.winner with
  | None -> None
  | Some bid -> Some bid.amount

let touch (t: t) =
  let current_cycle = level_to_cycle !Ligo.Tezos.level in
  let cycles_elapsed = Ligo.sub_nat_nat current_cycle t.cycle in
  if cycles_elapsed = Ligo.int_from_literal 1 then
    (* We're on a new cycle, so reset state, and save the winner pending their claim. *)
    { cycle = current_cycle; winner = t.leading_bid; leading_bid = None; delegate = None; }
    (* TODO what if we somehow skip a level? *)
  else
    { t with cycle = current_cycle; }

let delegate t  =
  t.delegate

let place_bid t ~sender ~amount =
  let t = touch t in
  match t.leading_bid with
  | Some current when Ligo.leq_tez_tez amount current.amount ->
    Error BidTooLow
  | _ ->
    (* Either there is no bid or this is the highest *)
    let bid = {bidder=sender; cycle=t.cycle; amount=amount} in
    let ticket = issue_bid_ticket bid in
    Ok (ticket, {t with leading_bid = Some bid;})

(* If successful, it consumes the ticket. *)
(* TODO: allow winner to nominate a different address as the delegate? *)
let claim_win t ~bid_ticket =
  let t = touch t in
  with_valid_bid_ticket ~bid_ticket @@ fun bid_ticket ->
  let (_, bid, _), _ = Ligo.Tezos.read_ticket bid_ticket in
  if Some bid = t.winner then
    Ok { t with delegate = Some bid.bidder }
  else
    Error NotAWinningBid

(* If successful, it consumes the ticket. *)
let reclaim_bid t ~bid_ticket =
  let t = touch t in
  with_valid_bid_ticket ~bid_ticket @@ fun bid_ticket ->
  let (_, bid, _), _ = Ligo.Tezos.read_ticket bid_ticket in
  if Some bid = t.leading_bid then
    Error CannotReclaimLeadingBid
  else if Some bid = t.winner then
    Error CannotReclaimWinningBid
  else if Ligo.gt_int_int (Ligo.sub_nat_nat t.cycle bid.cycle) (Ligo.int_from_literal 1) then
    Error BidTicketExpired
  else
    Ok (bid.amount, t)
