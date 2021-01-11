type bid = { bidder: Address.t; cycle: int; amount: Tez.t }
[@@deriving show]

type bid_ticket = bid Ticket.t

type Error.error +=
  | BidTooLow
  | BidTicketExpired
  | CannotReclaimLeadingBid
  | CannotReclaimWinningBid
  | NotAWinningBid

type t = { cycle: int; winner: bid option; leading_bid: bid option; delegate: Address.t option; }
[@@deriving show]

let empty (tezos: Tezos.t) = { cycle = Level.cycle tezos.level; winner = None; leading_bid = None; delegate = None; }

let cycle t = t.cycle

let winning_amount t = match t.winner with
  | None -> None
  | Some bid -> Some bid.amount

let touch (t: t) (tezos: Tezos.t) =
  let current_cycle = Level.cycle tezos.level in
  let cycles_elapsed = current_cycle - t.cycle in
  if cycles_elapsed = 1 then
    (* We're on a new cycle, so reset state, and save the winner pending their claim. *)
    { cycle = current_cycle; winner = t.leading_bid; leading_bid = None; delegate = None; }
    (* TODO what if we somehow skip a level? *)
  else
    { t with cycle = current_cycle; }

let delegate t  =
  t.delegate

let place_bid t (tezos: Tezos.t) ~sender ~amount =
  let t = touch t tezos in
  match t.leading_bid with
  | Some current when Tez.compare amount current.amount <= 0 ->
    Error BidTooLow
  | _ ->
    (* Either there is no bid or this is the highest *)
    let bid = {bidder=sender; cycle=t.cycle; amount=amount} in
    let ticket = Ticket.create ~issuer:tezos.self ~amount:Nat.one ~content:bid in
    Ok (ticket, {t with leading_bid = Some bid;})

(* TODO: allow winner to nominate a different address as the delegate? *)
let claim_win t tezos ~bid_ticket =
  let t = touch t tezos in
  let (_, _, bid, _) = Ticket.read bid_ticket in
  if Some bid = t.winner then
    Ok { t with delegate = Some bid.bidder }
  else
    Error NotAWinningBid

(* TODO use address *)
let reclaim_bid t tezos ~address:_ ~bid_ticket =
  let t = touch t tezos in
  let (_, _, bid, _) = Ticket.read bid_ticket in
  if Some bid = t.leading_bid then
    Error CannotReclaimLeadingBid
  else if Some bid = t.winner then
    Error CannotReclaimWinningBid
  else if t.cycle - bid.cycle > 1 then
    Error BidTicketExpired
  else
    Ok (bid.amount, t)

(* TODO check ticket amount *)
(* TODO check ticket issuer *)
