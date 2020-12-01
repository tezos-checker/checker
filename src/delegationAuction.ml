type bid = { bidder: Address.t; amount: Tez.t }
[@@deriving show]

type bid_ticket = bid Ticket.t

type Error.error +=
  | BidTooLow
  | CannotReclaimLeadingBid
  | CannotReclaimWinningBid

type t = { level: Level.t; winner: bid option; leading_bid: bid option }
[@@deriving show]

let empty (tezos: Tezos.t) = { level = tezos.level; winner = None; leading_bid = None }

let touch (t: t) (tezos: Tezos.t) =
  let cycles_elapsed = Level.cycle tezos.level - Level.cycle t.level in
  if cycles_elapsed = 1 then
    (* TODO punch ticket *)
    { level = tezos.level; winner = t.leading_bid; leading_bid = None; }
    (* TODO what if we somehow skip a level? *)
  else
    { t with level = tezos.level; }

let delegate t tezos =
  let t = touch t tezos in
  (Option.map (fun x -> x.bidder) t.winner, t)

let place_bid t (tezos:Tezos.t) ~sender ~amount =
  let t = touch t tezos in
  match t.leading_bid with
  | Some current when Tez.compare amount current.amount <= 0 ->
    Error BidTooLow
  | _ ->
    (* Either there is no bid or this is the highest *)
    let bid = {bidder=sender; amount=amount} in
    let ticket = Ticket.create ~issuer:tezos.self ~amount:1 ~content:bid in
    Ok (ticket, {t with leading_bid = Some bid;})

(* TODO use address *)
let reclaim_bid t ~address:_ ~bid_ticket =
  let (_, _, bid, _) = Ticket.read bid_ticket in
  if Some bid = t.leading_bid then
    Error CannotReclaimLeadingBid
  else if Some bid = t.winner then
    Error CannotReclaimWinningBid
  else
    (* TODO punch ticket *)
    Ok bid.amount
