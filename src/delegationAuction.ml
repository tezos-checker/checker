open Common
open TokenTypes
open DelegationAuctionTypes

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
  (bid, {t with leading_bid = Some bid;})

let same_delegation_auction_bid (t1: delegation_auction_bid option) (t2: delegation_auction_bid) =
  match t1 with
  | None -> false
  | Some t1 ->
    let { bidder=b1; cycle=c1; amount=a1 } = t1 in
    let { bidder=b2; cycle=c2; amount=a2 } = t2 in
    b1 = b2 && c1 = c2 && a1 = a2

(* If successful, it consumes the ticket. *)
let delegation_auction_claim_win (t: delegation_auction) (bid: delegation_auction_bid) (for_delegate: Ligo.key_hash) =
  let t = delegation_auction_touch t in
  if same_delegation_auction_bid t.winner bid
  then { t with delegate = Some for_delegate }
  else (failwith "NotAWinningBid": delegation_auction)

(* If successful, it consumes the ticket. *)
let delegation_auction_reclaim_bid (t: delegation_auction) (bid: delegation_auction_bid) =
  let t = delegation_auction_touch t in
  if same_delegation_auction_bid t.leading_bid bid then
    (failwith "CannotReclaimLeadingBid": Ligo.tez * delegation_auction)
  else if same_delegation_auction_bid t.winner bid then
    (failwith "CannotReclaimWinningBid": Ligo.tez * delegation_auction)
  else if Ligo.gt_int_int (Ligo.sub_nat_nat t.cycle bid.cycle) (Ligo.int_from_literal "1") then
    (failwith "BidTicketExpired": Ligo.tez * delegation_auction)
  else
    (bid.amount, t)
