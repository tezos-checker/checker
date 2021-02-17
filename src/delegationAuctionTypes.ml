open TokenTypes
open Common

type delegation_auction =
  { cycle: Ligo.nat;
    winner: delegation_auction_bid option;
    leading_bid: delegation_auction_bid option;
    delegate: Ligo.key_hash option;
  }
[@@deriving show]

let delegation_auction_empty = {
  cycle = level_to_cycle !Ligo.Tezos.level;
  winner = (None: delegation_auction_bid option);
  leading_bid = (None: delegation_auction_bid option);
  delegate = (None: Ligo.key_hash option);
}
