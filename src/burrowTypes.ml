open LiquidationAuctionTypes
[@@@coverage off]

type burrow_storage =
  { checker_address: Ligo.address;
    burrow_id: burrow_id
  }
[@@deriving show]

type burrow_parameter =
  | BurrowSetDelegate of Ligo.key_hash option
  | BurrowStoreTez
  | BurrowSendTezTo of (Ligo.tez * Ligo.address)
  | BurrowSendSliceToChecker of Ligo.tez
[@@deriving show]

[@@@coverage on]
