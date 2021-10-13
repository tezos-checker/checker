[@@@coverage off]

type burrow_storage =
  { checker_address: Ligo.address;
    collateral_fa2: Ligo.address;
  }
[@@deriving show]

type burrow_parameter =
  | BurrowSetDelegate of Ligo.key_hash option
  | BurrowTransfer of (Ligo.address * Ligo.nat)
[@@deriving show]

[@@@coverage on]
