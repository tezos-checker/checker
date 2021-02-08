type burrow_storage = Ligo.address
[@@deriving show]

type burrow_parameter =
  | BurrowSetDelegate of Ligo.key_hash option
  | BurrowStoreTez
  | BurrowSendTezTo of (Ligo.tez * Ligo.address)
  | BurrowSendSliceToChecker of Ligo.tez
[@@deriving show]
