[@@@coverage off]

type vault_storage = { owner : Ligo.address; }
[@@deriving show]

type vault_parameter =
  | Vault_set_delegate of Ligo.key_hash option
  | Vault_receive_tez of unit
  | Vault_send_tez of (Ligo.tez * Ligo.address) (* TODO: flipped order to reuse tez_address_transaction *)
[@@deriving show]

[@@@coverage on]
