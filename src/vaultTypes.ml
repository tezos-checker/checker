[@@@coverage off]

type vault_storage = { owner : Ligo.address; }
[@@deriving show]

type vault_parameter =
  | Vault_set_delegate of Ligo.key_hash option
  | Vault_receive_tez of unit
  | Vault_send_tez_to_vault of (Ligo.tez * Ligo.address)
  | Vault_send_tez_to_contract of (Ligo.tez * Ligo.address)
[@@deriving show]

[@@@coverage on]
