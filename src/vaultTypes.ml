[@@@coverage off]

type vault_storage = { owner : Ligo.address; }
[@@deriving show]

type vault_parameter =
  | Vault_set_delegate of Ligo.key_hash option
  | Vault_receive_tez of unit
  | Vault_send_tez_to_vault of (Ligo.tez * Ligo.address) (* AMENDMENT: flipped order to reuse tez_address_transaction *)
  | Vault_send_tez_to_contract of (Ligo.tez * Ligo.address) (* AMENDMENT: separate entrypoint for clarity. *)
[@@deriving show]

[@@@coverage on]
