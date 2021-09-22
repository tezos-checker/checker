open Common

type vault_storage = { owner : Ligo.address; }

type vault_params =
  | Vault_set_delegate of Ligo.key_hash option
  | Vault_receive_tez of unit
  | Vault_send_tez of (Ligo.tez * Ligo.address) (* TODO: flipped order to reuse tez_address_transaction *)

let vault_main (p, storage: vault_params * vault_storage): LigoOp.operation list * vault_storage =
  match p with
  | Vault_set_delegate kho ->
    let _ = ensure_no_tez_given () in
    if !Ligo.Tezos.sender <> storage.owner then
      failwith "unauthorized" (* TODO: Add new error in error.ml *)
    else
      ([LigoOp.Tezos.set_delegate kho], storage)
  | Vault_receive_tez () ->
    (* TODO: allowed from everyone? *)
    ([], storage)
  | Vault_send_tez tz_recipient ->
    let _ = ensure_no_tez_given () in
    if !Ligo.Tezos.sender <> storage.owner then
      failwith "unauthorized" (* TODO: Add new error in error.ml *)
    else
      let tz, recipient = tz_recipient in
      (* TODO: Consider whether we want to have this behavior conflated into
       * Vault_send_tez, or if it's better to have two separate entrypoints. *)
      let op = match (LigoOp.Tezos.get_entrypoint_opt "%vault_receive_tez" recipient : unit Ligo.contract option) with
        | Some c ->
          LigoOp.Tezos.unit_transaction () tz c
        | None ->
          begin match (LigoOp.Tezos.get_contract_opt recipient : unit Ligo.contract option) with
            | Some c -> LigoOp.Tezos.unit_transaction () tz c
            | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *)
          end
      in
      ([op], storage)
