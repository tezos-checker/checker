open Common

type storage = { owner : Ligo.address; }

type params =
  | Set_delegate of Ligo.key_hash option
  | Receive_tez of unit
  | Send_tez of (Ligo.tez * Ligo.address) (* TODO: flipped order to reuse tez_address_transaction *)

let main (p, storage: params * storage): LigoOp.operation list * storage =
  match p with
  | Set_delegate kho ->
    let _ = ensure_no_tez_given () in
    if !Ligo.Tezos.sender <> storage.owner then
      failwith "unauthorized" (* TODO: Add new error in error.ml *)
    else
      ([LigoOp.Tezos.set_delegate kho], storage)
  | Receive_tez () ->
    (* TODO: allowed from everyone? *)
    ([], storage)
  | Send_tez tz_recipient ->
    let _ = ensure_no_tez_given () in
    if !Ligo.Tezos.sender <> storage.owner then
      failwith "unauthorized" (* TODO: Add new error in error.ml *)
    else
      let tz, recipient = tz_recipient in
      (* TODO: Consider whether we want to have this behavior conflated into
       * Send_tez, or if it's better to have two separate entrypoints. *)
      let op = match (LigoOp.Tezos.get_entrypoint_opt "%receive_tez" recipient : unit Ligo.contract option) with
        | Some c ->
          LigoOp.Tezos.unit_transaction () tz c
        | None ->
          begin match (LigoOp.Tezos.get_contract_opt recipient : unit Ligo.contract option) with
            | Some c -> LigoOp.Tezos.unit_transaction () tz c
            | None -> (failwith "failure" : LigoOp.operation) (* TODO: Add new error in error.ml *)
          end
      in
      ([op], storage)
