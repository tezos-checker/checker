
type storage = { owner : Ligo.address; }

type params =
  | Set_delegate of Ligo.key_hash option
  | Receive_tez of unit
  | Send_tez of Ligo.address * Ligo.tez

let main (p, storage: params * storage): Ligo.Op.operation list * storage =
  match p with
  | Set_delegate kho ->
    let _ = ensure_no_tez_given () in
    if !Ligo.Tezos.sender <> state.owner then
      failwith "unauthorized"
    else
      ([LigoOp.Tezos.set_delegate kho], storage)
  | Receive_tez () ->
    (* TODO: allowed from everyone? *)
    ([], storage)
  | Send_tez recipient_tz ->
    let _ = ensure_no_tez_given () in
    if !Ligo.Tezos.sender <> state.owner then
      failwith "unauthorized"
    else
      let recipient, tz = recipient_tz in
      (* TODO: Consider whether we want to have this behavior conflated into
       * Send_tez, or if it's better to have two separate entrypoints. *)
      let op = match (LigoOp.Tezos.get_entrypoint_opt "%receive_tez" recipient : unit Ligo.contract option) with
        | Some c ->
          LigoOp.Tezos.address_unit_transaction () tz c
        | None ->
          begin match (LigoOp.Tezos.get_contract_opt recipient : unit Ligo.contract option) with
            | Some c -> LigoOp.Tezos.unit_transaction () tz c
            | None -> (failwith "failure" : Ligo.Op.operation)
          end
      in
      ([op], storage)
