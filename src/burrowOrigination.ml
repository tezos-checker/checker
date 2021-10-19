open BurrowTypes
open CheckerTypes
open Fa2Interface

let[@inline] originate_burrow (state: checker) (delegate_opt: Ligo.key_hash option) : LigoOp.operation * Ligo.address =
  LigoOp.Tezos.burrow_create_contract
    (fun (p, storage : burrow_parameter * burrow_storage) ->
       if !Ligo.Tezos.sender <> storage.checker_address then
         (Ligo.failwith (Ligo.int_from_literal "-1") : LigoOp.operation list * burrow_storage)
       else if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez" then
         (Ligo.failwith (Ligo.int_from_literal "-2") : LigoOp.operation list * burrow_storage)
       else
         match p with
         | BurrowSetDelegate kho ->
           (* NOTE: this deviates slightly from the design in the issue. *)
           let op = match (LigoOp.Tezos.get_entrypoint_opt "%set_delegate" storage.collateral_fa2 : Ligo.key_hash option Ligo.contract option) with
             | Some c -> LigoOp.Tezos.opt_key_hash_transaction kho (Ligo.tez_from_literal "0mutez") c
             | None -> (Ligo.failwith (Ligo.int_from_literal "-3") : LigoOp.operation) in (* i.e., set_delegate not supported *)
           ([op], storage)
         | BurrowTransfer p ->
           let (addr, amnt) = p in
           let transfer =
             { from_ = !Ligo.Tezos.self_address; (* from: FA2 account of burrow contract *)
               txs = [
                 { to_ = addr;                   (* to: FA2 account of the given address *)
                   token_id = Ligo.nat_from_literal "2n"; (* FIXME: THIS IS GREAT; collateral_token_id IS NOT ALLOWED HERE *)
                   amount = amnt;
                 }
               ];
             } in
           let fa2_transfer_contract =
             match (LigoOp.Tezos.get_entrypoint_opt "%transfer" storage.collateral_fa2 : (fa2_transfer list) Ligo.contract option) with
             | Some c -> c
             | None -> (Ligo.failwith (Ligo.int_from_literal "-4") : (fa2_transfer list) Ligo.contract) in
           let op =
             LigoOp.Tezos.fa2_transfer_transaction
               [transfer] (Ligo.tez_from_literal "0mutez")
               fa2_transfer_contract in
           ([op], storage)
    )
    delegate_opt
    (Ligo.tez_from_literal "0mutez") (* initially no collateral in it *)
    { checker_address = !Ligo.Tezos.self_address;
      collateral_fa2 = state.external_contracts.collateral_fa2;
    }
