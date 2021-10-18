open CheckerEntrypoints
open CheckerTypes
open Checker
open Fa2Interface
open Error
open Common

(* We can not serialize all of our parameters, since `Balance_of` contains a `contract`. So, we split
 * up parameters we can not serialize here.
*)
type strict_params =
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list

type checker_params =
  | LazyParams of lazy_params
  | StrictParams of strict_params

type params =
  | DeployFunction of (lazy_function_id * Ligo.bytes)
  | DeployMetadata of Ligo.bytes
  | SealContract of (Ligo.address * Ligo.address * Ligo.address)
  | CheckerEntrypoint of checker_params

(*
This is only for convenience, to actually create the storage just craft it manually by:

```
(Pair {} (Pair {} (Right "some_addr")))
```
*)

let initial_wrapper (addr: Ligo.address) =
  { lazy_functions = (Ligo.Big_map.empty: (lazy_function_id, Ligo.bytes) Ligo.big_map)
  ; metadata = (Ligo.Big_map.empty: (string, Ligo.bytes) Ligo.big_map)
  ; deployment_state = Unsealed addr
  }

(* BEGIN_LIGO
   let get_lazy_function (fnMap : lazy_function_map) (fnId: lazy_function_id) : lazy_function =
   match Ligo.Big_map.find_opt fnId fnMap with
   | Some bytes -> begin
      match (Ligo.Bytes.unpack bytes : lazy_function option) with
      | Some f -> f
      | None -> (failwith error_GetLazyFunctionUnpackFailure : lazy_function)
    end
   | None -> (failwith error_GetLazyFunctionMissingFunction : lazy_function)
   END_LIGO *)

let main (op, state: params * wrapper): LigoOp.operation list * wrapper =
  let _ = ensure_no_tez_given () in

  let { lazy_functions = lazy_functions; metadata = metadata; deployment_state = deployment_state } = state in

  let ops, lazy_functions, metadata, deployment_state = match deployment_state with
    | Unsealed deployer ->
      begin match !Ligo.Tezos.sender = deployer with
        | true ->
          begin match op with
            | DeployFunction p ->
              let lfi, bs = p in
              let lazy_functions =
                match Ligo.Big_map.find_opt lfi lazy_functions with
                | None -> Ligo.Big_map.add lfi bs lazy_functions
                | Some prev -> Ligo.Big_map.add lfi (Ligo.Bytes.concat prev bs) lazy_functions in
              (([]: LigoOp.operation list), lazy_functions, metadata, Unsealed deployer)
            | DeployMetadata bs ->
              let metadata =
                match Ligo.Big_map.find_opt "m" metadata with
                | None -> Ligo.Big_map.add "m" bs metadata
                | Some prev -> Ligo.Big_map.add "m" (Ligo.Bytes.concat prev bs) metadata in
              (([]: LigoOp.operation list), lazy_functions, metadata, Unsealed deployer)
            | SealContract (oracle_addr, collateral_fa2_addr, ctez_addr) ->
              let external_contracts = { oracle = oracle_addr; collateral_fa2 = collateral_fa2_addr; ctez = ctez_addr; } in

              (* check if the given oracle, collateral_fa2, and ctez contracts have the entrypoints we need *)
              let _ = get_oracle_entrypoint external_contracts in
              let _ = get_transfer_collateral_fa2_entrypoint external_contracts in
              let _ = get_transfer_ctez_entrypoint external_contracts in

              (* emit a touch operation to checker *)
              let touchOp =
                match (LigoOp.Tezos.get_entrypoint_opt "%touch" !Ligo.Tezos.self_address: unit Ligo.contract option) with
                | Some c -> LigoOp.Tezos.unit_transaction () (Ligo.tez_from_literal "0mutez") c
                | None -> (Ligo.failwith (Ligo.int_from_literal "-4") : LigoOp.operation) in

              (* initialize checker state *)
              let checker = initial_checker external_contracts in

              (* add the metadata boilerplate *)
              (* Python: b"tezos-storage:m".hex() *)
              let metadata_url = Ligo.bytes_from_literal "0x74657a6f732d73746f726167653a6d" in
              let metadata = Ligo.Big_map.add "" metadata_url metadata in

              ([touchOp], lazy_functions, metadata, Sealed checker)
            | CheckerEntrypoint _ ->
              (* Note: disabling coverage for the unreported but accessed right-hand side;
               * accessibility is sufficiently marked on the pattern itself. *)
              ((Ligo.failwith error_ContractNotDeployed [@coverage off]): LigoOp.operation list * lazy_function_map * (string, Ligo.bytes) Ligo.big_map * deployment_state)
          end
        | false ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((Ligo.failwith error_UnauthorisedCaller [@coverage off]): LigoOp.operation list * lazy_function_map * (string, Ligo.bytes) Ligo.big_map * deployment_state)
      end
    | Sealed checker ->
      let ops, checker =
        match op with
        | DeployFunction _ ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((Ligo.failwith error_ContractAlreadyDeployed [@coverage off]): LigoOp.operation list * checker)
        | SealContract _ ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((Ligo.failwith error_ContractAlreadyDeployed [@coverage off]): LigoOp.operation list * checker)
        | DeployMetadata _ ->
          (* Note: disabling coverage for the unreported but accessed right-hand side;
           * accessibility is sufficiently marked on the pattern itself. *)
          ((Ligo.failwith error_ContractAlreadyDeployed [@coverage off]): LigoOp.operation list * checker)
          [@coverage off]
        | CheckerEntrypoint op -> begin
            match op with
            | StrictParams op -> begin
                match op with
                | Balance_of p -> strict_entrypoint_balance_of (checker, p)
                | Transfer p -> strict_entrypoint_transfer (checker, p)
              end
            | LazyParams op ->
              (* BEGIN_LIGO
                 let fid, params = lazyParamsToLazyFunctionId op in
                 (get_lazy_function lazy_functions fid) (checker, params)
                 END_LIGO *)
              (* BEGIN_OCAML *)
              runLazyParams op checker
              (* END_OCAML *)
          end in
      (ops, lazy_functions, metadata, Sealed checker)
  in
  (ops, { lazy_functions = lazy_functions; metadata = metadata; deployment_state = deployment_state })
