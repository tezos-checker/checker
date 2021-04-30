open CheckerEntrypoints
open CheckerTypes
open Checker
open Error

type params =
  | DeployFunction of (lazy_function_id * Ligo.bytes)
  | SealContract of (Ligo.address * Ligo.address)
  | CheckerEntrypoint of checker_params

type deployment_state =
  | Unsealed of Ligo.address
  | Sealed of checker

type lazy_function_map = (lazy_function_id, Ligo.bytes) Ligo.big_map
type wrapper = lazy_function_map * deployment_state

(*
This is only for convenience, to actually create the storage just craft it manually by:

```
(Pair {} (Right "some_addr"))
```
*)
let initial_wrapper (addr: Ligo.address) =
  ((Ligo.Big_map.empty: (lazy_function_id, Ligo.bytes) Ligo.big_map), Unsealed addr)

(* BEGIN_LIGO
   let get_lazy_function (fnMap : lazy_function_map) (fnId: lazy_function_id) : lazy_function =
   match Ligo.Big_map.find_opt fnId fnMap with
   | Some bytes -> begin
      match (Ligo.Bytes.unpack bytes : lazy_function option) with
      | Some f -> f
      | None -> (failwith "lazy function unpack failure" : lazy_function)
    end
   | None -> (failwith "lazy function missing" : lazy_function)
   END_LIGO *)

let main (op, state: params * wrapper): LigoOp.operation list * wrapper =
  let lazy_functions, deployment_state = state in

  let ops, lazy_functions, deployment_state = match deployment_state with
    | Unsealed deployer ->
      let lazy_functions, deployment_state =
        if !Ligo.Tezos.sender = deployer
        then match op with
          | DeployFunction p ->
            let lfi, bs = p in
            let lazy_functions =
              match Ligo.Big_map.find_opt lfi lazy_functions with
              | None -> Ligo.Big_map.add lfi bs lazy_functions
              | Some prev -> Ligo.Big_map.add lfi (Ligo.Bytes.concat prev bs) lazy_functions in
            (lazy_functions, Unsealed deployer)
          | SealContract (oracle_addr, ctez_addr) ->
            let checker = initial_checker { oracle = oracle_addr; ctez = ctez_addr; } in
            (lazy_functions, Sealed checker)
          | CheckerEntrypoint _ ->
            (Ligo.failwith error_ContractNotDeployed: lazy_function_map * deployment_state)
        else (Ligo.failwith error_UnauthorisedCaller: lazy_function_map * deployment_state) in
      (([]: LigoOp.operation list), lazy_functions, deployment_state)
    | Sealed checker ->
      let ops, checker =
        match op with
        | DeployFunction _ -> (Ligo.failwith error_ContractAlreadyDeployed: LigoOp.operation list * checker)
        | SealContract _ -> (Ligo.failwith error_ContractAlreadyDeployed: LigoOp.operation list * checker)
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
      (ops, lazy_functions, Sealed checker)
  in
  (ops, (lazy_functions, deployment_state))
