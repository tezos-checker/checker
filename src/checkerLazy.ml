open CheckerEndpoints
open CheckerTypes
open Checker
open Error

type params =
  (* Deployment *)
  | DeployFunction of (lazy_function_id * Ligo.bytes)
  | SealContract
  | CheckerEndpoint of checker_params

type lazy_function_map = (lazy_function_id, Ligo.bytes) Ligo.big_map
type wrapper = checker * lazy_function_map * Ligo.address option

let initial_wrapper (addr: Ligo.address) =
  (initial_checker, (Ligo.Big_map.empty: (lazy_function_id, Ligo.bytes) Ligo.big_map), Some addr)

let get_lazy_function (fnMap : lazy_function_map) (fnId: lazy_function_id) : lazy_function =
  (* BEGIN_LIGO
     match Ligo.Big_map.find_opt fnId fnMap with
     | Some bytes -> begin
        match (Ligo.Bytes.unpack bytes : lazy_function option) with
        | Some f -> f
        | None -> (failwith "lazy function unpack failure" : lazy_function)
      end
     | None -> (failwith "lazy function missing" : lazy_function)
     END_LIGO *)
  (* BEGIN_OCAML *)
  lookUpLazyFunction fnId
(* END_OCAML *)

let main (op, state: params * wrapper): LigoOp.operation list * wrapper =
  let checker, lazy_functions, deployer = state in

  let ops, checker, lazy_functions, deployer = match deployer with
    | Some deployer -> begin
        let lazy_functions, deployer =
          if !Ligo.Tezos.sender = deployer
          then match op with
            | DeployFunction p ->
              let lfi, bs = p in
              let lazy_functions =
                match Ligo.Big_map.find_opt lfi lazy_functions with
                | None -> Ligo.Big_map.add lfi bs lazy_functions
                | Some prev -> Ligo.Big_map.add lfi (Ligo.Bytes.concat prev bs) lazy_functions in
              (lazy_functions, Some deployer)
            | SealContract ->
              (lazy_functions, (None: Ligo.address option))
            (* we really need wildcard patterns... *)
            | CheckerEndpoint _ -> (Ligo.failwith error_ContractNotDeployed: (lazy_function_id, Ligo.bytes) Ligo.big_map * Ligo.address option)
          else (Ligo.failwith error_UnauthorisedCaller: (lazy_function_id, Ligo.bytes) Ligo.big_map * Ligo.address option) in
        (([]: LigoOp.operation list), checker, lazy_functions, deployer)
      end
    | None ->
      let ops, checker =
        match op with
        | DeployFunction _ -> (Ligo.failwith error_ContractAlreadyDeployed: LigoOp.operation list * checker)
        | SealContract -> (Ligo.failwith error_ContractAlreadyDeployed: LigoOp.operation list * checker)
        | CheckerEndpoint op ->
          let fid, op = checkerParamsToLazyFunctionId op in
          (get_lazy_function lazy_functions fid) (checker, op) in
      (ops, checker, lazy_functions, deployer)
  in
  (ops, (checker, lazy_functions, deployer))
