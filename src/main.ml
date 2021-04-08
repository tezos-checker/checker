open CheckerEndpoints
open CheckerTypes
open Error

type params =
  (* Deployment *)
  | DeployFunction of (lazyFunctionId * Ligo.bytes)
  | SealContract
  | CheckerEndpoint of checker_params

type wrapper = checker * (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option
let initial_wrapper (addr: Ligo.address) =
  (initial_checker, (Ligo.Big_map.empty: (lazyFunctionId, Ligo.bytes) Ligo.big_map), Some addr)

let main (op_and_state: params * wrapper): LigoOp.operation list * wrapper =
  let op, state = op_and_state in
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
            | CheckerEndpoint _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
          else (Ligo.failwith error_UnauthorisedCaller: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option) in
        (([]: LigoOp.operation list), checker, lazy_functions, deployer)
      end
    | None -> failwith "argh"
  in

  (ops, (checker, lazy_functions, deployer))
