open CheckerEndpoints

type params =
  (* Deployment *)
  | DeployFunction of (lazyFunctionId * Ligo.bytes)
  | SealContract
      CheckerEndpoint of params

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
            | ActivateBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | AddLiquidity _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | BurnKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | BuyKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | CancelSliceLiquidation _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | CreateBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DeactivateBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DelegationAuctionClaimWin _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DelegationAuctionPlaceBid -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DelegationAuctionReclaimBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DepositTez _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | InvalidateAllPermissions _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | LiqAuctionPlaceBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | LiqAuctionReclaimBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | LiqAuctionReclaimWinningBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | MakePermission _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | MarkBurrowForLiquidation _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | MintKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | ReceiveLiquidationSlice -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | ReceivePrice _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | RemoveLiquidity _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | SellKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | SetBurrowDelegate _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | Touch -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | TouchBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | TouchLiquidationSlices _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | WithdrawTez _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
          else (Ligo.failwith error_UnauthorisedCaller: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option) in
        (([]: LigoOp.operation list), checker, lazy_functions, deployer)
      end
    | None -> failwith "argh"
  in

  (ops, (checker, lazy_functions, deployer))
