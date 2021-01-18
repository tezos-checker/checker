
let show_error (e: Error.error) : string =
  match e with
  (* Delegation auction errors *)
  | DelegationAuction.BidTooLow                        -> "DelegationAuction.BidTooLow"
  | DelegationAuction.BidTicketExpired                 -> "DelegationAuction.BidTicketExpired"
  | DelegationAuction.CannotReclaimLeadingBid          -> "DelegationAuction.CannotReclaimLeadingBid"
  | DelegationAuction.CannotReclaimWinningBid          -> "DelegationAuction.CannotReclaimWinningBid"
  | DelegationAuction.NotAWinningBid                   -> "DelegationAuction.NotAWinningBid"
  (* Uniswap errors *)
  | Uniswap.UniswapNonPositiveInput                    -> "Uniswap.UniswapNonPositiveInput"
  | Uniswap.UniswapTooLate                             -> "Uniswap.UniswapTooLate"
  | Uniswap.AddLiquidityNoTezGiven                     -> "Uniswap.AddLiquidityNoTezGiven"
  | Uniswap.AddLiquidityNoKitGiven                     -> "Uniswap.AddLiquidityNoKitGiven"
  | Uniswap.AddLiquidityNoLiquidityToBeAdded           -> "Uniswap.AddLiquidityNoLiquidityToBeAdded"
  | Uniswap.AddLiquidityLessThanOneTez                 -> "Uniswap.AddLiquidityLessThanOneTez"
  | Uniswap.AddLiquidityTooLowLiquidityMinted          -> "Uniswap.AddLiquidityTooLowLiquidityMinted"
  | Uniswap.AddLiquidityTooMuchKitRequired             -> "Uniswap.AddLiquidityTooMuchKitRequired"
  | Uniswap.AddLiquidityZeroKitDeposited               -> "Uniswap.AddLiquidityZeroKitDeposited"
  | Uniswap.RemoveLiquidityNonEmptyAmount              -> "Uniswap.RemoveLiquidityNonEmptyAmount"
  | Uniswap.RemoveLiquidityCantWithdrawEnoughTez       -> "Uniswap.RemoveLiquidityCantWithdrawEnoughTez"
  | Uniswap.RemoveLiquidityCantWithdrawEnoughKit       -> "Uniswap.RemoveLiquidityCantWithdrawEnoughKit"
  | Uniswap.RemoveLiquidityTooMuchTezWithdrawn         -> "Uniswap.RemoveLiquidityTooMuchTezWithdrawn"
  | Uniswap.RemoveLiquidityTooMuchKitWithdrawn         -> "Uniswap.RemoveLiquidityTooMuchKitWithdrawn"
  | Uniswap.RemoveLiquidityNoLiquidityBurned           -> "Uniswap.RemoveLiquidityNoLiquidityBurned"
  | Uniswap.RemoveLiquidityNoTezWithdrawnExpected      -> "Uniswap.RemoveLiquidityNoTezWithdrawnExpected"
  | Uniswap.RemoveLiquidityNoKitWithdrawnExpected      -> "Uniswap.RemoveLiquidityNoKitWithdrawnExpected"
  | Uniswap.BuyKitPriceFailure                         -> "Uniswap.BuyKitPriceFailure"
  | Uniswap.BuyKitTooLowExpectedKit                    -> "Uniswap.BuyKitTooLowExpectedKit"
  | Uniswap.BuyKitTooMuchKitBought                     -> "Uniswap.BuyKitTooMuchKitBought"
  | Uniswap.SellKitNonEmptyAmount                      -> "Uniswap.SellKitNonEmptyAmount"
  | Uniswap.SellKitPriceFailure                        -> "Uniswap.SellKitPriceFailure"
  | Uniswap.SellKitTooLowExpectedTez                   -> "Uniswap.SellKitTooLowExpectedTez"
  | Uniswap.SellKitTooMuchTezBought                    -> "Uniswap.SellKitTooMuchTezBought"
  | Uniswap.InvalidLiquidityToken                      -> "Uniswap.InvalidLiquidityToken"
  (* Burrow errors *)
  | Burrow.InsufficientFunds _                         -> "Burrow.InsufficientFunds"
  | Burrow.WithdrawTezFailure                          -> "Burrow.WithdrawTezFailure"
  | Burrow.MintKitFailure                              -> "Burrow.MintKitFailure"
  | Burrow.BurrowIsAlreadyActive                       -> "Burrow.BurrowIsAlreadyActive"
  | Burrow.DeactivatingAnOverburrowedBurrow            -> "Burrow.DeactivatingAnOverburrowedBurrow"
  | Burrow.DeactivatingAnInactiveBurrow                -> "Burrow.DeactivatingAnInactiveBurrow"
  | Burrow.DeactivatingWithOutstandingKit              -> "Burrow.DeactivatingWithOutstandingKit"
  | Burrow.DeactivatingWithCollateralAtAuctions        -> "Burrow.DeactivatingWithCollateralAtAuctions"
  (* Kit errors *)
  | Kit.InvalidKitToken                                -> "Kit.InvalidKitToken"
  (* Liquidation auction errors *)
  | LiquidationAuction.NoOpenAuction                   -> "LiquidationAuction.NoOpenAuction"
  | LiquidationAuction.BidTooLow                       -> "LiquidationAuction.BidTooLow"
  | LiquidationAuction.CannotReclaimLeadingBid         -> "LiquidationAuction.CannotReclaimLeadingBid"
  | LiquidationAuction.NotAWinningBid                  -> "LiquidationAuction.NotAWinningBid"
  | LiquidationAuction.NotAllSlicesClaimed             -> "LiquidationAuction.NotAllSlicesClaimed"
  | LiquidationAuction.LiquidationQueueTooLong         -> "LiquidationAuction.LiquidationQueueTooLong"
  | LiquidationAuction.InvalidLiquidationAuctionTicket -> "LiquidationAuction.InvalidLiquidationAuctionTicket"
  (* Checker errors *)
  | Checker.InsufficientPermission                     -> "Checker.InsufficientPermission"
  | Checker.MissingPermission                          -> "Checker.MissingPermission"
  | Checker.UnwantedTezGiven                           -> "Checker.UnwantedTezGiven"
  | Checker.BurrowHasCompletedLiquidation              -> "Checker.BurrowHasCompletedLiquidation"
  | Checker.NonExistentBurrow _                        -> "Checker.NonExistentBurrow"
  (* open ended *)
  | _ -> "Unknown Error"

let assert_ok (r: ('a, Error.error) result) : 'a =
  match r with
  | Ok a -> a
  | Error e -> OUnit2.assert_failure @@ show_error e

let assert_failwith (e: Error.error) (r: ('a, Error.error) result) : unit =
  match r with
  | Ok _ -> OUnit2.assert_failure "assert_failwith: should have failed but didn't!"
  | Error r ->
    if r = e then
      ()
    else
      OUnit2.assert_failure ("Expected error " ^ show_error e ^ " but got error " ^ show_error r)

