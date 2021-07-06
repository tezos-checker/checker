let[@inline] error_CfmmTooLate                                  : Ligo.int = Ligo.int_from_literal "2"

let[@inline] error_BuyKitTooLowExpectedKit                         : Ligo.int = Ligo.int_from_literal "10"
let[@inline] error_BuyKitPriceFailure                              : Ligo.int = Ligo.int_from_literal "11"
let[@inline] error_BuyKitNoTezGiven                                : Ligo.int = Ligo.int_from_literal "13"

let[@inline] error_SellKitTooLowExpectedTez                        : Ligo.int = Ligo.int_from_literal "21"
let[@inline] error_SellKitPriceFailure                             : Ligo.int = Ligo.int_from_literal "22"
let[@inline] error_SellKitNoKitGiven                               : Ligo.int = Ligo.int_from_literal "24"

let[@inline] error_AddLiquidityNoTezGiven                          : Ligo.int = Ligo.int_from_literal "30"
let[@inline] error_AddLiquidityNoKitGiven                          : Ligo.int = Ligo.int_from_literal "31"
let[@inline] error_AddLiquidityNoLiquidityToBeAdded                : Ligo.int = Ligo.int_from_literal "32"
let[@inline] error_AddLiquidityTooLowLiquidityMinted               : Ligo.int = Ligo.int_from_literal "33"
let[@inline] error_AddLiquidityTooMuchKitRequired                  : Ligo.int = Ligo.int_from_literal "34"

let[@inline] error_RemoveLiquidityNoLiquidityBurned                : Ligo.int = Ligo.int_from_literal "41"
let[@inline] error_RemoveLiquidityNoTezWithdrawnExpected           : Ligo.int = Ligo.int_from_literal "42"
let[@inline] error_RemoveLiquidityNoKitWithdrawnExpected           : Ligo.int = Ligo.int_from_literal "43"
let[@inline] error_RemoveLiquidityCantWithdrawEnoughTez            : Ligo.int = Ligo.int_from_literal "44"
let[@inline] error_RemoveLiquidityCantWithdrawEnoughKit            : Ligo.int = Ligo.int_from_literal "46"
let[@inline] error_RemoveLiquidityTooMuchLiquidityWithdrawn        : Ligo.int = Ligo.int_from_literal "48"

let[@inline] error_LiquidationQueueTooLong                         : Ligo.int = Ligo.int_from_literal "50"
let[@inline] error_BidTooLow                                       : Ligo.int = Ligo.int_from_literal "51"
let[@inline] error_NoOpenAuction                                   : Ligo.int = Ligo.int_from_literal "52"
let[@inline] error_NotAllSlicesClaimed                             : Ligo.int = Ligo.int_from_literal "55"
let[@inline] error_NotAWinningBid                                  : Ligo.int = Ligo.int_from_literal "56"

(*
let[@inline] error_TouchParametersInThePast                        : Ligo.int = Ligo.int_from_literal "58"
*)

let[@inline] error_InsufficientFunds                               : Ligo.int = Ligo.int_from_literal "60"
let[@inline] error_WithdrawTezFailure                              : Ligo.int = Ligo.int_from_literal "61"
let[@inline] error_MintKitFailure                                  : Ligo.int = Ligo.int_from_literal "62"
let[@inline] error_BurrowIsAlreadyActive                           : Ligo.int = Ligo.int_from_literal "63"
let[@inline] error_DeactivatingAnOverburrowedBurrow                : Ligo.int = Ligo.int_from_literal "64"
let[@inline] error_DeactivatingAnInactiveBurrow                    : Ligo.int = Ligo.int_from_literal "65"
let[@inline] error_DeactivatingWithOutstandingKit                  : Ligo.int = Ligo.int_from_literal "66"
let[@inline] error_DeactivatingWithCollateralAtAuctions            : Ligo.int = Ligo.int_from_literal "67"

let[@inline] error_NonExistentBurrow                               : Ligo.int = Ligo.int_from_literal "81"
let[@inline] error_BurrowHasCompletedLiquidation                   : Ligo.int = Ligo.int_from_literal "82"
let[@inline] error_UnwantedTezGiven                                : Ligo.int = Ligo.int_from_literal "83"
let[@inline] error_AuthenticationError                             : Ligo.int = Ligo.int_from_literal "84"
let[@inline] error_NotLiquidationCandidate                         : Ligo.int = Ligo.int_from_literal "85"
let[@inline] error_UnwarrantedCancellation                         : Ligo.int = Ligo.int_from_literal "86"
let[@inline] error_NotACompletedSlice                              : Ligo.int = Ligo.int_from_literal "87"
let[@inline] error_InvalidAvlPtr                                   : Ligo.int = Ligo.int_from_literal "88"
let[@inline] error_InvalidLeafPtr                                  : Ligo.int = Ligo.int_from_literal "89"
let[@inline] error_BurrowAlreadyExists                             : Ligo.int = Ligo.int_from_literal "90"
let[@inline] error_InvalidLiquidationAuction                       : Ligo.int = Ligo.int_from_literal "91"

let[@inline] error_GetContractOptFailure                           : Ligo.int = Ligo.int_from_literal "95"

let[@inline] error_GetEntrypointOptFailureTransferAddress          : Ligo.int = Ligo.int_from_literal "102"
let[@inline] error_GetEntrypointOptFailureBurrowStoreTez           : Ligo.int = Ligo.int_from_literal "103"
let[@inline] error_GetEntrypointOptFailureBurrowSendTezTo          : Ligo.int = Ligo.int_from_literal "105"
let[@inline] error_GetEntrypointOptFailureBurrowSetDelegate        : Ligo.int = Ligo.int_from_literal "106"
let[@inline] error_GetEntrypointOptFailureBurrowSendSliceToChecker : Ligo.int = Ligo.int_from_literal "107"
let[@inline] error_UnauthorisedCaller                              : Ligo.int = Ligo.int_from_literal "111"
let[@inline] error_GetEntrypointOptFailureReceivePrice             : Ligo.int = Ligo.int_from_literal "112"
let[@inline] error_GetEntrypointOptFailureOracleEntrypoint         : Ligo.int = Ligo.int_from_literal "113"
let[@inline] error_GetEntrypointOptFailureFA12Transfer             : Ligo.int = Ligo.int_from_literal "114"

let[@inline] error_ContractNotDeployed                             : Ligo.int = Ligo.int_from_literal "134"
let[@inline] error_ContractAlreadyDeployed                         : Ligo.int = Ligo.int_from_literal "135"
