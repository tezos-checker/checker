(* USER-FACING ERRORS *)

let[@inline] error_CfmmTooLate                                          : Ligo.int = Ligo.int_from_literal "2"

let[@inline] error_BuyKitTooLowExpectedKit                              : Ligo.int = Ligo.int_from_literal "10"
let[@inline] error_BuyKitPriceFailure                                   : Ligo.int = Ligo.int_from_literal "11"
let[@inline] error_BuyKitNoCtezGiven                                    : Ligo.int = Ligo.int_from_literal "13"

let[@inline] error_SellKitTooLowExpectedCtez                            : Ligo.int = Ligo.int_from_literal "21"
let[@inline] error_SellKitPriceFailure                                  : Ligo.int = Ligo.int_from_literal "22"
let[@inline] error_SellKitNoKitGiven                                    : Ligo.int = Ligo.int_from_literal "24"

let[@inline] error_AddLiquidityNoCtezGiven                              : Ligo.int = Ligo.int_from_literal "30"
let[@inline] error_AddLiquidityNoKitGiven                               : Ligo.int = Ligo.int_from_literal "31"
let[@inline] error_AddLiquidityNoLiquidityToBeAdded                     : Ligo.int = Ligo.int_from_literal "32"
let[@inline] error_AddLiquidityTooLowLiquidityMinted                    : Ligo.int = Ligo.int_from_literal "33"
let[@inline] error_AddLiquidityTooMuchKitRequired                       : Ligo.int = Ligo.int_from_literal "34"

let[@inline] error_RemoveLiquidityNoLiquidityBurned                     : Ligo.int = Ligo.int_from_literal "41"
let[@inline] error_RemoveLiquidityNoCtezWithdrawnExpected               : Ligo.int = Ligo.int_from_literal "42"
let[@inline] error_RemoveLiquidityNoKitWithdrawnExpected                : Ligo.int = Ligo.int_from_literal "43"
let[@inline] error_RemoveLiquidityCantWithdrawEnoughCtez                : Ligo.int = Ligo.int_from_literal "44"
let[@inline] error_RemoveLiquidityCantWithdrawEnoughKit                 : Ligo.int = Ligo.int_from_literal "46"
let[@inline] error_RemoveLiquidityTooMuchLiquidityWithdrawn             : Ligo.int = Ligo.int_from_literal "48"

let[@inline] error_LiquidationQueueTooLong                              : Ligo.int = Ligo.int_from_literal "50"
let[@inline] error_BidTooLow                                            : Ligo.int = Ligo.int_from_literal "51"
let[@inline] error_NoOpenAuction                                        : Ligo.int = Ligo.int_from_literal "52"
let[@inline] error_NotAllSlicesClaimed                                  : Ligo.int = Ligo.int_from_literal "55"
let[@inline] error_NotAWinningBid                                       : Ligo.int = Ligo.int_from_literal "56"

let[@inline] error_InsufficientFunds                                    : Ligo.int = Ligo.int_from_literal "60"
let[@inline] error_WithdrawTezFailure                                   : Ligo.int = Ligo.int_from_literal "61"
let[@inline] error_MintKitFailure                                       : Ligo.int = Ligo.int_from_literal "62"
let[@inline] error_BurrowIsAlreadyActive                                : Ligo.int = Ligo.int_from_literal "63"
let[@inline] error_DeactivatingAnOverburrowedBurrow                     : Ligo.int = Ligo.int_from_literal "64"
let[@inline] error_DeactivatingAnInactiveBurrow                         : Ligo.int = Ligo.int_from_literal "65"
let[@inline] error_DeactivatingWithOutstandingKit                       : Ligo.int = Ligo.int_from_literal "66"
let[@inline] error_DeactivatingWithCollateralAtAuctions                 : Ligo.int = Ligo.int_from_literal "67"

let[@inline] error_NonExistentBurrow                                    : Ligo.int = Ligo.int_from_literal "81"
let[@inline] error_BurrowHasCompletedLiquidation                        : Ligo.int = Ligo.int_from_literal "82"
let[@inline] error_UnwantedTezGiven                                     : Ligo.int = Ligo.int_from_literal "83"
let[@inline] error_AuthenticationError                                  : Ligo.int = Ligo.int_from_literal "84"
let[@inline] error_NotLiquidationCandidate                              : Ligo.int = Ligo.int_from_literal "85"
let[@inline] error_UnwarrantedCancellation                              : Ligo.int = Ligo.int_from_literal "86"
let[@inline] error_NotACompletedSlice                                   : Ligo.int = Ligo.int_from_literal "87"
let[@inline] error_InvalidAvlPtr                                        : Ligo.int = Ligo.int_from_literal "88"
let[@inline] error_InvalidLeafPtr                                       : Ligo.int = Ligo.int_from_literal "89"
let[@inline] error_BurrowAlreadyExists                                  : Ligo.int = Ligo.int_from_literal "90"
let[@inline] error_InvalidLiquidationAuction                            : Ligo.int = Ligo.int_from_literal "91"

let[@inline] error_GetContractOptFailure                                : Ligo.int = Ligo.int_from_literal "95"

let[@inline] error_GetEntrypointOptFailureTransferAddress               : Ligo.int = Ligo.int_from_literal "102"
let[@inline] error_GetEntrypointOptFailureBurrowStoreTez                : Ligo.int = Ligo.int_from_literal "103"
let[@inline] error_GetEntrypointOptFailureBurrowSendTezTo               : Ligo.int = Ligo.int_from_literal "105"
let[@inline] error_GetEntrypointOptFailureBurrowSetDelegate             : Ligo.int = Ligo.int_from_literal "106"
let[@inline] error_GetEntrypointOptFailureBurrowSendSliceToChecker      : Ligo.int = Ligo.int_from_literal "107"
let[@inline] error_UnauthorisedCaller                                   : Ligo.int = Ligo.int_from_literal "111"
let[@inline] error_GetEntrypointOptFailureReceivePrice                  : Ligo.int = Ligo.int_from_literal "112"
let[@inline] error_GetEntrypointOptFailureOracleEntrypoint              : Ligo.int = Ligo.int_from_literal "113"
let[@inline] error_GetEntrypointOptFailureFA12Transfer                  : Ligo.int = Ligo.int_from_literal "114"

let[@inline] error_ContractNotDeployed                                  : Ligo.int = Ligo.int_from_literal "134"
let[@inline] error_ContractAlreadyDeployed                              : Ligo.int = Ligo.int_from_literal "135"

let[@inline] error_UnexpectedParams                                     : Ligo.int = Ligo.int_from_literal "140"

let[@inline] error_GetLazyFunctionUnpackFailure                         : Ligo.int = Ligo.int_from_literal "150"
let[@inline] error_GetLazyFunctionMissingFunction                       : Ligo.int = Ligo.int_from_literal "151"

let[@inline] error_GetEntrypointOptFailureVaultReceiveTez               : Ligo.int = Ligo.int_from_literal "160"
let[@inline] error_GetEntrypointOptFailureVaultSendTezToVault           : Ligo.int = Ligo.int_from_literal "161"
let[@inline] error_GetEntrypointOptFailureVaultSendTezToContract        : Ligo.int = Ligo.int_from_literal "162"
let[@inline] error_GetEntrypointOptFailureVaultSetDelegate              : Ligo.int = Ligo.int_from_literal "163"

(* INTERNAL ERRORS *)

let[@inline] internalError_NodeTezFoundRoot                             : Ligo.int = Ligo.int_from_literal "200"
let[@inline] internalError_NodeHeightFoundRoot                          : Ligo.int = Ligo.int_from_literal "201"
let[@inline] internalError_NodeParentFoundRoot                          : Ligo.int = Ligo.int_from_literal "202"
let[@inline] internalError_NodeBranchFoundNonBranch                     : Ligo.int = Ligo.int_from_literal "203"
let[@inline] internalError_NodeLeafFoundNonLeaf                         : Ligo.int = Ligo.int_from_literal "204"
let[@inline] internalError_DerefAvlPtrFoundNonRoot                      : Ligo.int = Ligo.int_from_literal "205"
let[@inline] internalError_NodeSetParentFoundRoot                       : Ligo.int = Ligo.int_from_literal "206"
let[@inline] internalError_UpdateMatchingChildFoundLeaf                 : Ligo.int = Ligo.int_from_literal "207"
let[@inline] internalError_RefRotateLeftCurrentPtrNotBranch             : Ligo.int = Ligo.int_from_literal "208"
let[@inline] internalError_RefRotateLeftRightPtrNotBranch               : Ligo.int = Ligo.int_from_literal "209"
let[@inline] internalError_RefRotateRightCurrentPtrNotBranch            : Ligo.int = Ligo.int_from_literal "210"
let[@inline] internalError_RefRotateRightLeftPtrNotBranch               : Ligo.int = Ligo.int_from_literal "211"
let[@inline] internalError_RebalanceHeavyChildNonBranchch               : Ligo.int = Ligo.int_from_literal "212"
let[@inline] internalError_BalanceBottomUpFoundLeaf                     : Ligo.int = Ligo.int_from_literal "213"
let[@inline] internalError_RefDelParentIsLeaf                           : Ligo.int = Ligo.int_from_literal "214"
let[@inline] internalError_AvlDeleteEmptyTreeNonEmptyTree               : Ligo.int = Ligo.int_from_literal "215"
let[@inline] internalError_RefPeekFrontFoundRoot                        : Ligo.int = Ligo.int_from_literal "216"
let[@inline] internalError_RefSplitPostProcessingInvariantFailed        : Ligo.int = Ligo.int_from_literal "217"
let[@inline] internalError_RefSplitRecFoundRoot                         : Ligo.int = Ligo.int_from_literal "218"

let[@inline] internalError_ComputeTezToAuctionNegativeResult            : Ligo.int = Ligo.int_from_literal "230"

let[@inline] internalError_FractionToTezFloorNegative                   : Ligo.int = Ligo.int_from_literal "240"
let[@inline] internalError_FractionToTezFloorZeroDenominator            : Ligo.int = Ligo.int_from_literal "241"
let[@inline] internalError_FractionToNatFloorNegative                   : Ligo.int = Ligo.int_from_literal "242"
let[@inline] internalError_FractionToNatFloorZeroDenominator            : Ligo.int = Ligo.int_from_literal "243"

let[@inline] internalError_CompletedAuctionWithoutOutcome               : Ligo.int = Ligo.int_from_literal "250"
let[@inline] internalError_PopCompletedAuctionAuctionNotCompleted       : Ligo.int = Ligo.int_from_literal "251"
let[@inline] internalError_PopCompletedAuctionNoCompletedAuction        : Ligo.int = Ligo.int_from_literal "252"
let[@inline] internalError_PopCompletedAuctionCompletedAuctionNoOutcome : Ligo.int = Ligo.int_from_literal "253"
let[@inline] internalError_OldestCompletedSliceEmptyCompletedAuction    : Ligo.int = Ligo.int_from_literal "254"

let[@inline] internalError_CtezSubNegative                              : Ligo.int = Ligo.int_from_literal "260"
let[@inline] internalError_CtezOfFractionCeilNegative                   : Ligo.int = Ligo.int_from_literal "261"
let[@inline] internalError_CtezOfFractionFloorNegative                  : Ligo.int = Ligo.int_from_literal "262"

let[@inline] internalError_KitSubNegative                               : Ligo.int = Ligo.int_from_literal "270"
let[@inline] internalError_KitOfFractionCeilNegative                    : Ligo.int = Ligo.int_from_literal "271"
let[@inline] internalError_KitOfFractionFloorNegative                   : Ligo.int = Ligo.int_from_literal "272"

let[@inline] internalError_LqtSubNegative                               : Ligo.int = Ligo.int_from_literal "280"
let[@inline] internalError_LqtOfFractionCeilNegative                    : Ligo.int = Ligo.int_from_literal "281"
let[@inline] internalError_LqtOfFractionFloorNegative                   : Ligo.int = Ligo.int_from_literal "282"

let[@inline] internalError_PowRecImpossible                             : Ligo.int = Ligo.int_from_literal "290" (* NOTE: this really is impossible. *)
let[@inline] internalError_CdivIntIntZeroDenominator                    : Ligo.int = Ligo.int_from_literal "291"
let[@inline] internalError_FdivIntIntZeroDenominator                    : Ligo.int = Ligo.int_from_literal "292"

let[@inline] internalError_SliceListFromLeafPtrEmptySliceList           : Ligo.int = Ligo.int_from_literal "300"
let[@inline] internalError_SliceListRemoveEmptyList                     : Ligo.int = Ligo.int_from_literal "301"

let[@inline] internalError_MemGetElementNotFound                        : Ligo.int = Ligo.int_from_literal "310"

let[@inline] internalError_TokSubNegative                               : Ligo.int = Ligo.int_from_literal "320"
let[@inline] internalError_TokOfFractionCeilNegative                    : Ligo.int = Ligo.int_from_literal "321"
let[@inline] internalError_TokOfFractionFloorNegative                   : Ligo.int = Ligo.int_from_literal "322"
