(* ************************************************************************* *)
(*                               Constants                                   *)
(* ************************************************************************* *)

(** Dimensionless. Factor used for setting the minting limit. *)
let fminting : Q.t = FixedPoint.(to_q (of_string "2.1"))

(** Dimensionless. Factor used for setting the liquidation limit. *)
let fliquidation : Q.t = FixedPoint.(to_q (of_string "1.9"))

(** Number of tez needed to be given for the creation of a burrow; it does
  * not count towards the burrow's collateral. *)
let creation_deposit : Tez.t = Tez.one

(** Yearly burrow fee percentage. *)
let burrow_fee_percentage : FixedPoint.t = FixedPoint.of_string "0.005"

(** The percentage of a burrow's collateral that we offer to whoever triggers
  * the burrow's liquidation. *)
let liquidation_reward_percentage : FixedPoint.t = FixedPoint.of_string "0.001"

(** Percentage kept by the uniswap contract from the return asset. *)
let uniswap_fee = FixedPoint.(to_q (of_string "0.002"))

(** Protected index epsilon. The higher this value is, the faster the protected
  * index catches up with the actual index. *)
let protected_index_epsilon : FixedPoint.t = FixedPoint.of_string "0.0005"

(** The maximum number of tez that can be in an auction lot. *)
let max_lot_size : Tez.t = Tez.of_mutez 10_000_000_000

(** The minimum fraction of the auction queue which must go into a new auction lot. *)
let min_lot_auction_queue_fraction : FixedPoint.t = FixedPoint.of_string "0.05"

(** The percentage of additional collateral that we charge when liquidating
  * a burrow, to penalize it for liquidation. *)
let liquidation_penalty : Q.t = FixedPoint.(to_q (of_string "0.10"))

(** For convenience. The number of seconds in a year, taking into account
  * leap years. Basically (365 + 1/4 - 1/100 + 1/400) days * 24 * 60 * 60. *)
let seconds_in_a_year : int = 31556952

(** Low bracket used for the calculation of the drift derivative. *)
let target_low_bracket : FixedPoint.t = FixedPoint.of_string "0.005"

(** High bracket used for the calculation of the drift derivative. *)
let target_high_bracket : FixedPoint.t = FixedPoint.of_string "0.05"

(** How fast a descending option price drops. *)
let auction_decay_rate : FixedPoint.t = FixedPoint.of_string "0.999"

(** The maximum number of seconds that can pass between two (ascending) bids
  * during an auction. The auction should expire if more than this many seconds
  * pass between two bids. Currently set to 20min (20min * 60sec/min = 1200s). *)
let max_bid_interval_in_seconds : int = 1200

(** The maximum number of blocks that can pass between two (ascending) bids
  * during an auction. The auction should expire if more blocks than this
  * number pass between two bids. Currently set to 20. *)
let max_bid_interval_in_blocks : int = 20

(** Every bid in an ascending auction needs to improve over the previous bid by
  * at least 0.33 cNp. *)
let bid_improvement_factor : FixedPoint.t = FixedPoint.of_string "0.0033"
