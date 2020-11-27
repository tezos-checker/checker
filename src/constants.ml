(* ************************************************************************* *)
(*                               Constants                                   *)
(* ************************************************************************* *)

(** Dimensionless. Factor used for setting the minting limit. *)
let fminting : Q.t = Q.of_string "21/10" (* 2.1 *)

(** Dimensionless. Factor used for setting the liquidation limit. *)
let fliquidation : Q.t = Q.of_string "19/10" (* 1.9 *)

(** Number of tez needed to be given for the creation of a burrow; it does
  * not count towards the burrow's collateral. *)
let creation_deposit : Tez.t = Tez.one

(** Yearly burrow fee percentage. *)
let burrow_fee_percentage : Q.t = Q.of_string "5/1000" (* 0.005 *)

(** The percentage of a burrow's collateral that we offer to whoever triggers
  * the burrow's liquidation. *)
let liquidation_reward_percentage : Q.t = Q.of_string "1/1000" (* 0.001 *)

(** Percentage kept by the uniswap contract from the return asset. *)
let uniswap_fee : Q.t = Q.of_string "2/1000" (* 0.002 *)

(** Protected index epsilon. The higher this value is, the faster the protected
  * index catches up with the actual index. *)
let protected_index_epsilon : Q.t = Q.of_string "5/10000" (* 0.0005 *)

(** The maximum number of tez that can be in an auction lot. *)
let max_lot_size : Tez.t = Tez.of_mutez 10_000_000_000

(** The minimum fraction of the auction queue which must go into a new auction lot. *)
let min_lot_auction_queue_fraction : FixedPoint.t =
  FixedPoint.of_q_floor (Q.of_string "5/100")

(** The percentage of additional collateral that we charge when liquidating
  * a burrow, to penalize it for liquidation. *)
let liquidation_penalty : Q.t = Q.of_string "1/10" (* 0.1 *)

(** For convenience. The number of seconds in a year, taking into account
  * leap years. Basically (365 + 1/4 - 1/100 + 1/400) days * 24 * 60 * 60. *)
let seconds_in_a_year : int = 31556952

(** For convenience. The number of seconds in a day. Basically
  * 24h * 60min/h * 60sec/min = 86400. *)
let seconds_in_a_day : int = 86400

(** Low bracket used for the calculation of the drift derivative. *)
let target_low_bracket : Q.t = Q.of_string "5/1000" (* 0.005 *)

(** High bracket used for the calculation of the drift derivative. *)
let target_high_bracket : Q.t = Q.of_string "5/100" (* 0.05 *)

(** How fast a descending option price drops per second. Currently we want it
  * to drop by around 1cNp per minute, so we just divide by 60 to get roughly
  * how much it should be per second. *)
let auction_decay_rate : Q.t = Q.of_string "1/6000" (* 0.01/60 *)

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
let bid_improvement_factor : Q.t = Q.of_string "33/10000" (* 0.33/100 *)

(** Parameter used for calculating the current reward for touching the checker
  * contract. See calculate_touch_reward for their use. *)
let touch_reward_low_bracket : int = 600 (* = 60 * 10 = 10 minutes *)

(** We want the reward in the first bracket to be 0.1 kit / minute, so we just
  * divide by 60 to get roughly how much should it be per second. *)
let touch_low_reward : Q.t = Q.of_string "1/600" (* 0.1/60 *)

(** We want the reward in the first bracket to be 1 kit / minute, so we just
  * divide by 60 to get roughly how much should it be per second. *)
let touch_high_reward : Q.t = Q.of_string "1/60" (* 1/60 *)

(** The number of liquidation slices to process every time the checker
  * contract is touched. *)
let number_of_slices_to_process : int = 5

(** Maximum number of items allowed in the liquidation queue.
  * TODO: Decide on the number here.
  * TODO: We might want to check the height of an AVL tree
  * instead of an absolute number of slices. *)
let max_liquidation_slices_in_auction_queue: int = 1_000_000
