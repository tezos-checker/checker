open Ratio
open FixedPoint

(** Dimensionless. Factor used for setting the minting limit. *)
let[@inline] fminting : ratio = make_real_unsafe (Ligo.int_from_literal "21") (Ligo.int_from_literal "10") (* 2.1 *)

(** Dimensionless. Factor used for setting the liquidation limit. *)
let[@inline] fliquidation : ratio = make_real_unsafe (Ligo.int_from_literal "19") (Ligo.int_from_literal "10") (* 1.9 *)

(** Number of tez needed to be given for the creation of a burrow; it does
    not count towards the burrow's collateral. *)
let[@inline] creation_deposit : Ligo.tez = Ligo.tez_from_literal "1_000_000mutez"

(** Yearly burrow fee percentage. *)
let[@inline] burrow_fee_percentage : ratio = make_real_unsafe (Ligo.int_from_literal "5") (Ligo.int_from_literal "1000") (* 0.005 *)

(** Factor used to scale down the imbalance rate. The higher the value, the
    faster the imbalance rate saturates. *)
let[@inline] imbalance_scaling_factor : ratio = make_real_unsafe (Ligo.int_from_literal "3") (Ligo.int_from_literal "4") (* 0.75 *)

(** Maximum yearly imbalance rate. *)
let[@inline] imbalance_limit = make_real_unsafe (Ligo.int_from_literal "5") (Ligo.int_from_literal "100")

(** The percentage of a burrow's collateral that we offer to whoever triggers
    the burrow's liquidation. *)
let[@inline] liquidation_reward_percentage : ratio =
  make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "1000") (* 0.001 *)

(** Percentage kept by the cfmm contract from the return asset. *)
let[@inline] cfmm_fee : ratio = make_real_unsafe (Ligo.int_from_literal "2") (Ligo.int_from_literal "1000") (* 0.002 *)

(** Protected index epsilon. The higher this value is, the faster the protected
    index catches up with the actual index. Currently calculated as 0.05cNp
    per minute. Basically
    {[(0.05 / 100) / 60 = 5/600_000 = 1/120_000]}
    so the inverse is simply 120_000. *)
let[@inline] protected_index_inverse_epsilon : Ligo.int = Ligo.int_from_literal "120_000"

(** The maximum number of tez that can be in an auction lot. *)
let[@inline] max_lot_size : Ligo.tez = Ligo.tez_from_literal "10_000_000_000mutez"

(** The minimum fraction of the auction queue which must go into a new auction lot. *)
let[@inline] min_lot_auction_queue_fraction : ratio =
  make_real_unsafe (Ligo.int_from_literal "5") (Ligo.int_from_literal "100")

(** The percentage of additional collateral that we charge when liquidating
    a burrow, to penalize it for liquidation. *)
let[@inline] liquidation_penalty : ratio = make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "10") (* 0.1 *)

(** For convenience. The number of seconds in a year, taking into account
    leap years. Basically
    {[(365 + 1/4 - 1/100 + 1/400) days * 24 * 60 * 60]}
*)
let[@inline] seconds_in_a_year : Ligo.int = Ligo.int_from_literal "31556952"

(** For convenience. The number of seconds in a day. Basically
    {[24h * 60min/h * 60sec/min = 86400]}
*)
let[@inline] seconds_in_a_day : Ligo.int = Ligo.int_from_literal "86400"

(** Low bracket used for the calculation of the drift derivative. *)
let[@inline] target_low_bracket : ratio = make_real_unsafe (Ligo.int_from_literal "5") (Ligo.int_from_literal "1000") (* 0.005 *)

(** High bracket used for the calculation of the drift derivative. *)
let[@inline] target_high_bracket : ratio = make_real_unsafe (Ligo.int_from_literal "5") (Ligo.int_from_literal "100") (* 0.05 *)

(** The drift derivative can take one of 5 distinct values: 0, +/-0.01 cNp/day,
    and +/-0.05 cNp/day. We calculate those statically thus as follows:
    {[
      low_acceleration  = 0.01/100 * (86400 * 86400) = 1/74649600000000 =  247111 in fixedpoint
      high_acceleration = 0.05/100 * (86400 * 86400) = 5/74649600000000 = 1235555 in fixedpoint
    ]}
*)
let[@inline] low_positive_acceleration : fixedpoint = fixedpoint_of_raw (Ligo.int_from_literal "247111")
let[@inline] low_negative_acceleration : fixedpoint = fixedpoint_of_raw (Ligo.int_from_literal "-247111")
let[@inline] high_positive_acceleration : fixedpoint = fixedpoint_of_raw (Ligo.int_from_literal "1235555")
let[@inline] high_negative_acceleration : fixedpoint = fixedpoint_of_raw (Ligo.int_from_literal "-1235555")

(** How fast a descending option price drops per second. Currently we want it
    to drop by around 1cNp per minute, so we just divide by 60 to get roughly
    how much it should be per second. *)
let[@inline] auction_decay_rate : ratio = make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "6000") (* 0.01/60 *)

(** The maximum number of seconds that can pass between two (ascending) bids
    during an auction. The auction should expire if more than this many seconds
    pass between two bids. Currently set to 20min ([20min * 60sec/min = 1200s]). *)
let[@inline] max_bid_interval_in_seconds : Ligo.int = Ligo.int_from_literal "1200"

(** The maximum number of blocks that can pass between two (ascending) bids
    during an auction. The auction should expire if more blocks than this
    number pass between two bids. Currently set to 20. *)
let[@inline] max_bid_interval_in_blocks : Ligo.nat = Ligo.nat_from_literal "20n"

(** Every bid in an ascending auction needs to improve over the previous bid by
    at least 0.33 cNp. *)
let[@inline] bid_improvement_factor : ratio = make_real_unsafe (Ligo.int_from_literal "33") (Ligo.int_from_literal "10000") (* 0.33/100 *)

(** Parameter used for calculating the current reward for touching the checker
    contract. See {!Checker.calculate_touch_reward} for their use. *)
let[@inline] touch_reward_low_bracket : Ligo.int = Ligo.int_from_literal "600" (* = 60 * 10 = 10 minutes *)

(** We want the reward in the first bracket to be 0.1 kit / minute, so we just
    divide by 60 to get roughly how much should it be per second. *)
let[@inline] touch_low_reward : ratio = make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "600") (* 0.1/60 *)

(** We want the reward in the first bracket to be 1 kit / minute, so we just
    divide by 60 to get roughly how much should it be per second. *)
let[@inline] touch_high_reward : ratio = make_real_unsafe (Ligo.int_from_literal "1") (Ligo.int_from_literal "60") (* 1/60 *)

(** The number of liquidation slices to process every time the checker
    contract is touched. *)
let[@inline] number_of_slices_to_process : int = 5

(** Maximum height of the tree used as liquidation queue.
    The maximum number of elements will be between [2**(n-1)] and [2**(n-2)].
*)
let[@inline] max_liquidation_queue_height: Ligo.int = Ligo.int_from_literal "12"
