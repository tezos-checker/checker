open FixedPoint;;
open Tez;;

(* ************************************************************************* *)
(*                               Constants                                   *)
(* ************************************************************************* *)
module Constants : sig
  (** Dimensionless. Factor used for setting the minting limit
    * (alternatively: f_minting). *)
  val fplus  : FixedPoint.t

  (** Dimensionless. Factor used for setting the liquidation limit
    * (alternatively: f_liquidation). *)
  val fminus : FixedPoint.t

  (** Number of tez needed to be given for the creation of a burrow; it does
    * not count towards the burrow's collateral. *)
  val creation_deposit : Tez.t

  (** The percentage of the collateral (in tez) to give to the actor initiating
    * liquidation. TODO: Use cNp. *)
  val liquidation_reward_percentage : FixedPoint.t

  (** Percentage kept by the uniswap contract from the return asset. TODO: Use cNp. *)
  val uniswap_fee_percentage : FixedPoint.t

  (* Protected index epsilon. The higher this value is, the faster the protected
   * index catches up with the actual index. TODO: Use FixedPoint.t. *)
  val protected_index_epsilon : float
end =
struct
  let fplus  = FixedPoint.of_float 2.1

  let fminus = FixedPoint.of_float 1.9

  let creation_deposit = Tez.of_float 1.0

  let liquidation_reward_percentage = FixedPoint.of_float 0.001

  let uniswap_fee_percentage = FixedPoint.of_float 0.002

  let protected_index_epsilon = 0.0005
end

