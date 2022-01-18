
open Ctok
open Kit
open Lqt
open FixedPoint
(*
open Constants
*)
open CfmmTypes
(*
open Error
*)
open Common

(*
(* When the cfmm is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let[@inline] cfmm_assert_initialized (u: cfmm) : cfmm =
  assert (not (u.ctok = ctok_zero));
  assert (not (u.kit = kit_zero));
  assert (not (u.lqt = lqt_zero));
  u
*)

(** Compute the price of kit in ctok (ratio of ctok and kit in the cfmm
    contract), as it was at the end of the last block. This is to be used when
    required for the calculation of the drift derivative instead of up-to-date
    kit_in_ctok, because it is a little harder to manipulate. *)
val cfmm_kit_in_ctok_in_prev_block : cfmm -> ratio

(*
(* Update the kit_in_ctok cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_ctok at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * cfmm contract was touched. *)
let cfmm_sync_last_observed (cfmm: cfmm) : cfmm =
  assert (Ligo.geq_nat_nat !Ligo.Tezos.level cfmm.last_level);
  if Ligo.leq_nat_nat !Ligo.Tezos.level cfmm.last_level then
    (* do nothing if it's been touched already in this block *)
    cfmm
  else
    { cfmm with
      kit_in_ctok_in_prev_block =
        make_ratio
          (Ligo.mul_nat_int (ctok_to_denomination_nat cfmm.ctok) kit_scaling_factor_int)
          (Ligo.mul_nat_int (kit_to_denomination_nat cfmm.kit) ctok_scaling_factor_int);
      last_level = !Ligo.Tezos.level;
    }
*)

(** Compute the maximum [min_kit_expected] for [cfmm_buy_kit] to succeed. *)
val cfmm_view_min_kit_expected_buy_kit : cfmm -> fixedpoint -> ctok -> (kit * cfmm)

(** Buy some kit from the cfmm contract by providing some ctok. Fail if the
    desired amount of kit cannot be bought or if the deadline has passed. *)
val cfmm_buy_kit : cfmm -> fixedpoint -> ctok -> kit -> Ligo.timestamp -> (kit * cfmm)

(** Compute the maximum [min_ctok_expected] for [cfmm_sell_kit] to succeed. *)
val cfmm_view_min_ctok_expected_cfmm_sell_kit : cfmm -> fixedpoint -> kit -> (ctok * cfmm)

(** Sell some kit to the cfmm contract. Fail if the desired amount of ctok
    cannot be bought or if the deadline has passed. *)
val cfmm_sell_kit : cfmm -> fixedpoint -> kit -> ctok -> Ligo.timestamp -> (ctok * cfmm)

(** Compute the minimum [max_kit_deposited] and the maximum [min_lqt_minted]
    for [cfmm_add_liquidity] to succeed. *)
val cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity : cfmm -> ctok -> (lqt * kit * cfmm)

(** Buy some liquidity from the cfmm contract, by giving it some ctok and
    some kit. If the given amounts does not have the right ratio, we
    liquidate all the ctok given and as much kit as we can with the right
    ratio, and return the leftovers, along with the liquidity tokens. *)
val cfmm_add_liquidity : cfmm -> ctok -> kit -> lqt -> Ligo.timestamp -> (lqt * kit * cfmm)

(** Compute the maximum [min_ctok_withdrawn] and the minimum
    [min_kit_withdrawn] for [cfmm_remove_liquidity] to succeed. *)
val cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity : cfmm -> lqt -> (ctok * kit * cfmm)

(** Sell some liquidity to the cfmm contract. Selling liquidity always
    succeeds, but might leave the contract without ctok and kit if everybody
    sells their liquidity. I think it is unlikely to happen, since the last
    liquidity holders wouldn't want to lose the burrow fees. *)
val cfmm_remove_liquidity : cfmm -> lqt -> ctok -> kit -> Ligo.timestamp -> (ctok * kit * cfmm)

(** Add accrued burrowing fees to the cfmm contract. *)
val cfmm_add_accrued_kit : cfmm -> kit -> cfmm
