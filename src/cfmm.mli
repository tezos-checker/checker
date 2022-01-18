
open Ctok
open Kit
open Lqt
open FixedPoint
open CfmmTypes
open Common

(** Compute the price of kit in ctok (ratio of ctok and kit in the cfmm
    contract), as it was at the end of the last block. This is to be used when
    required for the calculation of the drift derivative instead of up-to-date
    kit_in_ctok, because it is a little harder to manipulate. *)
val cfmm_kit_in_ctok_in_prev_block : cfmm -> ratio

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
