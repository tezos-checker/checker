open Ctez
open Kit
open Lqt
open CfmmTypes

(* The general concept of cfmm is that you have quantity a of an asset A
 * and b of an asset B and you process buy and sell requests by maintaining
 * the product a * b constant. So if someone wants to sell a quantity da of
 * asset A to the contract, the balance would become (a + da) so you can
 * give that person a quantity db of asset B in exchange such that (a +
 * da)(b - db) = a * b. Solving for db gives db  = da * b / (a + da). We
 * can rewrite this as db = da * (b / a) * (a / (a + da)) where (b / a)
 * represents the  "price" before the order and a / (a + da)  represents
 * the "slippage". Indeed, a property of cfmm is that with arbitrageurs
 * around, the ratio (a / b) gives you the market price of A in terms of B.
 *
 * On top of that, we can add some fees of 0.2 cNp. So the equation becomes
 * something like db = da * b / (a + da) * (1 - 0.2/100) (note that this
 * formula is a first-order approximation in the sense that two orders of size
 * da / 2 will give you a better price than one order of size da, but the
 * difference is far smaller than typical fees or any amount we care about.
*)
(* Check out dexter for technical details:
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/ligo/dexter.ligo
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/docs/dexter-informal-specification.md
*)

(** Compute the price of kit in ctez (ratio of ctez and kit in the cfmm
    contract), as it was at the end of the last block. This is to be used when
    required for the calculation of the drift derivative instead of up-to-date
    kit_in_ctez, because it is a little harder to manipulate. *)
val cfmm_kit_in_ctez_in_prev_block : cfmm -> Common.ratio

(** Compute the maximum [min_kit_expected] for [cfmm_buy_kit] to succeed. *)
val cfmm_view_min_kit_expected_buy_kit : cfmm -> ctez -> (kit * cfmm)

(** Buy some kit from the cfmm contract by providing some ctez. Fail if the
    desired amount of kit cannot be bought or if the deadline has passed. *)
val cfmm_buy_kit :
  cfmm ->
  ctez (* amount *) ->
  kit (* min kit expected *) ->
  Ligo.timestamp (* deadline *) ->
  (kit * cfmm)

(** Compute the maximum [min_ctez_expected] for [cfmm_sell_kit] to succeed. *)
val cfmm_view_min_ctez_expected_cfmm_sell_kit : cfmm -> kit -> (ctez * cfmm)

(** Sell some kit to the cfmm contract. Fail if the desired amount of ctez
    cannot be bought or if the deadline has passed. *)
val cfmm_sell_kit :
  cfmm ->
  kit ->
  ctez (* min ctez expected *) ->
  Ligo.timestamp (* deadline *) ->
  (ctez * cfmm)

(** Compute the minimum [max_kit_deposited] and the maximum [min_lqt_minted]
    for [cfmm_add_liquidity] to succeed. *)
val cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity : cfmm -> ctez -> (lqt * kit * cfmm)

(** Buy some liquidity from the cfmm contract, by giving it some ctez and
    some kit. If the given amounts does not have the right ratio, we
    liquidate all the ctez given and as much kit as we can with the right
    ratio, and return the leftovers, along with the liquidity tokens. *)
(* But where do the assets in cfmm come from? Liquidity providers, or
 * "LP" deposit can deposit a quantity la and lb of assets A and B in the
 * same proportion as the contract la / lb = a / b . Assuming there are n
 * "liquidity tokens" extant, they receive m = floor(n la / a) tokens and
 * there are now m +n liquidity tokens extant. They can redeem then at
 * anytime for a fraction of the assets A and B. The reason to do this in
 * cfmm is that usage of cfmm costs 0.3%, and that ultimately can
 * grow the balance of the assets in the contract. An additional reason
 * to do it in checker is that the kit balance of the cfmm contract is
 * continuously credited with the burrow fee taken from burrow holders.
*)
val cfmm_add_liquidity :
  cfmm ->
  ctez (* amount *) ->
  kit (* max kit deposited *) ->
  lqt (* min lqt minted *) ->
  Ligo.timestamp (* deadline *) ->
  (lqt * kit * cfmm)

(** Compute the maximum [min_ctez_withdrawn] and the minimum
    [min_kit_withdrawn] for [cfmm_remove_liquidity] to succeed. *)
val cfmm_view_min_ctez_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity : cfmm -> lqt -> (ctez * kit * cfmm)

(** Sell some liquidity to the cfmm contract. Selling liquidity always
    succeeds, but might leave the contract without ctez and kit if everybody
    sells their liquidity. I think it is unlikely to happen, since the last
    liquidity holders wouldn't want to lose the burrow fees.
*)
val cfmm_remove_liquidity :
  cfmm ->
  lqt (* lqt burned *) ->
  ctez (* min ctez withdrawn *) ->
  kit (* min kit withdrawn *) ->
  Ligo.timestamp (* deadline *) ->
  (ctez * kit * cfmm)

(** Add accrued burrowing fees to the cfmm contract. *)
val cfmm_add_accrued_kit : cfmm -> kit -> cfmm
