open Kit
open UniswapTypes

(* The general concept of uniswap is that you have quantity a of an asset A
 * and b of an asset B and you process buy and sell requests by maintaining
 * the product a * b constant. So if someone wants to sell a quantity da of
 * asset A to the contract, the balance would become (a + da) so you can
 * give that person a quantity db of asset B in exchange such that (a +
 * da)(b - db) = a * b. Solving for db gives db  = da * b / (a + da). We
 * can rewrite this as db = da * (b / a) * (a / (a + da)) where (b / a)
 * represents the  "price" before the order and a / (a + da)  represents
 * the "slippage". Indeed, a property of uniswap is that with arbitrageurs
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
(* Remaining TODO for uniswap.mli:
 * - Ensure that the balances and prices in uniswap do not go too far off.
*)

(** Compute the price of kit in tez (ratio of tez and kit in the uniswap
  * contract), as it was at the end of the last block. This is to be used when
  * required for the calculation of the drift derivative instead of up-to-date
  * kit_in_tez, because it is a little harder to manipulate. *)
val uniswap_kit_in_tez_in_prev_block : uniswap -> Ratio.ratio

(** Buy some kit from the uniswap contract by providing some tez. Fail if the
  * desired amount of kit cannot be bought or if the deadline has passed. *)
val uniswap_buy_kit :
  uniswap ->
  Ligo.tez (* amount *) ->
  kit (* min kit expected *) ->
  Ligo.timestamp (* deadline *) ->
  (kit * uniswap)

(** Sell some kit to the uniswap contract. Fail if the desired amount of tez
  * cannot be bought or if the deadline has passed. *)
val uniswap_sell_kit :
  uniswap ->
  Ligo.tez (* amount: must be zero *) ->
  kit ->
  Ligo.tez (* min tez expected *) ->
  Ligo.timestamp (* deadline *) ->
  (Ligo.tez * uniswap)

(** Buy some liquidity from the uniswap contract, by giving it some tez and
  * some kit. If the given amounts does not have the right ratio, we
  * liquidate as much as we can with the right ratio, and return the
  * leftovers, along with the liquidity tokens. *)
(* But where do the assets in uniswap come from? Liquidity providers, or
 * "LP" deposit can deposit a quantity la and lb of assets A and B in the
 * same proportion as the contract la / lb = a / b . Assuming there are n
 * "liquidity tokens" extant, they receive m = floor(n la / a) tokens and
 * there are now m +n liquidity tokens extant. They can redeem then at
 * anytime for a fraction of the assets A and B. The reason to do this in
 * uniswap is that usage of uniswap costs 0.3%, and that ultimately can
 * grow the balance of the assets in the contract. An additional reason
 * to do it in huxian is that the kit balance of the uniswap contract is
 * continuously credited with the burrow fee taken from burrow holders.
*)
val uniswap_add_liquidity :
  uniswap ->
  Ligo.tez (* amount *) ->
  (** This amount is temporarily treated as if it is part of the tez balance *)
  Ligo.tez (* pending accrual *) ->
  kit (* max kit deposited *) ->
  Ligo.nat (* min lqt minted *) ->
  Ligo.timestamp (* deadline *) ->
  (Ligo.nat * kit * uniswap)

(** Sell some liquidity to the uniswap contract. Selling liquidity always
  * succeeds, but might leave the contract without tez and kit if everybody
  * sells their liquidity. I think it is unlikely to happen, since the last
  * liquidity holders wouldn't want to lose the burrow fees.
*)
val uniswap_remove_liquidity :
  uniswap ->
  Ligo.tez (* amount: should be zero *) ->
  Ligo.nat (* lqt burned *) ->
  Ligo.tez (* min tez withdrawn *) ->
  kit (* min kit withdrawn *) ->
  Ligo.timestamp (* deadline *) ->
  (Ligo.tez * kit * uniswap)

(** Add accrued burrowing fees to the uniswap contract. *)
val uniswap_add_accrued_kit : uniswap -> kit -> uniswap

(** Add accrued tez to the uniswap contract. *)
val uniswap_add_accrued_tez : uniswap -> Ligo.tez -> uniswap

(* BEGIN_OCAML *)
(* FOR TESTING PURPOSES ONLY. SHOULD NOT BE EXPORTED REALLY. *)
val uniswap_make_for_test :
  tez:Ligo.tez ->
  kit:kit ->
  lqt:Ligo.nat ->
  kit_in_tez_in_prev_block:Ratio.ratio ->
  last_level: Ligo.nat ->
  uniswap

(* FOR TESTING PURPOSES ONLY. SHOULD NOT BE EXPORTED REALLY. Compute the
 * current price of kit in tez, as estimated using the ratio of tez and kit
 * currently in the uniswap contract. *)
val uniswap_kit_in_tez : uniswap -> Ratio.ratio

(* FOR TESTING PURPOSES ONLY. SHOULD NOT BE EXPORTED REALLY. Compute the
 * current product of kit and tez, using the current contents of the uniswap
 * contract. *)
val uniswap_kit_times_tez : uniswap -> Ratio.ratio

(* FOR TESTING PURPOSES ONLY. SHOULD NOT BE EXPORTED REALLY. Reveal the
 * current number of liquidity tokens extant. *)
val uniswap_liquidity_tokens_extant : uniswap -> Ligo.nat
(* END_OCAML *)
