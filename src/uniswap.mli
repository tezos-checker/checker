(* ************************************************************************* *)
(*                                Uniswap                                    *)
(* ************************************************************************* *)
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
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/docs/dexter-informal-specification.md
*)
(* Remaining TODO for uniswap.mli:
 * - Ensure that the balances and prices in uniswap do not go too far off.
 * - Implement the auction for deciding who to delegate to.
 * - How should we transfer liquidity shares?
*)
type liquidity

val show_liquidity : liquidity -> string
val pp_liquidity : Format.formatter -> liquidity -> unit

val liquidity_of_int : int -> liquidity

type t

val show : t -> string
val pp : Format.formatter -> t -> unit

val make_for_test :
  tez:Tez.t ->
  kit:Kit.t ->
  lqt:liquidity ->
  kit_in_tez_in_prev_block:Q.t ->
  last_level:Level.t ->
  t

(** The initial state of the uniswap contract. All amounts are set to zero, and
  * the last-observed kit-in-tez price is undefined (0/0). *)
val make_initial : Level.t -> t

(** Check whether the uniswap contract contains zero tez. *)
val is_tez_pool_empty : t -> bool

(** Check whether the uniswap contract contains zero kit. *)
val is_token_pool_empty : t -> bool

(** Check whether the uniswap contract contains zero liquidity tokens. *)
val is_liquidity_token_pool_empty : t -> bool

(* NOTE: Let's keep internal for now, for safety.
(** Compute the current price of kit in tez, as estimated using the ratio of
  * tez and kit currently in the uniswap contract. *)
val kit_in_tez : t -> Q.t
*)

(** Compute the price of kit in tez (ration of tez and kit in the uniswap
  * contract), as it was at the end of the last block. This is to be used when
  * required for the calculation of the drift derivative instead of up-to-date
  * kit_in_tez, because it is a little harder to manipulate. *)
val kit_in_tez_in_prev_block : t -> Q.t

(** Buy some kit from the uniswap contract. Fail if the desired amount of kit
  * cannot be bought or if the deadline has passed. *)
val buy_kit :
  t ->
  amount:Tez.t ->
  min_kit_expected:Kit.t ->
  tezos:Tezos.t ->
  deadline:Timestamp.t ->
  (Kit.t * t, Error.error) result

(** Sell some kit to the uniswap contract. Fail if the desired amount of tez
  * cannot be bought or if the deadline has passed. *)
val sell_kit :
  t ->
  amount:Tez.t ->
  Kit.t ->
  min_tez_expected:Tez.t ->
  tezos:Tezos.t ->
  deadline:Timestamp.t ->
  (Tez.t * t, Error.error) result

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
val add_liquidity :
  t ->
  amount:Tez.t ->
  max_kit_deposited:Kit.t ->
  min_lqt_minted:liquidity ->
  tezos:Tezos.t ->
  deadline:Timestamp.t ->
  (liquidity * Tez.t * Kit.t * t, Error.error) result

(** Sell some liquidity to the uniswap contract. Selling liquidity always
  * succeeds, but might leave the contract without tez and kit if everybody
  * sells their liquidity. I think it is unlikely to happen, since the last
  * liquidity holders wouldn't want to lose the burrow fees.
*)
val remove_liquidity :
  t ->
  amount:Tez.t ->
  lqt_burned:liquidity ->
  min_tez_withdrawn:Tez.t ->
  min_kit_withdrawn:Kit.t ->
  tezos:Tezos.t ->
  deadline:Timestamp.t ->
  (Tez.t * Kit.t * t, Error.error) result

(** Add accrued burrowing fees to the uniswap contract. *)
val add_accrued_kit :
  t ->
  Tezos.t ->
  Kit.t ->
  t
