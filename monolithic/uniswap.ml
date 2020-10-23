open Constants
include Constants
open FixedPoint
open Kit
include Parameters
open Tez

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
type liquidity = int

(* TODO: The state of uniswap should also (in the future) include an ongoing
 * auction to decide who to delegate to, possibly multiple tez balances, etc.
 * Just leaving this note here lest we forget. *)
type uniswap =
  { tez: Tez.t;
    kit: Kit.t;
    total_liquidity_tokens: int;
  }

let pp_uniswap (ppf: Format.formatter) (u : uniswap) =
  Format.fprintf
    ppf
    "{tez = %a;\n kit = %a;\n total_liquidity_tokens = %d}\n"
    Tez.pp u.tez
    Kit.pp u.kit
    u.total_liquidity_tokens

let print_uniswap (u : uniswap) = pp_uniswap Format.std_formatter u

let uniswap_non_empty(u: uniswap) =
  u.kit > Kit.zero && u.tez > Tez.zero

let sell_kit (uniswap: uniswap) (kit: Kit.t) : Tez.t * Kit.t * uniswap =
  (* Utku: I think, as long as the contract has non-zero tez and kit this
   * always succeeds.
   *
   * So, I think the function should fail when the contract is missing either
   * currency. It will presumably be started with some amount of tez, and
   * the first minting fee will initialize the kit amount.
  *)
  assert (uniswap_non_empty uniswap);
  assert (kit > Kit.zero);

  let price = FixedPoint.(Tez.to_fp uniswap.tez / Kit.to_fp uniswap.kit) in
  let slippage = Kit.div uniswap.kit (Kit.add uniswap.kit kit) in
  let return = Tez.of_fp FixedPoint.(Kit.to_fp kit * price * slippage * (FixedPoint.one - uniswap_fee_percentage)) in
  let updated = { uniswap with
                  kit = Kit.add uniswap.kit kit;
                  tez = Tez.sub uniswap.tez return } in
  (return, Kit.zero, updated)

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
let buy_liquidity (uniswap: uniswap) (tez: Tez.t) (kit: Kit.t)
  : liquidity * Tez.t * Kit.t * uniswap =
  (* Adding liquidity always succeeds, if the exchange has non-zero amount. *)
  assert (uniswap_non_empty uniswap);
  (* TODO: How to compute things without explicitly calculating the ratio
   * (using division) here? *)
  let ratio = FixedPoint.(Tez.to_fp uniswap.tez / Kit.to_fp uniswap.kit) in
  (* There is a chance that the given tez and kit have the wrong ratio,
   * so we liquidate as much as we can and return the leftovers. NOTE:
   * Alternatively, the LP can use the uniswap contract to get the right
   * ratio beforehand.
   * Invariant here is that (tez', kit') should have the correct ratio.
  *)
  let (tez', kit') = FixedPoint.(
    if Tez.to_fp tez * Kit.to_fp uniswap.kit > Kit.to_fp kit * Tez.to_fp uniswap.tez
    then (Tez.of_fp (Kit.to_fp kit * ratio), kit)
    else if Tez.to_fp tez * Kit.to_fp uniswap.kit < Kit.to_fp kit * Tez.to_fp uniswap.tez
    then (tez, Kit.of_fp (Tez.to_fp tez / ratio))
    else (tez, kit) ) in
  let liquidity =
    if uniswap.total_liquidity_tokens = 0
    then 1
    else int_of_float (floor (
        float_of_int uniswap.total_liquidity_tokens
        *. Tez.to_float tez'
        /. Tez.to_float uniswap.tez))
  in
  let updated =
    { kit = Kit.add uniswap.kit kit';
      tez = Tez.add uniswap.tez tez';
      total_liquidity_tokens =
        uniswap.total_liquidity_tokens + liquidity } in
  (liquidity, Tez.sub tez tez', Kit.sub kit kit', updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees.
*)
let sell_liquidity (uniswap: uniswap) (liquidity: liquidity)
  : Tez.t * Kit.t * uniswap =
  (* Since this requires a liquidity token, contract can not be empty *)
  assert(uniswap_non_empty(uniswap));
  let ratio =
    float_of_int liquidity /. float_of_int uniswap.total_liquidity_tokens in
  let tez = Tez.of_float (Tez.to_float uniswap.tez *. ratio) in
  let kit = Kit.of_float (Kit.to_float uniswap.kit *. ratio) in
  let updated = {
    tez = Tez.sub uniswap.tez tez;
    kit = Kit.sub uniswap.kit kit;
    total_liquidity_tokens = uniswap.total_liquidity_tokens - liquidity } in
  (tez, kit, updated)

