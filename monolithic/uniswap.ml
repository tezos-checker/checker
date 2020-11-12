(* ************************************************************************* *)
(*                                Uniswap                                    *)
(* ************************************************************************* *)
type liquidity = int [@@deriving show]

let liquidity_of_int i = i

(* TODO: The state of uniswap should also (in the future) include an ongoing
 * auction to decide who to delegate to, possibly multiple tez balances, etc.
 * Just leaving this note here lest we forget. *)
type t =
  { tez: Tez.t;
    kit: Kit.t;
    total_liquidity_tokens: liquidity;
  }
[@@deriving show]

let uniswap_non_empty (u: t) =
  u.kit > Kit.zero && u.tez > Tez.zero

let kit_in_tez (uniswap: t) = Q.(Tez.to_q uniswap.tez / Kit.to_q uniswap.kit)

type Error.error +=
  | EmptyUniswap
  | UniswapNonPositiveInput
  | UniswapTooLate
  | UniswapBuyKitPriceFailure
  | UniswapSellKitPriceFailure

let buy_kit (uniswap: t) (tez: Tez.t) ~min_kit_expected ~now ~deadline =
  if not (uniswap_non_empty uniswap) then
    Error EmptyUniswap
  else if (tez <= Tez.zero) then
    Error UniswapNonPositiveInput
  else if now > deadline then
    Error UniswapTooLate
  else
    let price = Q.(Kit.to_q uniswap.kit / Tez.to_q uniswap.tez) in
    let slippage = Q.(Tez.to_q uniswap.tez / Tez.(to_q (uniswap.tez + tez))) in
    let return = (* TODO: floor or ceil? *)
      Kit.of_q_floor Q.(Tez.to_q tez * price * slippage * (one - Constants.uniswap_fee)) in

    if return < min_kit_expected then
      Error UniswapBuyKitPriceFailure
    else
      Ok ( return,
           { uniswap with
             kit = Kit.(uniswap.kit - return);
             tez = Tez.(uniswap.tez + tez) }
         )

let sell_kit (uniswap: t) (kit: Kit.t) ~min_tez_expected ~now ~deadline =
  (* Utku: I think, as long as the contract has non-zero tez and kit this
   * always succeeds.
   *
   * So, I think the function should fail when the contract is missing either
   * currency. It will presumably be started with some amount of tez, and
   * the first minting fee will initialize the kit amount.
  *)
  if not (uniswap_non_empty uniswap) then
    Error EmptyUniswap
  else if (kit <= Kit.zero) then
    Error UniswapNonPositiveInput
  else if now > deadline then
    Error UniswapTooLate
  else
    let price = Q.(Tez.to_q uniswap.tez / Kit.to_q uniswap.kit) in
    let slippage = Q.(Kit.to_q uniswap.kit / Kit.(to_q (uniswap.kit + kit))) in
    let return = (* TODO: floor or ceil? *)
      Tez.of_q_floor Q.(Kit.to_q kit * price * slippage * (one - Constants.uniswap_fee)) in

    if return < min_tez_expected then
      Error UniswapSellKitPriceFailure
    else
      Ok ( return,
           { uniswap with
             kit = Kit.(uniswap.kit + kit);
             tez = Tez.(uniswap.tez - return) }
         )

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
let buy_liquidity (uniswap: t) (tez: Tez.t) (kit: Kit.t)
  : liquidity * Tez.t * Kit.t * t =
  (* Adding liquidity always succeeds, if the exchange has non-zero amount. *)
  assert (uniswap_non_empty uniswap);
  let ratio = kit_in_tez uniswap in
  (* There is a chance that the given tez and kit have the wrong ratio,
   * so we liquidate as much as we can and return the leftovers. NOTE:
   * Alternatively, the LP can use the uniswap contract to get the right
   * ratio beforehand.
   * Invariant here is that (tez', kit') should have the correct ratio.
  *)
  let (tez', kit') =
    if Q.(Tez.to_q tez * Kit.to_q uniswap.kit > Kit.to_q kit * Tez.to_q uniswap.tez)
    then (Tez.of_q_floor Q.(Kit.to_q kit * ratio), kit) (* TODO: floor or ceil? *)
    else if Q.(Tez.to_q tez * Kit.to_q uniswap.kit < Kit.to_q kit * Tez.to_q uniswap.tez)
    then (tez, Kit.of_q_floor Q.(Tez.to_q tez / ratio)) (* TODO: floor or ceil? *)
    else (tez, kit)
  in
  let liquidity =
    if uniswap.total_liquidity_tokens = 0
    then 1
    else Q.(
        to_int ( (* floors it *)
          of_int uniswap.total_liquidity_tokens
          * Tez.to_q tez'
          / Tez.to_q uniswap.tez)
      ) in
  let updated =
    { kit = Kit.(uniswap.kit + kit');
      tez = Tez.(uniswap.tez + tez');
      total_liquidity_tokens =
        uniswap.total_liquidity_tokens + liquidity } in
  (liquidity, Tez.(tez - tez'), Kit.(kit - kit'), updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees.
*)
let sell_liquidity (uniswap: t) (liquidity: liquidity)
  : Tez.t * Kit.t * t =
  (* Since this requires a liquidity token, contract can not be empty *)
  assert(uniswap_non_empty(uniswap));
  let ratio = Q.(of_int liquidity / of_int uniswap.total_liquidity_tokens) in
  let tez = Tez.of_q_floor Q.(Tez.to_q uniswap.tez * ratio) in
  let kit = Kit.of_q_floor Q.(Kit.to_q uniswap.kit * ratio) in
  let updated = {
    tez = Tez.(uniswap.tez - tez);
    kit = Kit.(uniswap.kit - kit);
    total_liquidity_tokens = uniswap.total_liquidity_tokens - liquidity } in
  (tez, kit, updated)

let add_accrued_kit (uniswap: t) (accrual: Kit.t) : t =
  { uniswap with kit = Kit.(uniswap.kit + accrual) }
