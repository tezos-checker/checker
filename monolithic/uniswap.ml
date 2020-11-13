(* ************************************************************************* *)
(*                                Uniswap                                    *)
(* ************************************************************************* *)
type liquidity = int [@@deriving show]

let liquidity_of_int i = assert (i > 0); i

(* TODO: The state of uniswap should also (in the future) include an ongoing
 * auction to decide who to delegate to, possibly multiple tez balances, etc.
 * Just leaving this note here lest we forget. *)
type t =
  { tez: Tez.t;
    kit: Kit.t;
    total_liquidity_tokens: liquidity;
  }
[@@deriving show]

let is_tez_pool_empty (u: t) = assert (u.tez >= Tez.zero); u.tez = Tez.zero

let is_token_pool_empty (u: t) = assert (u.kit >= Kit.zero); u.kit = Kit.zero

let is_liquidity_token_pool_empty (u: t) =
  assert (u.total_liquidity_tokens >= 0);
  u.total_liquidity_tokens = 0

let kit_in_tez (uniswap: t) = Q.(Tez.to_q uniswap.tez / Kit.to_q uniswap.kit)

type Error.error +=
  | UniswapEmptyTezPool
  | UniswapEmptyTokenPool
  | UniswapEmptyLiquidityTokenPool
  | UniswapNoTezGiven
  | UniswapNoLiquidityToBeAdded
  | UniswapTooLowLiquidityMinted
  | UniswapTooManyTokensRequired
  | UniswapCantWithdrawEnoughTez
  | UniswapCantWithdrawEnoughTokens
  | UniswapTooManyTezWithdrawn
  | UniswapTooManyTokensWithdrawn
  | UniswapNoLiquidityBurned
  | UniswapTooMuchLiquidityBurned
  | UniswapZeroTokensDeposited
  | UniswapNonPositiveInput
  | UniswapTooLate
  | UniswapBuyKitPriceFailure
  | UniswapSellKitPriceFailure

let buy_kit (uniswap: t) (tez: Tez.t) ~min_kit_expected ~now ~deadline =
  if is_tez_pool_empty uniswap then
    Error UniswapEmptyTezPool
  else if is_token_pool_empty uniswap then
    Error UniswapEmptyTokenPool
  else if (tez <= Tez.zero) then
    Error UniswapNonPositiveInput
  else if now >= deadline then
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
  if is_tez_pool_empty uniswap then
    Error UniswapEmptyTezPool
  else if is_token_pool_empty uniswap then
    Error UniswapEmptyTokenPool
  else if (kit <= Kit.zero) then
    Error UniswapNonPositiveInput
  else if now >= deadline then
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

let add_liquidity (uniswap: t) ~amount ~max_kit_deposited ~min_lqt_minted ~now ~deadline =
  if is_tez_pool_empty uniswap then
    Error UniswapEmptyTezPool
  else if now >= deadline then
    Error UniswapTooLate
  else if amount = Tez.zero then
    Error UniswapNoTezGiven
  else if min_lqt_minted = 0 then
    Error UniswapNoLiquidityToBeAdded
  else
    let lqt_minted = Q.(to_int (of_int uniswap.total_liquidity_tokens * Tez.to_q amount / Tez.to_q uniswap.tez)) in (* floor *)
    let kit_deposited = Kit.of_q_ceil Q.(Kit.to_q uniswap.kit * Tez.to_q amount / Tez.to_q uniswap.tez) in (* ceil *)

    if lqt_minted < min_lqt_minted then
      Error UniswapTooLowLiquidityMinted
    else if max_kit_deposited < kit_deposited then
      Error UniswapTooManyTokensRequired
    else if kit_deposited = Kit.zero then
      Error UniswapZeroTokensDeposited
    else
      let updated =
        { kit = Kit.(uniswap.kit + kit_deposited);
          tez = Tez.(uniswap.tez + amount);
          total_liquidity_tokens = uniswap.total_liquidity_tokens + lqt_minted
        } in
      Ok (lqt_minted, Tez.zero, Kit.(max_kit_deposited - kit_deposited), updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees.
*)
(* TODO: Allowance checks *)
(* TODO: amount = 0 check *)
let remove_liquidity (uniswap: t) ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~now ~deadline
  : (Tez.t * Kit.t * t, Error.error) result =
  if is_liquidity_token_pool_empty uniswap then
    (* Since this requires a liquidity token, contract cannot be empty *)
    Error UniswapEmptyLiquidityTokenPool
  else if now >= deadline then
    Error UniswapTooLate
  else if lqt_burned <= 0 then
    Error UniswapNoLiquidityBurned
  else
    let ratio = Q.(of_int lqt_burned / of_int uniswap.total_liquidity_tokens) in
    let tez_withdrawn = Tez.of_q_floor Q.(Tez.to_q uniswap.tez * ratio) in
    let kit_withdrawn = Kit.of_q_floor Q.(Kit.to_q uniswap.kit * ratio) in

    if tez_withdrawn < min_tez_withdrawn then
      Error UniswapCantWithdrawEnoughTez
    else if tez_withdrawn > uniswap.tez then
      Error UniswapTooManyTezWithdrawn
    else if kit_withdrawn < min_kit_withdrawn then
      Error UniswapCantWithdrawEnoughTokens
    else if kit_withdrawn > uniswap.kit then
      Error UniswapTooManyTokensWithdrawn
    else if lqt_burned > uniswap.total_liquidity_tokens then
      Error UniswapTooMuchLiquidityBurned
    else
      let updated = {
        tez = Tez.(uniswap.tez - tez_withdrawn);
        kit = Kit.(uniswap.kit - kit_withdrawn);
        total_liquidity_tokens = uniswap.total_liquidity_tokens - lqt_burned } in
      Ok (tez_withdrawn, kit_withdrawn, updated)

let add_accrued_kit (uniswap: t) (accrual: Kit.t) : t =
  { uniswap with kit = Kit.(uniswap.kit + accrual) }
