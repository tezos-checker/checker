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
    (* NOTE: George: I don't expect this to get really big in size cause it's
     * always derived by dividing uniswap.tez / uniswap.kit (i.e. even if they
     * are relatively prime, we are OK). *)
    kit_in_tez_in_prev_block: Q.t [@printer Q.pp_print];
    last_level: Level.t;
  }
[@@deriving show]

let make_for_test ~tez ~kit ~total_liquidity_tokens ~kit_in_tez_in_prev_block ~last_level =
  { tez = tez;
    kit = kit;
    total_liquidity_tokens = total_liquidity_tokens;
    kit_in_tez_in_prev_block = kit_in_tez_in_prev_block;
    last_level = last_level;
  }

let make_initial (level: Level.t) =
  { tez = Tez.one;
    kit = Kit.one;
    total_liquidity_tokens = liquidity_of_int 1;
    kit_in_tez_in_prev_block = Q.one; (* TODO: same as tez/kit now *)
    last_level = level;
  }

let is_tez_pool_empty (u: t) = assert (u.tez >= Tez.zero); u.tez = Tez.zero

let is_token_pool_empty (u: t) = assert (u.kit >= Kit.zero); u.kit = Kit.zero

let is_liquidity_token_pool_empty (u: t) =
  assert (u.total_liquidity_tokens >= 0);
  u.total_liquidity_tokens = 0

let kit_in_tez (uniswap: t) = Q.(Tez.to_q uniswap.tez / Kit.to_q uniswap.kit)

let kit_in_tez_in_prev_block (uniswap: t) = uniswap.kit_in_tez_in_prev_block

(* Update the kit_in_tez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_tez at the end of the last block. NOTE: George:
 * this might not be the previous block, but the last block in which the
 * uniswap contract was touched. *)
let sync_last_observed (uniswap: t) (tezos: Tezos.t) =
  assert (tezos.level >= uniswap.last_level); (* TODO: can it be later?? *)
  if uniswap.last_level = tezos.level then
    uniswap (* do nothing if it's been touched already in this block *)
  else
    { uniswap with
        kit_in_tez_in_prev_block = kit_in_tez uniswap;
        last_level = tezos.level;
    }

type Error.error +=
  | UniswapEmptyTezPool
  | UniswapEmptyKitPool
  | UniswapEmptyLiquidityTokenPool
  | UniswapNonPositiveInput
  | UniswapTooLate
  | AddLiquidityNoTezGiven
  | AddLiquidityNoLiquidityToBeAdded
  | AddLiquidityTooLowLiquidityMinted
  | AddLiquidityTooMuchKitRequired
  | AddLiquidityZeroKitDeposited
  | RemoveLiquidityNonEmptyAmount
  | RemoveLiquidityCantWithdrawEnoughTez
  | RemoveLiquidityCantWithdrawEnoughKit
  | RemoveLiquidityTooMuchTezWithdrawn
  | RemoveLiquidityTooMuchKitWithdrawn
  | RemoveLiquidityNoLiquidityBurned
  | RemoveLiquidityTooMuchLiquidityBurned
  | BuyKitPriceFailure
  | BuyKitTooLowExpectedKit
  | BuyKitTooMuchKitBought
  | SellKitNonEmptyAmount
  | SellKitPriceFailure
  | SellKitTooLowExpectedTez
  | SellKitTooMuchTezBought

let buy_kit (uniswap: t) ~amount ~min_kit_expected ~tezos ~deadline =
  let uniswap = sync_last_observed uniswap tezos in
  if (is_tez_pool_empty uniswap) then
    Error UniswapEmptyTezPool
  else if (is_token_pool_empty uniswap) then
    Error UniswapEmptyKitPool
  else if (amount <= Tez.zero) then
    Error UniswapNonPositiveInput
  else if (tezos.now >= deadline) then
    Error UniswapTooLate
  else if (min_kit_expected <= Kit.zero) then
    Error BuyKitTooLowExpectedKit
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = Q.(Kit.to_q uniswap.kit / Tez.to_q uniswap.tez) in
    let slippage = Q.(Tez.to_q uniswap.tez / Tez.(to_q (uniswap.tez + amount))) in
    let return = Kit.of_q_floor Q.(Tez.to_q amount * price * slippage * (one - Constants.uniswap_fee)) in
    if return < min_kit_expected then
      Error BuyKitPriceFailure
    else if return > uniswap.kit then
      Error BuyKitTooMuchKitBought
    else
      Ok ( return,
           { uniswap with
             kit = Kit.(uniswap.kit - return);
             tez = Tez.(uniswap.tez + amount) }
         )

let sell_kit (uniswap: t) ~amount (kit: Kit.t) ~min_tez_expected ~tezos ~deadline =
  let uniswap = sync_last_observed uniswap tezos in
  if is_tez_pool_empty uniswap then
    Error UniswapEmptyTezPool
  else if is_token_pool_empty uniswap then
    Error UniswapEmptyKitPool
  else if (kit <= Kit.zero) then
    Error UniswapNonPositiveInput
  else if tezos.now >= deadline then
    Error UniswapTooLate
  else if amount <> Tez.zero then
    Error SellKitNonEmptyAmount
  else if (min_tez_expected <= Tez.zero) then
    Error SellKitTooLowExpectedTez
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = Q.(Tez.to_q uniswap.tez / Kit.to_q uniswap.kit) in
    let slippage = Q.(Kit.to_q uniswap.kit / Kit.(to_q (uniswap.kit + kit))) in
    let return = Tez.of_q_floor Q.(Kit.to_q kit * price * slippage * (one - Constants.uniswap_fee)) in

    if return < min_tez_expected then
      Error SellKitPriceFailure
    else if return > uniswap.tez then
      Error SellKitTooMuchTezBought
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
 * continuously credited with the burrow fee taken from burrow holders. *)
let add_liquidity (uniswap: t) ~amount ~max_kit_deposited ~min_lqt_minted ~tezos ~deadline =
  let uniswap = sync_last_observed uniswap tezos in
  if is_tez_pool_empty uniswap then
    Error UniswapEmptyTezPool
  else if tezos.now >= deadline then
    Error UniswapTooLate
  else if amount = Tez.zero then
    Error AddLiquidityNoTezGiven
  else if min_lqt_minted = 0 then
    Error AddLiquidityNoLiquidityToBeAdded
  else
    let lqt_minted = Q.(to_int (of_int uniswap.total_liquidity_tokens * Tez.to_q amount / Tez.to_q uniswap.tez)) in (* floor *)
    let kit_deposited = Kit.of_q_ceil Q.(Kit.to_q uniswap.kit * Tez.to_q amount / Tez.to_q uniswap.tez) in (* ceil *)

    if lqt_minted < min_lqt_minted then
      Error AddLiquidityTooLowLiquidityMinted
    else if max_kit_deposited < kit_deposited then
      Error AddLiquidityTooMuchKitRequired
    else if kit_deposited = Kit.zero then
      Error AddLiquidityZeroKitDeposited
    else
      let updated = { uniswap with
        kit = Kit.(uniswap.kit + kit_deposited);
        tez = Tez.(uniswap.tez + amount);
        total_liquidity_tokens = uniswap.total_liquidity_tokens + lqt_minted } in
      Ok (lqt_minted, Tez.zero, Kit.(max_kit_deposited - kit_deposited), updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
(* TODO: Allowance checks *)
(* TODO: for the purpose of removing liquidity, the bid accrues only after the next period begins. *)
let remove_liquidity (uniswap: t) ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~tezos ~deadline
  : (Tez.t * Kit.t * t, Error.error) result =
  let uniswap = sync_last_observed uniswap tezos in
  if is_liquidity_token_pool_empty uniswap then
    (* Since this requires a liquidity token, contract cannot be empty *)
    Error UniswapEmptyLiquidityTokenPool
  else if amount <> Tez.zero then
    Error RemoveLiquidityNonEmptyAmount
  else if tezos.now >= deadline then
    Error UniswapTooLate
  else if lqt_burned <= 0 then
    Error RemoveLiquidityNoLiquidityBurned
  else
    let ratio = Q.(of_int lqt_burned / of_int uniswap.total_liquidity_tokens) in
    let tez_withdrawn = Tez.of_q_floor Q.(Tez.to_q uniswap.tez * ratio) in
    let kit_withdrawn = Kit.of_q_floor Q.(Kit.to_q uniswap.kit * ratio) in

    if tez_withdrawn < min_tez_withdrawn then
      Error RemoveLiquidityCantWithdrawEnoughTez
    else if tez_withdrawn > uniswap.tez then
      Error RemoveLiquidityTooMuchTezWithdrawn
    else if kit_withdrawn < min_kit_withdrawn then
      Error RemoveLiquidityCantWithdrawEnoughKit
    else if kit_withdrawn > uniswap.kit then
      Error RemoveLiquidityTooMuchKitWithdrawn
    else if lqt_burned > uniswap.total_liquidity_tokens then
      Error RemoveLiquidityTooMuchLiquidityBurned
    else
      let updated = { uniswap with
        tez = Tez.(uniswap.tez - tez_withdrawn);
        kit = Kit.(uniswap.kit - kit_withdrawn);
        total_liquidity_tokens = uniswap.total_liquidity_tokens - lqt_burned } in
      Ok (tez_withdrawn, kit_withdrawn, updated)

let add_accrued_kit (uniswap: t) tezos (accrual: Kit.t) : t =
  let uniswap = sync_last_observed uniswap tezos in
  { uniswap with kit = Kit.(uniswap.kit + accrual) }
