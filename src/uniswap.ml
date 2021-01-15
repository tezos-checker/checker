type Error.error +=
  | UniswapNonPositiveInput
  | UniswapTooLate
  | AddLiquidityNoTezGiven
  | AddLiquidityNoKitGiven
  | AddLiquidityNoLiquidityToBeAdded
  | AddLiquidityLessThanOneTez
  | AddLiquidityTooLowLiquidityMinted
  | AddLiquidityTooMuchKitRequired
  | AddLiquidityZeroKitDeposited
  | RemoveLiquidityNonEmptyAmount
  | RemoveLiquidityCantWithdrawEnoughTez
  | RemoveLiquidityCantWithdrawEnoughKit
  | RemoveLiquidityTooMuchTezWithdrawn
  | RemoveLiquidityTooMuchKitWithdrawn
  | RemoveLiquidityNoLiquidityBurned
  | RemoveLiquidityNoTezWithdrawnExpected
  | RemoveLiquidityNoKitWithdrawnExpected
  | BuyKitPriceFailure
  | BuyKitTooLowExpectedKit
  | BuyKitTooMuchKitBought
  | SellKitNonEmptyAmount
  | SellKitPriceFailure
  | SellKitTooLowExpectedTez
  | SellKitTooMuchTezBought
  | InvalidLiquidityToken

(* To be used as the content in liquidity tokens for disambiguation. *)
type liquidity_token_content = Lqt [@@deriving show]

type liquidity = liquidity_token_content Ticket.t [@@deriving show]

let issue_liquidity_tokens ~(tezos: Tezos.t) (i: Nat.t) =
  Ticket.create ~issuer:tezos.self ~amount:i ~content:Lqt

(** Check whether a liquidity token is valid. A liquidity token is valid if (a)
  * it is issued by checker, its amount is non-negative (George: I assume that
  * the ticket mechanism gives us that for free, by using nat?), and (c) is
  * tagged appropriately (this is already enforced by its type). *)
let is_liquidity_token_valid
    ~(tezos:Tezos.t)
    ~(liquidity: liquidity)
  : (liquidity, Error.error) result =
  let issuer, _amount, _content, same_ticket = Ticket.read liquidity in
  let is_valid = issuer = tezos.self in (* NOTE: perhaps we want amount = 0 to be invalid here already *)
  if is_valid then Ok same_ticket else Error InvalidLiquidityToken

let with_valid_liquidity_token
    ~(tezos:Tezos.t)
    ~(liquidity: liquidity)
    (f: liquidity -> ('a, Error.error) result)
  : ('a, Error.error) result =
  match is_liquidity_token_valid ~tezos ~liquidity with
  | Error err -> Error err
  | Ok ticket -> f ticket

type t =
  { tez: Tez.t;
    kit: Kit.token;
    lqt: liquidity;
    (* George: I don't expect this to get really big in size cause it's
     * always derived by dividing uniswap.tez / uniswap.kit (i.e. even if they
     * are relatively prime, we are OK). *)
    kit_in_tez_in_prev_block: Ratio.t [@printer Ratio.pp];
    last_level: Level.t;
  }
[@@deriving show]

let make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block ~last_level =
  { tez = tez;
    kit = kit;
    lqt = lqt;
    kit_in_tez_in_prev_block = kit_in_tez_in_prev_block;
    last_level = last_level;
  }

let make_initial ~tezos =
  { tez = Tez.of_mutez Z.one;
    kit = Kit.issue ~tezos (Kit.of_mukit Z.one);
    lqt = issue_liquidity_tokens ~tezos (Nat.from_literal 1);
    kit_in_tez_in_prev_block = Ratio.one; (* Same as tez/kit now. *)
    last_level = tezos.level;
  }

let is_tez_pool_empty (u: t) =
  assert (u.tez >= Tez.zero);
  u.tez = Tez.zero

let is_kit_pool_empty (u: t) =
  let kit, _same_token = Kit.read_kit u.kit in (* NOTE: replace? *)
  kit = Kit.zero

(* NOTE: Make sure to restore the ticket. *)
let is_liquidity_token_pool_empty (u: t) =
  let _, n, _, _same_ticket = Ticket.read u.lqt in
  n = Nat.from_literal 0

(* When the uniswap is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let assert_initialized (u: t) =
  assert (not (is_tez_pool_empty u));
  assert (not (is_kit_pool_empty u));
  assert (not (is_liquidity_token_pool_empty u))

(* NOTE: FOR TESTING ONLY *)
let kit_in_tez (u: t) =
  let u_kit, _same_token = Kit.read_kit u.kit in (* NOTE: replace? *)
  Ratio.div (Tez.to_ratio u.tez) (Kit.to_ratio u_kit)

(* NOTE: FOR TESTING ONLY *)
let kit_times_tez (u: t) =
  let u_kit, _same_token = Kit.read_kit u.kit in (* NOTE: replace? *)
  Ratio.mul (Tez.to_ratio u.tez) (Kit.to_ratio u_kit)

(** NOTE: FOR TESTING ONLY *)
let liquidity_tokens_extant (u: t) = u.lqt

let kit_in_tez_in_prev_block (uniswap: t) =
  assert_initialized uniswap;
  uniswap.kit_in_tez_in_prev_block

(* Update the kit_in_tez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_tez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * uniswap contract was touched. *)
let sync_last_observed (uniswap: t) (tezos: Tezos.t) =
  assert (tezos.level >= uniswap.last_level); (* TODO: can it be later?? *)
  if uniswap.last_level = tezos.level then
    (* do nothing if it's been touched already in this block *)
    uniswap
  else
    let uniswap_kit, _same_token = Kit.read_kit uniswap.kit in (* NOTE: replace? *)
    { uniswap with
      kit_in_tez_in_prev_block = Ratio.div (Tez.to_ratio uniswap.tez) (Kit.to_ratio uniswap_kit);
      last_level = tezos.level;
    }

let buy_kit (uniswap: t) ~amount ~min_kit_expected ~tezos ~deadline =
  let uniswap = sync_last_observed uniswap tezos in
  assert_initialized uniswap;
  if (amount <= Tez.zero) then
    Error UniswapNonPositiveInput
  else if (tezos.now >= deadline) then
    Error UniswapTooLate
  else if (min_kit_expected <= Kit.zero) then
    Error BuyKitTooLowExpectedKit
  else
    let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = Ratio.div (Kit.to_ratio uniswap_kit) (Tez.to_ratio uniswap.tez) in
    let slippage = Ratio.make (Tez.to_mutez uniswap.tez) (Tez.to_mutez (Tez.add uniswap.tez amount)) in
    let return =
      Kit.of_ratio_floor
        (Ratio.mul
           (Tez.to_ratio amount)
           (Ratio.mul
              price
              (Ratio.mul
                 slippage
                 (Ratio.sub Ratio.one Constants.uniswap_fee)
              )
           )
        ) in
    if return < min_kit_expected then
      Error BuyKitPriceFailure
    else if return > uniswap_kit then
      Error BuyKitTooMuchKitBought
    else
      let bought_kit, remaining_kit = Kit.split_or_fail all_kit_in_uniswap return (Kit.sub uniswap_kit return) in
      Ok ( bought_kit,
           { uniswap with
             kit = remaining_kit;
             tez = Tez.add uniswap.tez amount }
         )

let sell_kit (uniswap: t) ~amount (token: Kit.token) ~min_tez_expected ~tezos ~deadline =
  let uniswap = sync_last_observed uniswap tezos in
  let kit, token = Kit.read_kit token in
  let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
  assert_initialized uniswap;
  if (kit <= Kit.zero) then
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
    let price = Ratio.div (Tez.to_ratio uniswap.tez) (Kit.to_ratio uniswap_kit) in
    let slippage = Ratio.div (Kit.to_ratio uniswap_kit) (Kit.to_ratio (Kit.add uniswap_kit kit)) in
    let return =
      Tez.of_ratio_floor
        (Ratio.mul
           (Kit.to_ratio kit)
           (Ratio.mul
              price
              (Ratio.mul
                 slippage
                 (Ratio.sub Ratio.one Constants.uniswap_fee)
              )
           )
        ) in
    if return < min_tez_expected then
      Error SellKitPriceFailure
    else if return > uniswap.tez then
      Error SellKitTooMuchTezBought
    else
      let new_all_kit_in_uniswap = Kit.join_or_fail all_kit_in_uniswap token in
      Ok ( return,
           { uniswap with
             kit = new_all_kit_in_uniswap;
             tez = Tez.sub uniswap.tez return }
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
let add_liquidity (uniswap: t) ~tezos ~amount ~pending_accrual ~max_kit_deposited ~min_lqt_minted ~deadline =
  let uniswap = sync_last_observed uniswap tezos in
  let max_kit_deposited, all_kit_deposited = Kit.read_kit max_kit_deposited in
  let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
  assert_initialized uniswap;
  if tezos.now >= deadline then
    Error UniswapTooLate
  else if amount = Tez.zero then
    Error AddLiquidityNoTezGiven
  else if max_kit_deposited = Kit.zero then
    Error AddLiquidityNoKitGiven
  else if min_lqt_minted = Nat.from_literal 0 then
    Error AddLiquidityNoLiquidityToBeAdded
  else
    let _, uniswap_lqt, _, _same_ticket = Ticket.read uniswap.lqt in (* TODO: Make sure to restore the ticket. *)
    let effective_tez_balance = Tez.add uniswap.tez pending_accrual in
    let lqt_minted =
      Ratio.to_nat_floor
        (Ratio.mul
           (Ratio.of_nat uniswap_lqt)
           (Ratio.make (Tez.to_mutez amount) (Tez.to_mutez effective_tez_balance))
        ) in
    let kit_deposited =
      Kit.of_ratio_ceil
        (Ratio.mul
           (Kit.to_ratio uniswap_kit)
           (Ratio.make (Tez.to_mutez amount) (Tez.to_mutez effective_tez_balance))
        ) in
    if lqt_minted < min_lqt_minted then
      Error AddLiquidityTooLowLiquidityMinted
    else if max_kit_deposited < kit_deposited then
      Error AddLiquidityTooMuchKitRequired
    else if kit_deposited = Kit.zero then
      Error AddLiquidityZeroKitDeposited
    else
      let kit_deposited, kit_to_return =
        Kit.split_or_fail
          all_kit_deposited
          kit_deposited
          (Kit.sub max_kit_deposited kit_deposited) in
      let new_all_kit_in_uniswap = Kit.join_or_fail all_kit_in_uniswap kit_deposited in
      let liq_tokens = issue_liquidity_tokens ~tezos lqt_minted in
      let updated = { uniswap with
                      kit = new_all_kit_in_uniswap;
                      tez = Tez.add uniswap.tez amount;
                      lqt = Option.get (Ticket.join uniswap.lqt liq_tokens) } in (* NOTE: SHOULD NEVER FAIL!! *)
      (* EXPECTED PROPERTY: kit_to_return + final_uniswap_kit = max_kit_deposited + initial_uniswap_kit *)
      Ok (liq_tokens, kit_to_return, updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
(* TODO: for the purpose of removing liquidity, the bid accrues only after the next period begins. *)
let remove_liquidity (uniswap: t) ~tezos ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline
  : (Tez.t * Kit.token * t, Error.error) result =
  with_valid_liquidity_token ~tezos ~liquidity:lqt_burned @@ fun lqt_burned ->
  let uniswap = sync_last_observed uniswap tezos in
  assert_initialized uniswap;
  let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
  let _, lqt_burned, _, _ = Ticket.read lqt_burned in (* NOTE: consumed, right here. *)
  if amount <> Tez.zero then
    Error RemoveLiquidityNonEmptyAmount
  else if tezos.now >= deadline then
    Error UniswapTooLate
  else if lqt_burned = Nat.from_literal 0 then
    Error RemoveLiquidityNoLiquidityBurned
  else if min_tez_withdrawn <= Tez.zero then
    Error RemoveLiquidityNoTezWithdrawnExpected
  else if min_kit_withdrawn <= Kit.zero then
    Error RemoveLiquidityNoKitWithdrawnExpected
    (* TODO: Check whether we have more edge cases to give a failure for. *)
  else
    let _, uniswap_lqt, _, same_ticket = Ticket.read uniswap.lqt in
    assert (lqt_burned <= uniswap_lqt); (* the ticket mechanism should enforce this *)
    let ratio = Ratio.make (Nat.int lqt_burned) (Nat.int uniswap_lqt) in
    let tez_withdrawn = Tez.of_ratio_floor (Ratio.mul (Tez.to_ratio uniswap.tez) ratio) in
    let kit_withdrawn = Kit.of_ratio_floor (Ratio.mul (Kit.to_ratio uniswap_kit) ratio) in

    if tez_withdrawn < min_tez_withdrawn then
      Error RemoveLiquidityCantWithdrawEnoughTez
    else if tez_withdrawn > uniswap.tez then
      Error RemoveLiquidityTooMuchTezWithdrawn
    else if kit_withdrawn < min_kit_withdrawn then
      Error RemoveLiquidityCantWithdrawEnoughKit
    else if kit_withdrawn > uniswap_kit then
      Error RemoveLiquidityTooMuchKitWithdrawn
    else
      let remaining_lqt, _burned = Option.get ( (* NOTE: SHOULD NEVER FAIL!! *)
          match Nat.is_nat (Nat.sub uniswap_lqt lqt_burned) with
          | None -> failwith "Uniswap.remove_liquidity: impossible"
          | Some remaining -> Ticket.split same_ticket remaining lqt_burned
        ) in

      let kit_withdrawn, remaining_kit = Kit.split_or_fail all_kit_in_uniswap kit_withdrawn (Kit.sub uniswap_kit kit_withdrawn) in
      let updated = { uniswap with
                      tez = Tez.sub uniswap.tez tez_withdrawn;
                      kit = remaining_kit;
                      lqt = remaining_lqt } in
      Ok (tez_withdrawn, kit_withdrawn, updated)

let add_accrued_kit (uniswap: t) ~tezos (accrual: Kit.token) : t =
  let uniswap = sync_last_observed uniswap tezos in
  { uniswap with kit = Kit.join_or_fail uniswap.kit accrual }

let add_accrued_tez (uniswap: t) tezos (accrual: Tez.t) : t =
  let uniswap = sync_last_observed uniswap tezos in
  { uniswap with tez = Tez.add uniswap.tez accrual }
