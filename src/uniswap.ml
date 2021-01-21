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

type liquidity = liquidity_token_content Ligo.ticket [@@deriving show]

let issue_liquidity_tokens (i: Ligo.nat) = Ligo.Tezos.create_ticket Lqt i

(** Check whether a liquidity token is valid. A liquidity token is valid if it
  * is issued by checker, and it is tagged appropriately (this is already
  * enforced by its type). *)
let is_liquidity_token_valid
    ~(liquidity: liquidity)
  : (liquidity, Error.error) result =
  let (issuer, _content, _amount), liquidity = Ligo.Tezos.read_ticket liquidity in
  if issuer = Ligo.Tezos.self then Ok liquidity else Error InvalidLiquidityToken

let with_valid_liquidity_token
    ~(liquidity: liquidity)
    (f: liquidity -> ('a, Error.error) result)
  : ('a, Error.error) result =
  match is_liquidity_token_valid ~liquidity with
  | Error err -> Error err
  | Ok ticket -> f ticket

type t =
  { tez: Ligo.tez;
    kit: Kit.token;
    lqt: liquidity;
    (* George: I don't expect this to get really big in size cause it's
     * always derived by dividing uniswap.tez / uniswap.kit (i.e. even if they
     * are relatively prime, we are OK). *)
    kit_in_tez_in_prev_block: Ratio.t [@printer Ratio.pp];
    last_level: Ligo.nat;
  }
[@@deriving show]

let make_initial =
  { tez = Ligo.tez_from_literal "1mutez";
    kit = Kit.issue (Kit.of_mukit (Ligo.int_from_literal "1"));
    lqt = issue_liquidity_tokens (Ligo.nat_from_literal "1n");
    kit_in_tez_in_prev_block = Ratio.one; (* Same as tez/kit now. *)
    last_level = !Ligo.Tezos.level;
  }

(* When the uniswap is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let assert_initialized (u: t) =
  let is_tez_pool_empty (u: t) =
    assert (u.tez >= Ligo.tez_from_literal "0mutez");
    u.tez = Ligo.tez_from_literal "0mutez" in
  let is_kit_pool_empty (u: t) =
    let kit, _same_token = Kit.read_kit u.kit in
    kit = Kit.zero in
  let is_liquidity_token_pool_empty (u: t) =
    (* NOTE: this part consumes the ticket. *)
    let (_, _, n), _same_ticket = Ligo.Tezos.read_ticket u.lqt in
    n = Ligo.nat_from_literal "0n" in
  assert (not (is_tez_pool_empty u));
  assert (not (is_kit_pool_empty u));
  assert (not (is_liquidity_token_pool_empty u))

let kit_in_tez_in_prev_block (uniswap: t) =
  assert_initialized uniswap;
  uniswap.kit_in_tez_in_prev_block

(* Update the kit_in_tez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_tez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * uniswap contract was touched. *)
let sync_last_observed (uniswap: t) =
  assert (!Ligo.Tezos.level >= uniswap.last_level); (* TODO: can it be later?? *)
  if uniswap.last_level = !Ligo.Tezos.level then
    (* do nothing if it's been touched already in this block *)
    uniswap
  else
    let uniswap_kit, _same_token = Kit.read_kit uniswap.kit in (* NOTE: replace? *)
    { uniswap with
      kit_in_tez_in_prev_block = Ratio.div (Ratio.of_tez uniswap.tez) (Kit.to_ratio uniswap_kit);
      last_level = !Ligo.Tezos.level;
    }

let buy_kit (uniswap: t) ~amount ~min_kit_expected ~deadline =
  let uniswap = sync_last_observed uniswap in
  assert_initialized uniswap;
  if (amount <= Ligo.tez_from_literal "0mutez") then
    Error UniswapNonPositiveInput
  else if (!Ligo.Tezos.now >= deadline) then
    Error UniswapTooLate
  else if (min_kit_expected <= Kit.zero) then
    Error BuyKitTooLowExpectedKit
  else
    let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = Ratio.div (Kit.to_ratio uniswap_kit) (Ratio.of_tez uniswap.tez) in
    let slippage = Ratio.make (Common.tez_to_mutez uniswap.tez) (Common.tez_to_mutez (Ligo.add_tez_tez uniswap.tez amount)) in
    let return =
      Kit.of_ratio_floor
        (Ratio.mul
           (Ratio.of_tez amount)
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
             tez = Ligo.add_tez_tez uniswap.tez amount }
         )

let sell_kit (uniswap: t) ~amount (token: Kit.token) ~min_tez_expected ~deadline =
  let uniswap = sync_last_observed uniswap in
  let kit, token = Kit.read_kit token in
  let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
  assert_initialized uniswap;
  if (kit <= Kit.zero) then
    Error UniswapNonPositiveInput
  else if !Ligo.Tezos.now >= deadline then
    Error UniswapTooLate
  else if amount <> Ligo.tez_from_literal "0mutez" then
    Error SellKitNonEmptyAmount
  else if (min_tez_expected <= Ligo.tez_from_literal "0mutez") then
    Error SellKitTooLowExpectedTez
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = Ratio.div (Ratio.of_tez uniswap.tez) (Kit.to_ratio uniswap_kit) in
    let slippage = Ratio.div (Kit.to_ratio uniswap_kit) (Kit.to_ratio (Kit.add uniswap_kit kit)) in
    let return =
      Ratio.to_tez_floor
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
             tez = Ligo.sub_tez_tez uniswap.tez return }
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
let add_liquidity (uniswap: t) ~amount ~pending_accrual ~max_kit_deposited ~min_lqt_minted ~deadline =
  let uniswap = sync_last_observed uniswap in
  let max_kit_deposited, all_kit_deposited = Kit.read_kit max_kit_deposited in
  let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
  assert_initialized uniswap;
  if !Ligo.Tezos.now >= deadline then
    Error UniswapTooLate
  else if amount = Ligo.tez_from_literal "0mutez" then
    Error AddLiquidityNoTezGiven
  else if max_kit_deposited = Kit.zero then
    Error AddLiquidityNoKitGiven
  else if min_lqt_minted = Ligo.nat_from_literal "0n" then
    Error AddLiquidityNoLiquidityToBeAdded
  else
    let (_, _, uniswap_lqt), _same_ticket = Ligo.Tezos.read_ticket uniswap.lqt in (* TODO: Make sure to restore the ticket. *)
    let effective_tez_balance = Ligo.add_tez_tez uniswap.tez pending_accrual in
    let lqt_minted =
      Ratio.to_nat_floor
        (Ratio.mul
           (Ratio.of_nat uniswap_lqt)
           (Ratio.make (Common.tez_to_mutez amount) (Common.tez_to_mutez effective_tez_balance))
        ) in
    let kit_deposited =
      Kit.of_ratio_ceil
        (Ratio.mul
           (Kit.to_ratio uniswap_kit)
           (Ratio.make (Common.tez_to_mutez amount) (Common.tez_to_mutez effective_tez_balance))
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
      let liq_tokens = issue_liquidity_tokens lqt_minted in
      let updated = { uniswap with
                      kit = new_all_kit_in_uniswap;
                      tez = Ligo.add_tez_tez uniswap.tez amount;
                      lqt = Option.get (Ligo.Tezos.join_tickets uniswap.lqt liq_tokens) } in (* NOTE: SHOULD NEVER FAIL!! *)
      (* EXPECTED PROPERTY: kit_to_return + final_uniswap_kit = max_kit_deposited + initial_uniswap_kit *)
      Ok (liq_tokens, kit_to_return, updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
(* TODO: for the purpose of removing liquidity, the bid accrues only after the next period begins. *)
let remove_liquidity (uniswap: t) ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline
  : (Ligo.tez * Kit.token * t, Error.error) result =
  with_valid_liquidity_token ~liquidity:lqt_burned @@ fun lqt_burned ->
  let uniswap = sync_last_observed uniswap in
  assert_initialized uniswap;
  let uniswap_kit, all_kit_in_uniswap = Kit.read_kit uniswap.kit in
  let (_, _, lqt_burned), _ = Ligo.Tezos.read_ticket lqt_burned in (* NOTE: consumed, right here. *)
  if amount <> Ligo.tez_from_literal "0mutez" then
    Error RemoveLiquidityNonEmptyAmount
  else if !Ligo.Tezos.now >= deadline then
    Error UniswapTooLate
  else if lqt_burned = Ligo.nat_from_literal "0n" then
    Error RemoveLiquidityNoLiquidityBurned
  else if min_tez_withdrawn <= Ligo.tez_from_literal "0mutez" then
    Error RemoveLiquidityNoTezWithdrawnExpected
  else if min_kit_withdrawn <= Kit.zero then
    Error RemoveLiquidityNoKitWithdrawnExpected
    (* TODO: Check whether we have more edge cases to give a failure for. *)
  else
    let (_, _, uniswap_lqt), same_ticket = Ligo.Tezos.read_ticket uniswap.lqt in
    assert (lqt_burned <= uniswap_lqt); (* the ticket mechanism should enforce this *)
    let ratio = Ratio.make (Ligo.int lqt_burned) (Ligo.int uniswap_lqt) in
    let tez_withdrawn = Ratio.to_tez_floor (Ratio.mul (Ratio.of_tez uniswap.tez) ratio) in
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
          match Ligo.is_nat (Ligo.sub_nat_nat uniswap_lqt lqt_burned) with
          | None -> failwith "Uniswap.remove_liquidity: impossible"
          | Some remaining -> Ligo.Tezos.split_ticket same_ticket (remaining, lqt_burned)
        ) in

      let kit_withdrawn, remaining_kit = Kit.split_or_fail all_kit_in_uniswap kit_withdrawn (Kit.sub uniswap_kit kit_withdrawn) in
      let updated = { uniswap with
                      tez = Ligo.sub_tez_tez uniswap.tez tez_withdrawn;
                      kit = remaining_kit;
                      lqt = remaining_lqt } in
      Ok (tez_withdrawn, kit_withdrawn, updated)

let add_accrued_kit (uniswap: t) (accrual: Kit.token) : t =
  let uniswap = sync_last_observed uniswap in
  { uniswap with kit = Kit.join_or_fail uniswap.kit accrual }

let add_accrued_tez (uniswap: t) (accrual: Ligo.tez) : t =
  let uniswap = sync_last_observed uniswap in
  { uniswap with tez = Ligo.add_tez_tez uniswap.tez accrual }

(* BEGIN_OCAML *)
let make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block ~last_level =
  { tez = tez;
    kit = kit;
    lqt = lqt;
    kit_in_tez_in_prev_block = kit_in_tez_in_prev_block;
    last_level = last_level;
  }

let kit_in_tez (u: t) =
  let u_kit, _same_token = Kit.read_kit u.kit in
  Ratio.div (Ratio.of_tez u.tez) (Kit.to_ratio u_kit)

let kit_times_tez (u: t) =
  let u_kit, _same_token = Kit.read_kit u.kit in
  Ratio.mul (Ratio.of_tez u.tez) (Kit.to_ratio u_kit)

let liquidity_tokens_extant (u: t) = u.lqt
(* END_OCAML *)
