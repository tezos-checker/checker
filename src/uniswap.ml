open Ratio
open Kit
open Common
open Constants

(* To be used as the content in liquidity tokens for disambiguation. *)
type liquidity_token_content = Lqt [@@deriving show]

type liquidity = liquidity_token_content Ligo.ticket [@@deriving show]

let issue_liquidity_tokens (n: Ligo.nat) : liquidity = Ligo.Tezos.create_ticket (Lqt) n

(** Check whether a liquidity token is valid. A liquidity token is valid if it
  * is issued by checker, and it is tagged appropriately (this is already
  * enforced by its type). *)
let assert_valid_liquidity_token (liquidity: liquidity) : liquidity =
  let (issuer, (_content, _lqt)), liquidity = Ligo.Tezos.read_ticket liquidity in
  if issuer = checker_address
  then liquidity
  else (failwith "InvalidLiquidityToken" : liquidity)

type uniswap =
  { tez: Ligo.tez;
    kit: kit_token;
    lqt: liquidity;
    (* George: I don't expect this to get really big in size cause it's
     * always derived by dividing uniswap.tez / uniswap.kit (i.e. even if they
     * are relatively prime, we are OK). *)
    kit_in_tez_in_prev_block: ratio [@printer pp_ratio];
    last_level: Ligo.nat;
  }
[@@deriving show]

let uniswap_make_initial =
  { tez = Ligo.tez_from_literal "1mutez";
    kit = kit_issue (kit_of_mukit (Ligo.int_from_literal "1"));
    lqt = issue_liquidity_tokens (Ligo.nat_from_literal "1n");
    kit_in_tez_in_prev_block = one_ratio; (* Same as tez/kit now. *)
    last_level = !Ligo.tezos_level;
  }

(* When the uniswap is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let uniswap_assert_initialized (u: uniswap) : uniswap =
  let kit, u_kit = read_kit u.kit in
  let (_issuer, (_content, lqt)), u_lqt = Ligo.Tezos.read_ticket u.lqt in
  assert (not (u.tez = Ligo.tez_from_literal "0mutez"));
  assert (not (kit = kit_zero));
  assert (not (lqt = Ligo.nat_from_literal "0n"));
  {u with kit = u_kit; lqt = u_lqt;}

let uniswap_kit_in_tez_in_prev_block (uniswap: uniswap) =
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  uniswap.kit_in_tez_in_prev_block

(* Update the kit_in_tez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_tez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * uniswap contract was touched. *)
let uniswap_sync_last_observed (uniswap: uniswap) : uniswap =
  assert (!Ligo.tezos_level >= uniswap.last_level); (* TODO: can it be later?? *)
  if uniswap.last_level = !Ligo.tezos_level then
    (* do nothing if it's been touched already in this block *)
    uniswap
  else
    let uniswap_kit, same_kit = read_kit uniswap.kit in
    { uniswap with
      kit = same_kit;
      kit_in_tez_in_prev_block = div_ratio (ratio_of_tez uniswap.tez) (kit_to_ratio uniswap_kit);
      last_level = !Ligo.tezos_level;
    }

let uniswap_buy_kit
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (min_kit_expected: kit)
    (deadline: Ligo.timestamp)
  : (kit_token * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  if (tez_amount = Ligo.tez_from_literal "0mutez") then
    (failwith "UniswapNonPositiveInput" : (kit_token * uniswap))
  else if (!Ligo.Tezos.now >= deadline) then
    (failwith "UniswapTooLate" : (kit_token * uniswap))
  else if (min_kit_expected <= kit_zero) then
    (failwith "BuyKitTooLowExpectedKit" : (kit_token * uniswap))
  else
    let uniswap_kit, all_kit_in_uniswap = read_kit uniswap.kit in
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = div_ratio (kit_to_ratio uniswap_kit) (ratio_of_tez uniswap.tez) in
    let slippage = make_ratio (tez_to_mutez uniswap.tez) (tez_to_mutez (Ligo.add_tez_tez uniswap.tez tez_amount)) in
    let return =
      kit_of_ratio_floor
        (mul_ratio
           (ratio_of_tez tez_amount)
           (mul_ratio
              price
              (mul_ratio
                 slippage
                 (sub_ratio one_ratio uniswap_fee)
              )
           )
        ) in
    if return < min_kit_expected then
      (failwith "BuyKitPriceFailure" : (kit_token * uniswap))
    else if return > uniswap_kit then
      (failwith "BuyKitTooMuchKitBought" : (kit_token * uniswap))
    else
      let bought_kit, remaining_kit = kit_split_or_fail all_kit_in_uniswap return (kit_sub uniswap_kit return) in
      ( bought_kit,
        { uniswap with
          kit = remaining_kit;
          tez = Ligo.add_tez_tez uniswap.tez tez_amount }
      )

let uniswap_sell_kit
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (token: kit_token)
    (min_tez_expected: Ligo.tez)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let kit, token = read_kit token in
  let uniswap_kit, all_kit_in_uniswap = read_kit uniswap.kit in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  if (kit <= kit_zero) then
    (failwith "UniswapNonPositiveInput" : (Ligo.tez * uniswap))
  else if !Ligo.Tezos.now >= deadline then
    (failwith "UniswapTooLate" : (Ligo.tez * uniswap))
  else if tez_amount <> Ligo.tez_from_literal "0mutez" then
    (failwith "SellKitNonEmptyAmount" : (Ligo.tez * uniswap))
  else if (min_tez_expected = Ligo.tez_from_literal "0mutez") then
    (failwith "SellKitTooLowExpectedTez" : (Ligo.tez * uniswap))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = div_ratio (ratio_of_tez uniswap.tez) (kit_to_ratio uniswap_kit) in
    let slippage = div_ratio (kit_to_ratio uniswap_kit) (kit_to_ratio (kit_add uniswap_kit kit)) in
    let return =
      ratio_to_tez_floor
        (mul_ratio
           (kit_to_ratio kit)
           (mul_ratio
              price
              (mul_ratio
                 slippage
                 (sub_ratio one_ratio uniswap_fee)
              )
           )
        ) in
    if return < min_tez_expected then
      (failwith "SellKitPriceFailure" : (Ligo.tez * uniswap))
    else if return > uniswap.tez then
      (failwith "SellKitTooMuchTezBought" : (Ligo.tez * uniswap))
    else
      let new_all_kit_in_uniswap = kit_join_or_fail all_kit_in_uniswap token in
      ( return,
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
let uniswap_add_liquidity
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (pending_accrual: Ligo.tez)
    (max_kit_deposited: kit_token)
    (min_lqt_minted: Ligo.nat)
    (deadline: Ligo.timestamp)
  : (liquidity * kit_token * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let max_kit_deposited, all_kit_deposited = read_kit max_kit_deposited in
  let uniswap_kit, all_kit_in_uniswap = read_kit uniswap.kit in
  let uniswap = uniswap_assert_initialized uniswap in
  if !Ligo.Tezos.now >= deadline then
    (failwith "UniswapTooLate" : (liquidity * kit_token * uniswap))
  else if tez_amount = Ligo.tez_from_literal "0mutez" then
    (failwith "AddLiquidityNoTezGiven" : (liquidity * kit_token * uniswap))
  else if max_kit_deposited = kit_zero then
    (failwith "AddLiquidityNoKitGiven" : (liquidity * kit_token * uniswap))
  else if min_lqt_minted = Ligo.nat_from_literal "0n" then
    (failwith "AddLiquidityNoLiquidityToBeAdded" : (liquidity * kit_token * uniswap))
  else
    let (_issuer, (_content, uniswap_lqt)), same_uniswap_lqt = Ligo.Tezos.read_ticket uniswap.lqt in
    let uniswap = {uniswap with lqt = same_uniswap_lqt;} in

    let effective_tez_balance = Ligo.add_tez_tez uniswap.tez pending_accrual in
    let lqt_minted =
      ratio_to_nat_floor
        (mul_ratio
           (ratio_of_nat uniswap_lqt)
           (make_ratio (tez_to_mutez tez_amount) (tez_to_mutez effective_tez_balance))
        ) in
    let kit_deposited =
      kit_of_ratio_ceil
        (mul_ratio
           (kit_to_ratio uniswap_kit)
           (make_ratio (tez_to_mutez tez_amount) (tez_to_mutez effective_tez_balance))
        ) in
    if lqt_minted < min_lqt_minted then
      (failwith "AddLiquidityTooLowLiquidityMinted" : (liquidity * kit_token * uniswap))
    else if max_kit_deposited < kit_deposited then
      (failwith "AddLiquidityTooMuchKitRequired" : (liquidity * kit_token * uniswap))
    else if kit_deposited = kit_zero then
      (failwith "AddLiquidityZeroKitDeposited" : (liquidity * kit_token * uniswap))
    else
      let kit_deposited, kit_to_return =
        kit_split_or_fail
          all_kit_deposited
          kit_deposited
          (kit_sub max_kit_deposited kit_deposited) in
      let new_all_kit_in_uniswap = kit_join_or_fail all_kit_in_uniswap kit_deposited in
      let liq_tokens = issue_liquidity_tokens lqt_minted in
      let updated = { uniswap with
                      kit = new_all_kit_in_uniswap;
                      tez = Ligo.add_tez_tez uniswap.tez tez_amount;
                      lqt = match Ligo.Tezos.join_tickets (uniswap.lqt, liq_tokens) with
                        | None -> (failwith "impossible" : liquidity)
                        | Some liq -> liq;
                    } in
      (* EXPECTED PROPERTY: kit_to_return + final_uniswap_kit = max_kit_deposited + initial_uniswap_kit *)
      (liq_tokens, kit_to_return, updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
(* TODO: for the purpose of removing liquidity, the bid accrues only after the next period begins. *)
let uniswap_remove_liquidity
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (lqt_burned: liquidity)
    (min_tez_withdrawn: Ligo.tez)
    (min_kit_withdrawn: kit)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * kit_token * uniswap) =
  let lqt_burned = assert_valid_liquidity_token lqt_burned in
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  let uniswap_kit, all_kit_in_uniswap = read_kit uniswap.kit in
  let (_, (_, lqt_burned)), _ = Ligo.Tezos.read_ticket lqt_burned in (* NOTE: consumed, right here. *)
  if tez_amount <> Ligo.tez_from_literal "0mutez" then
    (failwith "RemoveLiquidityNonEmptyAmount" : (Ligo.tez * kit_token * uniswap))
  else if !Ligo.Tezos.now >= deadline then
    (failwith "UniswapTooLate" : (Ligo.tez * kit_token * uniswap))
  else if lqt_burned = Ligo.nat_from_literal "0n" then
    (failwith "RemoveLiquidityNoLiquidityBurned" : (Ligo.tez * kit_token * uniswap))
  else if min_tez_withdrawn = Ligo.tez_from_literal "0mutez" then
    (failwith "RemoveLiquidityNoTezWithdrawnExpected" : (Ligo.tez * kit_token * uniswap))
  else if min_kit_withdrawn <= kit_zero then
    (failwith "RemoveLiquidityNoKitWithdrawnExpected" : (Ligo.tez * kit_token * uniswap))
    (* TODO: Check whether we have more edge cases to give a failure for. *)
  else
    let (_, (_, uniswap_lqt)), same_ticket = Ligo.Tezos.read_ticket uniswap.lqt in
    assert (lqt_burned <= uniswap_lqt); (* the ticket mechanism should enforce this *)
    let ratio = make_ratio (Ligo.int lqt_burned) (Ligo.int uniswap_lqt) in
    let tez_withdrawn = ratio_to_tez_floor (mul_ratio (ratio_of_tez uniswap.tez) ratio) in
    let kit_withdrawn = kit_of_ratio_floor (mul_ratio (kit_to_ratio uniswap_kit) ratio) in

    if tez_withdrawn < min_tez_withdrawn then
      (failwith "RemoveLiquidityCantWithdrawEnoughTez" : (Ligo.tez * kit_token * uniswap))
    else if tez_withdrawn > uniswap.tez then
      (failwith "RemoveLiquidityTooMuchTezWithdrawn" : (Ligo.tez * kit_token * uniswap))
    else if kit_withdrawn < min_kit_withdrawn then
      (failwith "RemoveLiquidityCantWithdrawEnoughKit" : (Ligo.tez * kit_token * uniswap))
    else if kit_withdrawn > uniswap_kit then
      (failwith "RemoveLiquidityTooMuchKitWithdrawn" : (Ligo.tez * kit_token * uniswap))
    else
      let remaining_lqt, _burned = (
        match Ligo.is_nat (Ligo.sub_nat_nat uniswap_lqt lqt_burned) with
        | None -> (failwith "uniswap_remove_liquidity: impossible" : (liquidity * liquidity))
        | Some remaining -> (
            match Ligo.Tezos.split_ticket same_ticket (remaining, lqt_burned) with
            | None -> (failwith "uniswap_remove_liquidity: impossible" : (liquidity * liquidity))
            | Some remaining_and_burned -> remaining_and_burned
          )
      ) in

      let kit_withdrawn, remaining_kit = kit_split_or_fail all_kit_in_uniswap kit_withdrawn (kit_sub uniswap_kit kit_withdrawn) in
      let updated = { uniswap with
                      tez = Ligo.sub_tez_tez uniswap.tez tez_withdrawn;
                      kit = remaining_kit;
                      lqt = remaining_lqt } in
      (tez_withdrawn, kit_withdrawn, updated)

let uniswap_add_accrued_kit (uniswap: uniswap) (accrual: kit_token) : uniswap =
  let uniswap = uniswap_sync_last_observed uniswap in
  { uniswap with kit = kit_join_or_fail uniswap.kit accrual }

let uniswap_add_accrued_tez (uniswap: uniswap) (accrual: Ligo.tez) : uniswap =
  let uniswap = uniswap_sync_last_observed uniswap in
  { uniswap with tez = Ligo.add_tez_tez uniswap.tez accrual }

(* BEGIN_OCAML *)
let uniswap_make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block ~last_level =
  { tez = tez;
    kit = kit;
    lqt = lqt;
    kit_in_tez_in_prev_block = kit_in_tez_in_prev_block;
    last_level = last_level;
  }

let uniswap_kit_in_tez (u: uniswap) =
  let u_kit, _same_token = read_kit u.kit in
  div_ratio (ratio_of_tez u.tez) (kit_to_ratio u_kit)

let uniswap_kit_times_tez (u: uniswap) =
  let u_kit, _same_token = read_kit u.kit in
  mul_ratio (ratio_of_tez u.tez) (kit_to_ratio u_kit)

let uniswap_liquidity_tokens_extant (u: uniswap) = u.lqt
(* END_OCAML *)
