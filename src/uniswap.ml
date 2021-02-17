open Ratio
open Kit
open Common
open Constants
open UniswapTypes

(* When the uniswap is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let uniswap_assert_initialized (u: uniswap) : uniswap =
  assert (not (u.tez = Ligo.tez_from_literal "0mutez"));
  assert (not (u.kit = kit_zero));
  assert (not (u.lqt = Ligo.nat_from_literal "0n"));
  u

let uniswap_kit_in_tez_in_prev_block (uniswap: uniswap) =
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  uniswap.kit_in_tez_in_prev_block

(* Update the kit_in_tez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_tez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * uniswap contract was touched. *)
let uniswap_sync_last_observed (uniswap: uniswap) : uniswap =
  assert (!Ligo.Tezos.level >= uniswap.last_level); (* TODO: can it be later?? *)
  if uniswap.last_level = !Ligo.Tezos.level then
    (* do nothing if it's been touched already in this block *)
    uniswap
  else
    { uniswap with
      kit_in_tez_in_prev_block = div_ratio (ratio_of_tez uniswap.tez) (kit_to_ratio uniswap.kit);
      last_level = !Ligo.Tezos.level;
    }

let uniswap_buy_kit
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (min_kit_expected: kit)
    (deadline: Ligo.timestamp)
  : (kit * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  if (tez_amount = Ligo.tez_from_literal "0mutez") then
    (failwith "UniswapNonPositiveInput" : (kit * uniswap))
  else if (!Ligo.Tezos.now >= deadline) then
    (failwith "UniswapTooLate" : (kit * uniswap))
  else if (min_kit_expected = kit_zero) then
    (failwith "BuyKitTooLowExpectedKit" : (kit * uniswap))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let price = div_ratio (kit_to_ratio uniswap.kit) (ratio_of_tez uniswap.tez) in
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
      (failwith "BuyKitPriceFailure" : (kit * uniswap))
    else if return > uniswap.kit then
      (failwith "BuyKitTooMuchKitBought" : (kit * uniswap))
    else
      let bought_kit, remaining_kit = (return, kit_sub uniswap.kit return) in
      ( bought_kit,
        { uniswap with
          kit = remaining_kit;
          tez = Ligo.add_tez_tez uniswap.tez tez_amount }
      )

let uniswap_sell_kit
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (kit: kit)
    (min_tez_expected: Ligo.tez)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  if (kit = kit_zero) then
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
    let price = div_ratio (ratio_of_tez uniswap.tez) (kit_to_ratio uniswap.kit) in
    let slippage = div_ratio (kit_to_ratio uniswap.kit) (kit_to_ratio (kit_add uniswap.kit kit)) in
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
      let new_all_kit_in_uniswap = kit_add uniswap.kit kit in
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
    (max_kit_deposited: kit)
    (min_lqt_minted: Ligo.nat)
    (deadline: Ligo.timestamp)
  : (Ligo.nat * kit * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in
  if !Ligo.Tezos.now >= deadline then
    (failwith "UniswapTooLate" : (Ligo.nat * kit * uniswap))
  else if tez_amount = Ligo.tez_from_literal "0mutez" then
    (failwith "AddLiquidityNoTezGiven" : (Ligo.nat * kit * uniswap))
  else if max_kit_deposited = kit_zero then
    (failwith "AddLiquidityNoKitGiven" : (Ligo.nat * kit * uniswap))
  else if min_lqt_minted = Ligo.nat_from_literal "0n" then
    (failwith "AddLiquidityNoLiquidityToBeAdded" : (Ligo.nat * kit * uniswap))
  else
    let effective_tez_balance = Ligo.add_tez_tez uniswap.tez pending_accrual in
    let lqt_minted =
      ratio_to_nat_floor
        (mul_ratio
           (ratio_of_nat uniswap.lqt)
           (make_ratio (tez_to_mutez tez_amount) (tez_to_mutez effective_tez_balance))
        ) in
    let kit_deposited =
      kit_of_ratio_ceil
        (mul_ratio
           (kit_to_ratio uniswap.kit)
           (make_ratio (tez_to_mutez tez_amount) (tez_to_mutez effective_tez_balance))
        ) in
    if lqt_minted < min_lqt_minted then
      (failwith "AddLiquidityTooLowLiquidityMinted" : (Ligo.nat * kit * uniswap))
    else if max_kit_deposited < kit_deposited then
      (failwith "AddLiquidityTooMuchKitRequired" : (Ligo.nat * kit * uniswap))
    else if kit_deposited = kit_zero then
      (failwith "AddLiquidityZeroKitDeposited" : (Ligo.nat * kit * uniswap))
    else
      let kit_deposited, kit_to_return = (kit_deposited, kit_sub max_kit_deposited kit_deposited) in
      let updated = { uniswap with
                      kit = kit_add uniswap.kit kit_deposited;
                      tez = Ligo.add_tez_tez uniswap.tez tez_amount;
                      lqt = Ligo.add_nat_nat uniswap.lqt lqt_minted;
                    } in
      (* EXPECTED PROPERTY: kit_to_return + final_uniswap_kit = max_kit_deposited + initial_uniswap_kit *)
      (lqt_minted, kit_to_return, updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
(* TODO: for the purpose of removing liquidity, the bid accrues only after the next period begins. *)
let uniswap_remove_liquidity
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (lqt_burned: Ligo.nat)
    (min_tez_withdrawn: Ligo.tez)
    (min_kit_withdrawn: kit)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * kit * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  if tez_amount <> Ligo.tez_from_literal "0mutez" then
    (failwith "RemoveLiquidityNonEmptyAmount" : (Ligo.tez * kit * uniswap))
  else if !Ligo.Tezos.now >= deadline then
    (failwith "UniswapTooLate" : (Ligo.tez * kit * uniswap))
  else if lqt_burned = Ligo.nat_from_literal "0n" then
    (failwith "RemoveLiquidityNoLiquidityBurned" : (Ligo.tez * kit * uniswap))
  else if min_tez_withdrawn = Ligo.tez_from_literal "0mutez" then
    (failwith "RemoveLiquidityNoTezWithdrawnExpected" : (Ligo.tez * kit * uniswap))
  else if min_kit_withdrawn = kit_zero then
    (failwith "RemoveLiquidityNoKitWithdrawnExpected" : (Ligo.tez * kit * uniswap))
    (* TODO: Check whether we have more edge cases to give a failure for. *)
  else
    let _ = assert (lqt_burned <= uniswap.lqt) in (* the ticket mechanism should enforce this *)
    let ratio = make_ratio (Ligo.int lqt_burned) (Ligo.int uniswap.lqt) in
    let tez_withdrawn = ratio_to_tez_floor (mul_ratio (ratio_of_tez uniswap.tez) ratio) in
    let kit_withdrawn = kit_of_ratio_floor (mul_ratio (kit_to_ratio uniswap.kit) ratio) in

    if tez_withdrawn < min_tez_withdrawn then
      (failwith "RemoveLiquidityCantWithdrawEnoughTez" : (Ligo.tez * kit * uniswap))
    else if tez_withdrawn > uniswap.tez then
      (failwith "RemoveLiquidityTooMuchTezWithdrawn" : (Ligo.tez * kit * uniswap))
    else if kit_withdrawn < min_kit_withdrawn then
      (failwith "RemoveLiquidityCantWithdrawEnoughKit" : (Ligo.tez * kit * uniswap))
    else if kit_withdrawn > uniswap.kit then
      (failwith "RemoveLiquidityTooMuchKitWithdrawn" : (Ligo.tez * kit * uniswap))
    else
      let remaining_lqt, _burned = (
        match Ligo.is_nat (Ligo.sub_nat_nat uniswap.lqt lqt_burned) with
        | None -> (failwith "uniswap_remove_liquidity: impossible" : (Ligo.nat * Ligo.nat))
        | Some remaining -> (remaining, lqt_burned)
      ) in

      let kit_withdrawn, remaining_kit = (kit_withdrawn, kit_sub uniswap.kit kit_withdrawn) in
      let updated = { uniswap with
                      tez = Ligo.sub_tez_tez uniswap.tez tez_withdrawn;
                      kit = remaining_kit;
                      lqt = remaining_lqt } in
      (tez_withdrawn, kit_withdrawn, updated)

let uniswap_add_accrued_kit (uniswap: uniswap) (accrual: kit) : uniswap =
  let uniswap = uniswap_sync_last_observed uniswap in
  { uniswap with kit = kit_add uniswap.kit accrual }

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
  div_ratio (ratio_of_tez u.tez) (kit_to_ratio u.kit)

let uniswap_kit_times_tez (u: uniswap) =
  mul_ratio (ratio_of_tez u.tez) (kit_to_ratio u.kit)

let uniswap_liquidity_tokens_extant (u: uniswap) = u.lqt
(* END_OCAML *)
