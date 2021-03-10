open Ratio
open Kit
open Common
open Constants
open UniswapTypes
open Error

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
      kit_in_tez_in_prev_block =
        make_real_unsafe
          (Ligo.mul_int_int (tez_to_mutez uniswap.tez) kit_scaling_factor_int)
          (Ligo.mul_int_int (kit_to_mukit_int uniswap.kit) (Ligo.int_from_literal "1_000_000"));
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
    (Ligo.failwith error_UniswapNonPositiveInput : (kit * uniswap))
  else if (!Ligo.Tezos.now >= deadline) then
    (Ligo.failwith error_UniswapTooLate : (kit * uniswap))
  else if (min_kit_expected = kit_zero) then
    (Ligo.failwith error_BuyKitTooLowExpectedKit : (kit * uniswap))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = uniswap_fee in
      { num = Ligo.sub_int_int den_uf num_uf; den = den_uf; } (* 1 - uniswap_fee *)
    in
    let new_uniswap_tez = Ligo.add_tez_tez uniswap.tez tez_amount in
    let numerator =
      Ligo.mul_int_int
        (tez_to_mutez tez_amount)
        (Ligo.mul_int_int (kit_to_mukit_int uniswap.kit) num_uf) in
    let denominator =
      Ligo.mul_int_int
        kit_scaling_factor_int
        (Ligo.mul_int_int (tez_to_mutez new_uniswap_tez) den_uf) in
    let bought_kit = kit_of_fraction_floor numerator denominator in

    if bought_kit < min_kit_expected then
      (Ligo.failwith error_BuyKitPriceFailure : (kit * uniswap))
    else if bought_kit > uniswap.kit then
      (Ligo.failwith error_BuyKitTooMuchKitBought : (kit * uniswap))
    else
      let remaining_kit = kit_sub uniswap.kit bought_kit in
      ( bought_kit,
        { uniswap with
          kit = remaining_kit;
          tez = new_uniswap_tez;
        }
      )

let uniswap_sell_kit
    (uniswap: uniswap)
    (tez_amount: Ligo.tez)
    (kit_amount: kit)
    (min_tez_expected: Ligo.tez)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * uniswap) =
  let uniswap = uniswap_sync_last_observed uniswap in
  let uniswap = uniswap_assert_initialized uniswap in (* DON'T DROP! *)
  if (kit_amount = kit_zero) then
    (Ligo.failwith error_UniswapNonPositiveInput : (Ligo.tez * uniswap))
  else if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_UniswapTooLate : (Ligo.tez * uniswap))
  else if tez_amount <> Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_SellKitNonEmptyAmount : (Ligo.tez * uniswap))
  else if (min_tez_expected = Ligo.tez_from_literal "0mutez") then
    (Ligo.failwith error_SellKitTooLowExpectedTez : (Ligo.tez * uniswap))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = uniswap_fee in
      { num = Ligo.sub_int_int den_uf num_uf; den = den_uf; } (* 1 - uniswap_fee *)
    in
    let new_uniswap_kit = kit_add uniswap.kit kit_amount in
    let numerator =
      Ligo.mul_int_int
        (kit_to_mukit_int kit_amount)
        (Ligo.mul_int_int (tez_to_mutez uniswap.tez) num_uf) in
    let denominator =
      Ligo.mul_int_int
        (Ligo.int_from_literal "1_000_000")
        (Ligo.mul_int_int (kit_to_mukit_int new_uniswap_kit) den_uf) in
    let bought_tez = fraction_to_tez_floor numerator denominator in

    if bought_tez < min_tez_expected then
      (Ligo.failwith error_SellKitPriceFailure : (Ligo.tez * uniswap))
    else if bought_tez > uniswap.tez then
      (Ligo.failwith error_SellKitTooMuchTezBought : (Ligo.tez * uniswap))
    else
      let remaining_tez = Ligo.sub_tez_tez uniswap.tez bought_tez in
      ( bought_tez,
        { uniswap with
          kit = new_uniswap_kit;
          tez = remaining_tez;
        }
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
    (Ligo.failwith error_UniswapTooLate : (Ligo.nat * kit * uniswap))
  else if tez_amount = Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_AddLiquidityNoTezGiven : (Ligo.nat * kit * uniswap))
  else if max_kit_deposited = kit_zero then
    (Ligo.failwith error_AddLiquidityNoKitGiven : (Ligo.nat * kit * uniswap))
  else if min_lqt_minted = Ligo.nat_from_literal "0n" then
    (Ligo.failwith error_AddLiquidityNoLiquidityToBeAdded : (Ligo.nat * kit * uniswap))
  else
    let effective_tez_balance = tez_to_mutez (Ligo.add_tez_tez uniswap.tez pending_accrual) in
    let lqt_minted =
      fraction_to_nat_floor
        (Ligo.mul_int_int (Ligo.int uniswap.lqt) (tez_to_mutez tez_amount))
        effective_tez_balance
    in
    let kit_deposited =
      kit_of_fraction_ceil
        (Ligo.mul_int_int (kit_to_mukit_int uniswap.kit) (tez_to_mutez tez_amount))
        (Ligo.mul_int_int kit_scaling_factor_int effective_tez_balance)
    in

    if lqt_minted < min_lqt_minted then
      (Ligo.failwith error_AddLiquidityTooLowLiquidityMinted : (Ligo.nat * kit * uniswap))
    else if max_kit_deposited < kit_deposited then
      (Ligo.failwith error_AddLiquidityTooMuchKitRequired : (Ligo.nat * kit * uniswap))
    else if kit_deposited = kit_zero then
      (Ligo.failwith error_AddLiquidityZeroKitDeposited : (Ligo.nat * kit * uniswap))
    else
      let kit_to_return = kit_sub max_kit_deposited kit_deposited in
      let updated = { uniswap with
                      kit = kit_add uniswap.kit kit_deposited;
                      tez = Ligo.add_tez_tez uniswap.tez tez_amount;
                      lqt = Ligo.add_nat_nat uniswap.lqt lqt_minted;
                    } in
      (* EXPECTED PROPERTY: kit_to_return + final_uniswap_kit = max_kit_deposited + initial_uniswap_kit
       * which follows from the definitions:
       *  kit_to_return     = max_kit_deposited   - kit_deposited
       *  final_uniswap_kit = initial_uniswap_kit + kit_deposited
       *)
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
    (Ligo.failwith error_RemoveLiquidityNonEmptyAmount : (Ligo.tez * kit * uniswap))
  else if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_UniswapTooLate : (Ligo.tez * kit * uniswap))
  else if lqt_burned = Ligo.nat_from_literal "0n" then
    (Ligo.failwith error_RemoveLiquidityNoLiquidityBurned : (Ligo.tez * kit * uniswap))
  else if min_tez_withdrawn = Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_RemoveLiquidityNoTezWithdrawnExpected : (Ligo.tez * kit * uniswap))
  else if min_kit_withdrawn = kit_zero then
    (Ligo.failwith error_RemoveLiquidityNoKitWithdrawnExpected : (Ligo.tez * kit * uniswap))
    (* TODO: Check whether we have more edge cases to give a failure for. *)
  else
    let _ = assert (lqt_burned <= uniswap.lqt) in (* the ticket mechanism should enforce this *)

    let tez_withdrawn =
      fraction_to_tez_floor
        (Ligo.mul_int_int (tez_to_mutez uniswap.tez) (Ligo.int lqt_burned))
        (Ligo.mul_int_int (Ligo.int_from_literal "1_000_000") (Ligo.int uniswap.lqt))
    in
    let kit_withdrawn =
      kit_of_fraction_floor
        (Ligo.mul_int_int (kit_to_mukit_int uniswap.kit) (Ligo.int lqt_burned))
        (Ligo.mul_int_int kit_scaling_factor_int (Ligo.int uniswap.lqt))
    in
    if tez_withdrawn < min_tez_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughTez : (Ligo.tez * kit * uniswap))
    else if tez_withdrawn > uniswap.tez then
      (Ligo.failwith error_RemoveLiquidityTooMuchTezWithdrawn : (Ligo.tez * kit * uniswap))
    else if kit_withdrawn < min_kit_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughKit : (Ligo.tez * kit * uniswap))
    else if kit_withdrawn > uniswap.kit then
      (Ligo.failwith error_RemoveLiquidityTooMuchKitWithdrawn : (Ligo.tez * kit * uniswap))
    else
      let remaining_lqt, _burned = (
        match Ligo.is_nat (Ligo.sub_nat_nat uniswap.lqt lqt_burned) with
        | None -> (failwith "uniswap_remove_liquidity: impossible" : (Ligo.nat * Ligo.nat))
        | Some remaining -> (remaining, lqt_burned)
      ) in

      let remaining_kit = kit_sub uniswap.kit kit_withdrawn in
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
