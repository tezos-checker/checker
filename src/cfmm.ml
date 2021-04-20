open Ratio
open Kit
open Common
open Constants
open CfmmTypes
open Error

(* When the cfmm is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let cfmm_assert_initialized (u: cfmm) : cfmm =
  assert (not (u.tez = Ligo.tez_from_literal "0mutez"));
  assert (not (u.kit = kit_zero));
  assert (not (u.lqt = Ligo.nat_from_literal "0n"));
  u

let cfmm_kit_in_tez_in_prev_block (cfmm: cfmm) =
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  cfmm.kit_in_tez_in_prev_block

(* Update the kit_in_tez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_tez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * cfmm contract was touched. *)
let cfmm_sync_last_observed (cfmm: cfmm) : cfmm =
  assert (!Ligo.Tezos.level >= cfmm.last_level); (* TODO: can it be later?? *)
  if cfmm.last_level = !Ligo.Tezos.level then
    (* do nothing if it's been touched already in this block *)
    cfmm
  else
    { cfmm with
      kit_in_tez_in_prev_block =
        make_real_unsafe
          (Ligo.mul_int_int (tez_to_mutez cfmm.tez) kit_scaling_factor_int)
          (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (Ligo.int_from_literal "1_000_000"));
      last_level = !Ligo.Tezos.level;
    }

let cfmm_buy_kit
    (cfmm: cfmm)
    (tez_amount: Ligo.tez)
    (min_kit_expected: kit)
    (deadline: Ligo.timestamp)
  : (kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  if (tez_amount = Ligo.tez_from_literal "0mutez") then
    (Ligo.failwith error_BuyKitNoTezGiven : (kit * cfmm))
  else if (!Ligo.Tezos.now >= deadline) then
    (Ligo.failwith error_CfmmTooLate : (kit * cfmm))
  else if (min_kit_expected = kit_zero) then
    (Ligo.failwith error_BuyKitTooLowExpectedKit : (kit * cfmm))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = cfmm_fee in
      { num = Ligo.sub_int_int den_uf num_uf; den = den_uf; } (* 1 - cfmm_fee *)
    in
    let new_cfmm_tez = Ligo.add_tez_tez cfmm.tez tez_amount in
    let numerator =
      Ligo.mul_int_int
        (tez_to_mutez tez_amount)
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) num_uf) in
    let denominator =
      Ligo.mul_int_int
        kit_scaling_factor_int
        (Ligo.mul_int_int (tez_to_mutez new_cfmm_tez) den_uf) in
    let bought_kit = kit_of_fraction_floor numerator denominator in

    if bought_kit < min_kit_expected then
      (Ligo.failwith error_BuyKitPriceFailure : (kit * cfmm))
    else if bought_kit > cfmm.kit then
      (Ligo.failwith error_BuyKitTooMuchKitBought : (kit * cfmm))
    else
      let remaining_kit = kit_sub cfmm.kit bought_kit in
      ( bought_kit,
        { cfmm with
          kit = remaining_kit;
          tez = new_cfmm_tez;
        }
      )

let cfmm_sell_kit
    (cfmm: cfmm)
    (tez_amount: Ligo.tez)
    (kit_amount: kit)
    (min_tez_expected: Ligo.tez)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  if (kit_amount = kit_zero) then
    (Ligo.failwith error_SellKitNoKitGiven : (Ligo.tez * cfmm))
  else if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_CfmmTooLate : (Ligo.tez * cfmm))
  else if tez_amount <> Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_SellKitNonEmptyAmount : (Ligo.tez * cfmm))
  else if (min_tez_expected = Ligo.tez_from_literal "0mutez") then
    (Ligo.failwith error_SellKitTooLowExpectedTez : (Ligo.tez * cfmm))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = cfmm_fee in
      { num = Ligo.sub_int_int den_uf num_uf; den = den_uf; } (* 1 - cfmm_fee *)
    in
    let new_cfmm_kit = kit_add cfmm.kit kit_amount in
    let numerator =
      Ligo.mul_int_int
        (kit_to_mukit_int kit_amount)
        (Ligo.mul_int_int (tez_to_mutez cfmm.tez) num_uf) in
    let denominator =
      Ligo.mul_int_int
        (Ligo.int_from_literal "1_000_000")
        (Ligo.mul_int_int (kit_to_mukit_int new_cfmm_kit) den_uf) in
    let bought_tez = fraction_to_tez_floor numerator denominator in

    if bought_tez < min_tez_expected then
      (Ligo.failwith error_SellKitPriceFailure : (Ligo.tez * cfmm))
    else if bought_tez > cfmm.tez then
      (Ligo.failwith error_SellKitTooMuchTezBought : (Ligo.tez * cfmm))
    else
      let remaining_tez = Ligo.sub_tez_tez cfmm.tez bought_tez in
      ( bought_tez,
        { cfmm with
          kit = new_cfmm_kit;
          tez = remaining_tez;
        }
      )

(* But where do the assets in cfmm come from? Liquidity providers, or
 * "LP" deposit can deposit a quantity la and lb of assets A and B in the
 * same proportion as the contract la / lb = a / b . Assuming there are n
 * "liquidity tokens" extant, they receive m = floor(n la / a) tokens and
 * there are now m +n liquidity tokens extant. They can redeem then at
 * anytime for a fraction of the assets A and B. The reason to do this in
 * cfmm is that usage of cfmm costs 0.3%, and that ultimately can
 * grow the balance of the assets in the contract. An additional reason
 * to do it in huxian is that the kit balance of the cfmm contract is
 * continuously credited with the burrow fee taken from burrow holders. *)
let cfmm_add_liquidity
    (cfmm: cfmm)
    (tez_amount: Ligo.tez)
    (pending_accrual: Ligo.tez)
    (max_kit_deposited: kit)
    (min_lqt_minted: Ligo.nat)
    (deadline: Ligo.timestamp)
  : (Ligo.nat * kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_CfmmTooLate : (Ligo.nat * kit * cfmm))
  else if tez_amount = Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_AddLiquidityNoTezGiven : (Ligo.nat * kit * cfmm))
  else if max_kit_deposited = kit_zero then
    (Ligo.failwith error_AddLiquidityNoKitGiven : (Ligo.nat * kit * cfmm))
  else if min_lqt_minted = Ligo.nat_from_literal "0n" then
    (Ligo.failwith error_AddLiquidityNoLiquidityToBeAdded : (Ligo.nat * kit * cfmm))
  else
    let effective_tez_balance = tez_to_mutez (Ligo.add_tez_tez cfmm.tez pending_accrual) in
    let lqt_minted =
      fraction_to_nat_floor
        (Ligo.mul_int_int (Ligo.int cfmm.lqt) (tez_to_mutez tez_amount))
        effective_tez_balance
    in
    let kit_deposited =
      kit_of_fraction_ceil
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (tez_to_mutez tez_amount))
        (Ligo.mul_int_int kit_scaling_factor_int effective_tez_balance)
    in

    if lqt_minted < min_lqt_minted then
      (Ligo.failwith error_AddLiquidityTooLowLiquidityMinted : (Ligo.nat * kit * cfmm))
    else if max_kit_deposited < kit_deposited then
      (Ligo.failwith error_AddLiquidityTooMuchKitRequired : (Ligo.nat * kit * cfmm))
    else if kit_deposited = kit_zero then
      (Ligo.failwith error_AddLiquidityZeroKitDeposited : (Ligo.nat * kit * cfmm))
    else
      let kit_to_return = kit_sub max_kit_deposited kit_deposited in
      let updated = { cfmm with
                      kit = kit_add cfmm.kit kit_deposited;
                      tez = Ligo.add_tez_tez cfmm.tez tez_amount;
                      lqt = Ligo.add_nat_nat cfmm.lqt lqt_minted;
                    } in
      (* EXPECTED PROPERTY: kit_to_return + final_cfmm_kit = max_kit_deposited + initial_cfmm_kit
       * which follows from the definitions:
       *  kit_to_return     = max_kit_deposited   - kit_deposited
       *  final_cfmm_kit = initial_cfmm_kit + kit_deposited
      *)
      (lqt_minted, kit_to_return, updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without tez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
(* NOTE: for the purpose of removing liquidity, the bid accrues only after the next period begins. *)
let cfmm_remove_liquidity
    (cfmm: cfmm)
    (tez_amount: Ligo.tez)
    (lqt_burned: Ligo.nat)
    (min_tez_withdrawn: Ligo.tez)
    (min_kit_withdrawn: kit)
    (deadline: Ligo.timestamp)
  : (Ligo.tez * kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  if tez_amount <> Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_RemoveLiquidityNonEmptyAmount : (Ligo.tez * kit * cfmm))
  else if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_CfmmTooLate : (Ligo.tez * kit * cfmm))
  else if lqt_burned = Ligo.nat_from_literal "0n" then
    (Ligo.failwith error_RemoveLiquidityNoLiquidityBurned : (Ligo.tez * kit * cfmm))
  else if min_tez_withdrawn = Ligo.tez_from_literal "0mutez" then
    (Ligo.failwith error_RemoveLiquidityNoTezWithdrawnExpected : (Ligo.tez * kit * cfmm))
  else if min_kit_withdrawn = kit_zero then
    (Ligo.failwith error_RemoveLiquidityNoKitWithdrawnExpected : (Ligo.tez * kit * cfmm))
  else
    let _ = assert (lqt_burned <= cfmm.lqt) in (* the ticket mechanism should enforce this *)

    let tez_withdrawn =
      fraction_to_tez_floor
        (Ligo.mul_int_int (tez_to_mutez cfmm.tez) (Ligo.int lqt_burned))
        (Ligo.mul_int_int (Ligo.int_from_literal "1_000_000") (Ligo.int cfmm.lqt))
    in
    let kit_withdrawn =
      kit_of_fraction_floor
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (Ligo.int lqt_burned))
        (Ligo.mul_int_int kit_scaling_factor_int (Ligo.int cfmm.lqt))
    in
    if tez_withdrawn < min_tez_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughTez : (Ligo.tez * kit * cfmm))
    else if tez_withdrawn > cfmm.tez then
      (Ligo.failwith error_RemoveLiquidityTooMuchTezWithdrawn : (Ligo.tez * kit * cfmm))
    else if kit_withdrawn < min_kit_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughKit : (Ligo.tez * kit * cfmm))
    else if kit_withdrawn > cfmm.kit then
      (Ligo.failwith error_RemoveLiquidityTooMuchKitWithdrawn : (Ligo.tez * kit * cfmm))
    else
      let remaining_lqt, _burned = (
        match Ligo.is_nat (Ligo.sub_nat_nat cfmm.lqt lqt_burned) with
        | None -> (failwith "cfmm_remove_liquidity: impossible" : (Ligo.nat * Ligo.nat))
        | Some remaining -> (remaining, lqt_burned)
      ) in

      let remaining_kit = kit_sub cfmm.kit kit_withdrawn in
      let updated = { cfmm with
                      tez = Ligo.sub_tez_tez cfmm.tez tez_withdrawn;
                      kit = remaining_kit;
                      lqt = remaining_lqt } in
      (tez_withdrawn, kit_withdrawn, updated)

let cfmm_add_accrued_kit (cfmm: cfmm) (accrual: kit) : cfmm =
  let cfmm = cfmm_sync_last_observed cfmm in
  { cfmm with kit = kit_add cfmm.kit accrual }

let cfmm_add_accrued_tez (cfmm: cfmm) (accrual: Ligo.tez) : cfmm =
  let cfmm = cfmm_sync_last_observed cfmm in
  { cfmm with tez = Ligo.add_tez_tez cfmm.tez accrual }
