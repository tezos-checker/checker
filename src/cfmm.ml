open Ratio
open Ctez
open Kit
open Lqt
open Constants
open CfmmTypes
open Error

(* When the cfmm is uninitialized, we should not be able to query prices
 * and/or do other things. George: I assume that the only thing we should allow
 * is adding liquidity, to kick things off. I would also like to assume that
 * all the conditions below should be either all true or all false, but the
 * implementation of remove_liquidity currently allows liquidity to reach zero.
 * *)
let[@inline] cfmm_assert_initialized (u: cfmm) : cfmm =
  assert (not (u.ctez = ctez_zero));
  assert (not (u.kit = kit_zero));
  assert (not (u.lqt = lqt_zero));
  u

let cfmm_kit_in_ctez_in_prev_block (cfmm: cfmm) =
  let cfmm = cfmm_assert_initialized cfmm in
  cfmm.kit_in_ctez_in_prev_block

(* Update the kit_in_ctez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_ctez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * cfmm contract was touched. *)
let cfmm_sync_last_observed (cfmm: cfmm) : cfmm =
  assert (Ligo.geq_nat_nat !Ligo.Tezos.level cfmm.last_level);
  if Ligo.leq_nat_nat !Ligo.Tezos.level cfmm.last_level then
    (* do nothing if it's been touched already in this block *)
    cfmm
  else
    { cfmm with
      kit_in_ctez_in_prev_block =
        make_ratio
          (Ligo.mul_nat_int (ctez_to_muctez_nat cfmm.ctez) kit_scaling_factor_int)
          (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (Ligo.int_from_literal "1_000_000"));
      last_level = !Ligo.Tezos.level;
    }

let cfmm_view_min_kit_expected_buy_kit
    (cfmm: cfmm)
    (ctez_amount: ctez)
  : (kit (* min_kit_expected *) * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if (eq_ctez_ctez ctez_amount ctez_zero) then
    (Ligo.failwith error_BuyKitNoCtezGiven : (kit * cfmm))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = cfmm_fee in
      { num = Ligo.sub_int_int den_uf num_uf; den = den_uf; } (* 1 - cfmm_fee *)
    in
    let new_cfmm_ctez = ctez_add cfmm.ctez ctez_amount in
    let numerator =
      Ligo.mul_nat_int
        (ctez_to_muctez_nat ctez_amount)
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) num_uf) in
    let denominator =
      Ligo.mul_int_int
        kit_scaling_factor_int
        (Ligo.mul_nat_int (ctez_to_muctez_nat new_cfmm_ctez) den_uf) in
    let bought_kit = kit_of_fraction_floor numerator denominator in
    (* Due to (a) the constant-factor calculation (which means that to deplete
     * the one amount the other would in effect have to become infinite), (b)
     * the fact that checker owns 1mu of each token, and (c) the fact that we
     * always floor in our calculations, it should be impossible to trigger the
     * following assertion. *)
    assert (lt_kit_kit bought_kit cfmm.kit);
    ( bought_kit,
      { cfmm with
        kit = kit_sub cfmm.kit bought_kit;
        ctez = new_cfmm_ctez;
      }
    )

let cfmm_buy_kit
    (cfmm: cfmm)
    (ctez_amount: ctez)
    (min_kit_expected: kit)
    (deadline: Ligo.timestamp)
  : (kit * cfmm) =
  if (eq_ctez_ctez ctez_amount ctez_zero) then
    (Ligo.failwith error_BuyKitNoCtezGiven : (kit * cfmm))
  else if (Ligo.geq_timestamp_timestamp !Ligo.Tezos.now deadline) then
    (Ligo.failwith error_CfmmTooLate : (kit * cfmm))
  else if (eq_kit_kit min_kit_expected kit_zero) then
    (Ligo.failwith error_BuyKitTooLowExpectedKit : (kit * cfmm))
  else
    let (bought_kit, cfmm) = cfmm_view_min_kit_expected_buy_kit cfmm ctez_amount in
    if lt_kit_kit bought_kit min_kit_expected then
      (Ligo.failwith error_BuyKitPriceFailure : (kit * cfmm))
    else
      (bought_kit, cfmm)

let cfmm_view_min_ctez_expected_cfmm_sell_kit
    (cfmm: cfmm)
    (kit_amount: kit)
  : (ctez * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if (eq_kit_kit kit_amount kit_zero) then
    (Ligo.failwith error_SellKitNoKitGiven : (ctez * cfmm))
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
        (Ligo.mul_nat_int (ctez_to_muctez_nat cfmm.ctez) num_uf) in
    let denominator =
      Ligo.mul_int_int
        (Ligo.int_from_literal "1_000_000")
        (Ligo.mul_int_int (kit_to_mukit_int new_cfmm_kit) den_uf) in
    let bought_ctez = ctez_of_fraction_floor numerator denominator in

    (* Due to (a) the constant-factor calculation (which means that to deplete
     * the one amount the other would in effect have to become infinite), (b)
     * the fact that checker owns 1mu of each token, and (c) the fact that we
     * always floor in our calculations, it should be impossible to trigger the
     * following assertion. *)
    assert (lt_ctez_ctez bought_ctez cfmm.ctez);
    ( bought_ctez,
      { cfmm with
        kit = new_cfmm_kit;
        ctez = ctez_sub cfmm.ctez bought_ctez;
      }
    )

let cfmm_sell_kit
    (cfmm: cfmm)
    (kit_amount: kit)
    (min_ctez_expected: ctez)
    (deadline: Ligo.timestamp)
  : (ctez * cfmm) =
  if (eq_kit_kit kit_amount kit_zero) then
    (Ligo.failwith error_SellKitNoKitGiven : (ctez * cfmm))
  else if Ligo.geq_timestamp_timestamp !Ligo.Tezos.now deadline then
    (Ligo.failwith error_CfmmTooLate : (ctez * cfmm))
  else if (eq_ctez_ctez min_ctez_expected ctez_zero) then
    (Ligo.failwith error_SellKitTooLowExpectedCtez : (ctez * cfmm))
  else
    let (bought_ctez, cfmm) = cfmm_view_min_ctez_expected_cfmm_sell_kit cfmm kit_amount in
    if lt_ctez_ctez bought_ctez min_ctez_expected then
      (Ligo.failwith error_SellKitPriceFailure : (ctez * cfmm))
    else
      (bought_ctez, cfmm)

let cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity
    (cfmm: cfmm)
    (ctez_amount: ctez)
  : (lqt * kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if eq_ctez_ctez ctez_amount ctez_zero then
    (Ligo.failwith error_AddLiquidityNoCtezGiven : (lqt * kit * cfmm))
  else
    let cfmm_ctez = ctez_to_muctez_nat cfmm.ctez in
    let lqt_minted =
      lqt_of_fraction_floor
        (Ligo.mul_int_nat (lqt_to_denomination_int cfmm.lqt) (ctez_to_muctez_nat ctez_amount))
        (Ligo.mul_int_nat lqt_scaling_factor_int cfmm_ctez) in
    let kit_deposited =
      kit_of_fraction_ceil
        (Ligo.mul_int_nat (kit_to_mukit_int cfmm.kit) (ctez_to_muctez_nat ctez_amount))
        (Ligo.mul_int_nat kit_scaling_factor_int cfmm_ctez) in
    (* Since (a) ctez_amount > 0, (b) cfmm.kit > 0, and (c) we ceil when
     * computing kit_deposited, it should be impossible to trigger the
     * following assertion. *)
    assert (gt_kit_kit kit_deposited kit_zero);
    ( lqt_minted,
      kit_deposited,
      { cfmm with
        kit = kit_add cfmm.kit kit_deposited;
        ctez = ctez_add cfmm.ctez ctez_amount;
        lqt = lqt_add cfmm.lqt lqt_minted;
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
 * to do it in checker is that the kit balance of the cfmm contract is
 * continuously credited with the burrow fee taken from burrow holders. *)
let cfmm_add_liquidity
    (cfmm: cfmm)
    (ctez_amount: ctez)
    (max_kit_deposited: kit)
    (min_lqt_minted: lqt)
    (deadline: Ligo.timestamp)
  : (lqt * kit * cfmm) =
  if Ligo.geq_timestamp_timestamp !Ligo.Tezos.now deadline then
    (Ligo.failwith error_CfmmTooLate : (lqt * kit * cfmm))
  else if eq_ctez_ctez ctez_amount ctez_zero then
    (Ligo.failwith error_AddLiquidityNoCtezGiven : (lqt * kit * cfmm))
  else if eq_kit_kit max_kit_deposited kit_zero then
    (Ligo.failwith error_AddLiquidityNoKitGiven : (lqt * kit * cfmm))
  else if eq_lqt_lqt min_lqt_minted lqt_zero then
    (Ligo.failwith error_AddLiquidityNoLiquidityToBeAdded : (lqt * kit * cfmm))
  else
    let (lqt_minted, kit_deposited, cfmm) =
      cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity cfmm ctez_amount in
    if lt_lqt_lqt lqt_minted min_lqt_minted then
      (Ligo.failwith error_AddLiquidityTooLowLiquidityMinted : (lqt * kit * cfmm))
    else if lt_kit_kit max_kit_deposited kit_deposited then
      (Ligo.failwith error_AddLiquidityTooMuchKitRequired : (lqt * kit * cfmm))
    else
      let kit_to_return = kit_sub max_kit_deposited kit_deposited in
      (* EXPECTED PROPERTY: kit_to_return + final_cfmm_kit = max_kit_deposited + initial_cfmm_kit
       * which follows from the definitions:
       *  kit_to_return  = max_kit_deposited - kit_deposited
       *  final_cfmm_kit = initial_cfmm_kit  + kit_deposited
      *)
      (lqt_minted, kit_to_return, cfmm)

let cfmm_view_min_ctez_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity
    (cfmm: cfmm)
    (lqt_burned: lqt)
  : (ctez * kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if eq_lqt_lqt lqt_burned lqt_zero then
    (Ligo.failwith error_RemoveLiquidityNoLiquidityBurned : (ctez * kit * cfmm))
  else if geq_lqt_lqt lqt_burned cfmm.lqt then
    (Ligo.failwith error_RemoveLiquidityTooMuchLiquidityWithdrawn : (ctez * kit * cfmm))
  else
    let ctez_withdrawn =
      ctez_of_fraction_floor
        (Ligo.mul_nat_int (ctez_to_muctez_nat cfmm.ctez) (lqt_to_denomination_int lqt_burned))
        (Ligo.mul_int_nat (Ligo.int_from_literal "1_000_000") (lqt_to_denomination_nat cfmm.lqt))
    in
    let kit_withdrawn =
      kit_of_fraction_floor
        (Ligo.mul_int_nat (kit_to_mukit_int cfmm.kit) (lqt_to_denomination_nat lqt_burned))
        (Ligo.mul_int_nat kit_scaling_factor_int (lqt_to_denomination_nat cfmm.lqt))
    in
    (* Since (a) 0 < lqt_burned < cfmm.lqt, and (b) we floor for both the kit
     * and the ctez withdrawn, it should be impossible to trigger the following
     * assertions. *)
    assert (lt_ctez_ctez ctez_withdrawn cfmm.ctez);
    assert (lt_kit_kit kit_withdrawn cfmm.kit);

    let remaining_ctez = ctez_sub cfmm.ctez ctez_withdrawn in
    let remaining_lqt = lqt_sub cfmm.lqt lqt_burned in
    let remaining_kit = kit_sub cfmm.kit kit_withdrawn in
    let updated = { cfmm with
                    ctez = remaining_ctez;
                    kit = remaining_kit;
                    lqt = remaining_lqt } in
    (ctez_withdrawn, kit_withdrawn, updated)

(* Selling liquidity always succeeds, but might leave the contract
 * without ctez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
let cfmm_remove_liquidity
    (cfmm: cfmm)
    (lqt_burned: lqt)
    (min_ctez_withdrawn: ctez)
    (min_kit_withdrawn: kit)
    (deadline: Ligo.timestamp)
  : (ctez * kit * cfmm) =
  if Ligo.geq_timestamp_timestamp !Ligo.Tezos.now deadline then
    (Ligo.failwith error_CfmmTooLate : (ctez * kit * cfmm))
  else if eq_lqt_lqt lqt_burned lqt_zero then
    (Ligo.failwith error_RemoveLiquidityNoLiquidityBurned : (ctez * kit * cfmm))
  else if geq_lqt_lqt lqt_burned cfmm.lqt then
    (Ligo.failwith error_RemoveLiquidityTooMuchLiquidityWithdrawn : (ctez * kit * cfmm))
  else if eq_ctez_ctez min_ctez_withdrawn ctez_zero then
    (Ligo.failwith error_RemoveLiquidityNoCtezWithdrawnExpected : (ctez * kit * cfmm))
  else if eq_kit_kit min_kit_withdrawn kit_zero then
    (Ligo.failwith error_RemoveLiquidityNoKitWithdrawnExpected : (ctez * kit * cfmm))
  else
    let (ctez_withdrawn, kit_withdrawn, cfmm) =
      cfmm_view_min_ctez_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity cfmm lqt_burned in
    if lt_ctez_ctez ctez_withdrawn min_ctez_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughCtez : (ctez * kit * cfmm))
    else if lt_kit_kit kit_withdrawn min_kit_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughKit : (ctez * kit * cfmm))
    else
      (ctez_withdrawn, kit_withdrawn, cfmm)

let cfmm_add_accrued_kit (cfmm: cfmm) (accrual: kit) : cfmm =
  let cfmm = cfmm_sync_last_observed cfmm in
  { cfmm with kit = kit_add cfmm.kit accrual }
