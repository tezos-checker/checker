(* The general concept of cfmm is that you have quantity a of an asset A
 * and b of an asset B and you process buy and sell requests by maintaining
 * the product a * b constant. So if someone wants to sell a quantity da of
 * asset A to the contract, the balance would become (a + da) so you can
 * give that person a quantity db of asset B in exchange such that (a +
 * da)(b - db) = a * b. Solving for db gives db  = da * b / (a + da). We
 * can rewrite this as db = da * (b / a) * (a / (a + da)) where (b / a)
 * represents the  "price" before the order and a / (a + da)  represents
 * the "slippage". Indeed, a property of cfmm is that with arbitrageurs
 * around, the ratio (a / b) gives you the market price of A in terms of B.
 *
 * On top of that, we can add some fees of 0.2 cNp. So the equation becomes
 * something like db = da * b / (a + da) * (1 - 0.2/100) (note that this
 * formula is a first-order approximation in the sense that two orders of size
 * da / 2 will give you a better price than one order of size da, but the
 * difference is far smaller than typical fees or any amount we care about.
*)
(* Check out dexter for technical details:
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/ligo/dexter.ligo
     https://gitlab.com/camlcase-dev/dexter/-/blob/master/docs/dexter-informal-specification.md
*)

open Ctez
open Kit
open Lqt
open Constants
open CfmmTypes
open Error
open Common

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

(** Compute the price of kit in ctez (ratio of ctez and kit in the cfmm
    contract), as it was at the end of the last block. This is to be used when
    required for the calculation of the drift derivative instead of up-to-date
    kit_in_ctez, because it is a little harder to manipulate. *)
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
          (Ligo.mul_nat_int (kit_to_denomination_nat cfmm.kit) ctez_scaling_factor_int);
      last_level = !Ligo.Tezos.level;
    }

(** Compute the maximum [min_kit_expected] for [cfmm_buy_kit] to succeed. *)
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
        (Ligo.mul_nat_int (kit_to_denomination_nat cfmm.kit) num_uf) in
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

(** Buy some kit from the cfmm contract by providing some ctez. Fail if the
    desired amount of kit cannot be bought or if the deadline has passed. *)
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

(** Compute the maximum [min_ctez_expected] for [cfmm_sell_kit] to succeed. *)
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
      Ligo.mul_nat_int
        (kit_to_denomination_nat kit_amount)
        (Ligo.mul_nat_int (ctez_to_muctez_nat cfmm.ctez) num_uf) in
    let denominator =
      Ligo.mul_int_int
        ctez_scaling_factor_int
        (Ligo.mul_nat_int (kit_to_denomination_nat new_cfmm_kit) den_uf) in
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

(** Sell some kit to the cfmm contract. Fail if the desired amount of ctez
    cannot be bought or if the deadline has passed. *)
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

(** Compute the minimum [max_kit_deposited] and the maximum [min_lqt_minted]
    for [cfmm_add_liquidity] to succeed. *)
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
        (Ligo.mul_int_nat (kit_to_denomination_int cfmm.kit) (ctez_to_muctez_nat ctez_amount))
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

(** Buy some liquidity from the cfmm contract, by giving it some ctez and
    some kit. If the given amounts does not have the right ratio, we
    liquidate all the ctez given and as much kit as we can with the right
    ratio, and return the leftovers, along with the liquidity tokens. *)
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

(** Compute the maximum [min_ctez_withdrawn] and the minimum
    [min_kit_withdrawn] for [cfmm_remove_liquidity] to succeed. *)
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
        (Ligo.mul_int_nat ctez_scaling_factor_int (lqt_to_denomination_nat cfmm.lqt))
    in
    let kit_withdrawn =
      kit_of_fraction_floor
        (Ligo.mul_int_nat (kit_to_denomination_int cfmm.kit) (lqt_to_denomination_nat lqt_burned))
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

(** Sell some liquidity to the cfmm contract. Selling liquidity always
    succeeds, but might leave the contract without ctez and kit if everybody
    sells their liquidity. I think it is unlikely to happen, since the last
    liquidity holders wouldn't want to lose the burrow fees. *)
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

(** Add accrued burrowing fees to the cfmm contract. *)
let cfmm_add_accrued_kit (cfmm: cfmm) (accrual: kit) : cfmm =
  let cfmm = cfmm_sync_last_observed cfmm in
  { cfmm with kit = kit_add cfmm.kit accrual }
