open Ratio
open Ctez
open Kit
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
  assert (not (u.ctez = ctez_zero));
  assert (not (u.kit = kit_zero));
  assert (not (u.lqt = Ligo.nat_from_literal "0n"));
  u

let cfmm_kit_in_ctez_in_prev_block (cfmm: cfmm) =
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  cfmm.kit_in_ctez_in_prev_block

(* Update the kit_in_ctez cached and last_level, if we just entered a new block.
 * This should be called before we many any changes to the contract so that we
 * don't lose the last kit_in_ctez at the end of the last block. George: Note
 * that this is not be the previous block, but the last block in which the
 * cfmm contract was touched. *)
let cfmm_sync_last_observed (cfmm: cfmm) : cfmm =
  assert (!Ligo.Tezos.level >= cfmm.last_level); (* TODO: can it be later?? *)
  if cfmm.last_level = !Ligo.Tezos.level then
    (* do nothing if it's been touched already in this block *)
    cfmm
  else
    { cfmm with
      kit_in_ctez_in_prev_block =
        make_real_unsafe
          (Ligo.mul_int_int (ctez_to_muctez_int cfmm.ctez) kit_scaling_factor_int)
          (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (Ligo.int_from_literal "1_000_000"));
      last_level = !Ligo.Tezos.level;
    }

let cfmm_view_min_kit_expected_buy_kit
    (cfmm: cfmm)
    (ctez_amount: ctez)
  : (kit (* min_kit_expected *) * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  if (ctez_amount = ctez_zero) then
    (Ligo.failwith error_BuyKitNoTezGiven : (kit * cfmm))
  else
    (* db = da * (b / a) * (a / (a + da)) * (1 - fee) or
     * db = da * b / (a + da) * (1 - fee) *)
    let { num = num_uf; den = den_uf; } =
      let { num = num_uf; den = den_uf; } = cfmm_fee in
      { num = Ligo.sub_int_int den_uf num_uf; den = den_uf; } (* 1 - cfmm_fee *)
    in
    let new_cfmm_ctez = ctez_add cfmm.ctez ctez_amount in
    let numerator =
      Ligo.mul_int_int
        (ctez_to_muctez_int ctez_amount)
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) num_uf) in
    let denominator =
      Ligo.mul_int_int
        kit_scaling_factor_int
        (Ligo.mul_int_int (ctez_to_muctez_int new_cfmm_ctez) den_uf) in
    let bought_kit = kit_of_fraction_floor numerator denominator in
    if bought_kit > cfmm.kit then
      (Ligo.failwith error_BuyKitTooMuchKitBought : (kit * cfmm))
    else
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
  if (ctez_amount = ctez_zero) then
    (Ligo.failwith error_BuyKitNoTezGiven : (kit * cfmm))
  else if (!Ligo.Tezos.now >= deadline) then
    (Ligo.failwith error_CfmmTooLate : (kit * cfmm))
  else if (min_kit_expected = kit_zero) then
    (Ligo.failwith error_BuyKitTooLowExpectedKit : (kit * cfmm))
  else
    let (bought_kit, cfmm) = cfmm_view_min_kit_expected_buy_kit cfmm ctez_amount in
    if bought_kit < min_kit_expected then
      (Ligo.failwith error_BuyKitPriceFailure : (kit * cfmm))
    else
      (bought_kit, cfmm)

let cfmm_view_min_ctez_expected_cfmm_sell_kit
    (cfmm: cfmm)
    (kit_amount: kit)
  : (ctez * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  if (kit_amount = kit_zero) then
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
        (Ligo.mul_int_int (ctez_to_muctez_int cfmm.ctez) num_uf) in
    let denominator =
      Ligo.mul_int_int
        (Ligo.int_from_literal "1_000_000")
        (Ligo.mul_int_int (kit_to_mukit_int new_cfmm_kit) den_uf) in
    let bought_ctez = ctez_of_fraction_floor numerator denominator in
    if bought_ctez > cfmm.ctez then
      (Ligo.failwith error_SellKitTooMuchTezBought : (ctez * cfmm))
    else
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
  if (kit_amount = kit_zero) then
    (Ligo.failwith error_SellKitNoKitGiven : (ctez * cfmm))
  else if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_CfmmTooLate : (ctez * cfmm))
  else if (min_ctez_expected = ctez_zero) then
    (Ligo.failwith error_SellKitTooLowExpectedTez : (ctez * cfmm))
  else
    let (bought_ctez, cfmm) = cfmm_view_min_ctez_expected_cfmm_sell_kit cfmm kit_amount in
    if bought_ctez < min_ctez_expected then
      (Ligo.failwith error_SellKitPriceFailure : (ctez * cfmm))
    else
      (bought_ctez, cfmm)

let cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity
    (cfmm: cfmm)
    (ctez_amount: ctez)
  : (liquidity * kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in
  if ctez_amount = ctez_zero then
    (Ligo.failwith error_AddLiquidityNoTezGiven : (Ligo.nat * kit * cfmm))
  else
    let cfmm_ctez = ctez_to_muctez_int cfmm.ctez in
    let lqt_minted =
      fraction_to_nat_floor
        (Ligo.mul_int_int (Ligo.int cfmm.lqt) (ctez_to_muctez_int ctez_amount))
        cfmm_ctez in
    let kit_deposited =
      kit_of_fraction_ceil
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (ctez_to_muctez_int ctez_amount))
        (Ligo.mul_int_int kit_scaling_factor_int cfmm_ctez) in
    ( lqt_minted,
      kit_deposited,
      { cfmm with
        kit = kit_add cfmm.kit kit_deposited;
        ctez = ctez_add cfmm.ctez ctez_amount;
        lqt = Ligo.add_nat_nat cfmm.lqt lqt_minted;
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
    (min_lqt_minted: liquidity)
    (deadline: Ligo.timestamp)
  : (liquidity * kit * cfmm) =
  if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_CfmmTooLate : (Ligo.nat * kit * cfmm))
  else if ctez_amount = ctez_zero then
    (Ligo.failwith error_AddLiquidityNoTezGiven : (Ligo.nat * kit * cfmm))
  else if max_kit_deposited = kit_zero then
    (Ligo.failwith error_AddLiquidityNoKitGiven : (Ligo.nat * kit * cfmm))
  else if min_lqt_minted = Ligo.nat_from_literal "0n" then
    (Ligo.failwith error_AddLiquidityNoLiquidityToBeAdded : (Ligo.nat * kit * cfmm))
  else
    let (lqt_minted, kit_deposited, cfmm) =
      cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity cfmm ctez_amount in
    if lqt_minted < min_lqt_minted then
      (Ligo.failwith error_AddLiquidityTooLowLiquidityMinted : (Ligo.nat * kit * cfmm))
    else if max_kit_deposited < kit_deposited then
      (Ligo.failwith error_AddLiquidityTooMuchKitRequired : (Ligo.nat * kit * cfmm))
    else if kit_deposited = kit_zero then
      (Ligo.failwith error_AddLiquidityZeroKitDeposited : (Ligo.nat * kit * cfmm))
    else
      let kit_to_return = kit_sub max_kit_deposited kit_deposited in
      (* EXPECTED PROPERTY: kit_to_return + final_cfmm_kit = max_kit_deposited + initial_cfmm_kit
       * which follows from the definitions:
       *  kit_to_return  = max_kit_deposited - kit_deposited
       *  final_cfmm_kit = initial_cfmm_kit  + kit_deposited
      *)
      (lqt_minted, kit_to_return, cfmm)

(* Selling liquidity always succeeds, but might leave the contract
 * without ctez and kit if everybody sells their liquidity. I think
 * it is unlikely to happen, since the last liquidity holders wouldn't
 * want to lose the burrow fees. *)
let cfmm_remove_liquidity
    (cfmm: cfmm)
    (lqt_burned: liquidity)
    (min_ctez_withdrawn: ctez)
    (min_kit_withdrawn: kit)
    (deadline: Ligo.timestamp)
  : (ctez * kit * cfmm) =
  let cfmm = cfmm_sync_last_observed cfmm in
  let cfmm = cfmm_assert_initialized cfmm in (* DON'T DROP! *)
  if !Ligo.Tezos.now >= deadline then
    (Ligo.failwith error_CfmmTooLate : (ctez * kit * cfmm))
  else if lqt_burned = Ligo.nat_from_literal "0n" then
    (Ligo.failwith error_RemoveLiquidityNoLiquidityBurned : (ctez * kit * cfmm))
  else if min_ctez_withdrawn = ctez_zero then
    (Ligo.failwith error_RemoveLiquidityNoTezWithdrawnExpected : (ctez * kit * cfmm))
  else if min_kit_withdrawn = kit_zero then
    (Ligo.failwith error_RemoveLiquidityNoKitWithdrawnExpected : (ctez * kit * cfmm))
  else
    let _ = assert (lqt_burned <= cfmm.lqt) in (* the ticket mechanism should enforce this *)

    let ctez_withdrawn =
      ctez_of_fraction_floor
        (Ligo.mul_int_int (ctez_to_muctez_int cfmm.ctez) (Ligo.int lqt_burned))
        (Ligo.mul_int_int (Ligo.int_from_literal "1_000_000") (Ligo.int cfmm.lqt))
    in
    let kit_withdrawn =
      kit_of_fraction_floor
        (Ligo.mul_int_int (kit_to_mukit_int cfmm.kit) (Ligo.int lqt_burned))
        (Ligo.mul_int_int kit_scaling_factor_int (Ligo.int cfmm.lqt))
    in
    if ctez_withdrawn < min_ctez_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughTez : (ctez * kit * cfmm))
    else if ctez_withdrawn > cfmm.ctez then
      (Ligo.failwith error_RemoveLiquidityTooMuchTezWithdrawn : (ctez * kit * cfmm))
    else if kit_withdrawn < min_kit_withdrawn then
      (Ligo.failwith error_RemoveLiquidityCantWithdrawEnoughKit : (ctez * kit * cfmm))
    else if kit_withdrawn > cfmm.kit then
      (Ligo.failwith error_RemoveLiquidityTooMuchKitWithdrawn : (ctez * kit * cfmm))
    else
      let remaining_lqt, _burned = (
        match Ligo.is_nat (Ligo.sub_nat_nat cfmm.lqt lqt_burned) with
        | None -> (failwith "cfmm_remove_liquidity: impossible" : (Ligo.nat * Ligo.nat))
        | Some remaining -> (remaining, lqt_burned)
      ) in

      let remaining_kit = kit_sub cfmm.kit kit_withdrawn in
      let updated = { cfmm with
                      ctez = ctez_sub cfmm.ctez ctez_withdrawn;
                      kit = remaining_kit;
                      lqt = remaining_lqt } in
      (ctez_withdrawn, kit_withdrawn, updated)

let cfmm_add_accrued_kit (cfmm: cfmm) (accrual: kit) : cfmm =
  let cfmm = cfmm_sync_last_observed cfmm in
  { cfmm with kit = kit_add cfmm.kit accrual }
