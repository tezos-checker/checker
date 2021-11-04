open OUnit2
open TestLib
open Ratio
open Ctok
open Kit
open Lqt
open Cfmm
open CfmmTypes
open Error
open Common

let property_test_count = 100

(* Compute the current price of kit in ctok, as estimated using the ratio of ctok and kit
 * currently in the cfmm contract. *)
let cfmm_kit_in_ctok (u: cfmm) =
  div_ratio (ratio_of_ctok u.ctok) (kit_to_ratio u.kit)

(* Compute the current product of kit and ctok, using the current contents of the cfmm
 * contract. *)
let cfmm_kit_times_ctok (u: cfmm) =
  mul_ratio (ratio_of_ctok u.ctok) (kit_to_ratio u.kit)

(* Reveal the current number of liquidity tokens extant. *)
let cfmm_liquidity_tokens_extant (u: cfmm) = u.lqt

(* amount > 0xtz *)
(* max_kit_deposited = CEIL{kit * amount / ctok} *)
(* min_lqt_minted = FLOOR{lqt * amount / ctok} *)
(* NB: some values are fixed *)
let make_inputs_for_add_liquidity_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. The liquidity created can be zero for example. *)
    (fun ((ctok, kit, lqt, cfmm), amount) ->
       let max_kit_deposited =
         let { num = x_num; den = x_den; } =
           mul_ratio (kit_to_ratio kit) (make_ratio (ctok_to_muctok_int amount) (ctok_to_muctok_int ctok)) in
         kit_of_fraction_ceil x_num x_den
       in
       let min_lqt_minted =
         let { num = x_num; den = x_den; } =
           mul_ratio (lqt_to_ratio lqt) (make_ratio (ctok_to_muctok_int amount) (ctok_to_muctok_int ctok)) in
         lqt_of_fraction_floor x_num x_den
       in
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline)
    )
    (QCheck.pair (arbitrary_non_empty_cfmm one_ratio !Ligo.Tezos.level) TestArbitrary.arb_positive_ctok)

(* NB: some values are fixed *)
let make_inputs_for_remove_liquidity_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. *)
    (fun ((ctok, kit, lqt, cfmm), factor) ->
       let lqt_to_burn =
         let { num = x_num; den = x_den; } =
           div_ratio (lqt_to_ratio lqt) (ratio_of_int (Ligo.int_from_literal (string_of_int factor))) in
         lqt_of_fraction_floor x_num x_den
       in

       (* let lqt_to_burn = if lqt_to_burn = Ligo.int_from_literal 0 then Ligo.int_from_literal 1 else lqt_to_burn in *)

       let lqt_burned = lqt_to_burn in
       let min_ctok_withdrawn =
         let { num = x_num; den = x_den; } =
           div_ratio (mul_ratio (ratio_of_ctok ctok) (lqt_to_ratio lqt_to_burn)) (lqt_to_ratio lqt) in
         ctok_of_fraction_floor x_num x_den
       in
       let min_kit_withdrawn =
         let { num = x_num; den = x_den; } =
           div_ratio (mul_ratio (kit_to_ratio kit) (lqt_to_ratio lqt_to_burn)) (lqt_to_ratio lqt) in
         kit_of_fraction_floor x_num x_den
       in

       (* NOTE: We cannot just factor down the number of liquidity tokens
        * extant for this operation. When we remove liquidity we round the
        * amounts of kit and ctok to return towards zero; they might end up
        * being zero because of this, which would make remove_liquidity fail.
        * We make the generator thus ensure that at least the lowest
        * denomination of each will be returned. *)
       let lqt_burned, min_ctok_withdrawn, min_kit_withdrawn =
         if lqt_to_burn = lqt_zero || min_ctok_withdrawn = ctok_zero || min_kit_withdrawn = kit_zero then
           let lqt_to_burn =
             let least_kit_percentage = (div_ratio (kit_to_ratio (kit_of_denomination (Ligo.nat_from_literal "1n"))) (kit_to_ratio kit)) in
             let least_ctok_percentage = make_ratio (ctok_to_muctok_int (ctok_of_muctok (Ligo.nat_from_literal "1n"))) (ctok_to_muctok_int ctok) in
             let as_q = (mul_ratio (lqt_to_ratio lqt) (max_ratio least_kit_percentage least_ctok_percentage)) in
             lqt_of_fraction_ceil as_q.num as_q.den in
           let lqt_burned = lqt_to_burn in
           let min_ctok_withdrawn =
             let { num = x_num; den = x_den; } =
               div_ratio (mul_ratio (ratio_of_ctok ctok) (lqt_to_ratio lqt_to_burn)) (lqt_to_ratio lqt) in
             ctok_of_fraction_floor x_num x_den
           in
           let min_kit_withdrawn =
             let { num = x_num; den = x_den; } =
               div_ratio (mul_ratio (kit_to_ratio kit) (lqt_to_ratio lqt_to_burn)) (lqt_to_ratio lqt) in
             kit_of_fraction_floor x_num x_den
           in
           (lqt_burned, min_ctok_withdrawn, min_kit_withdrawn)
         else
           lqt_burned, min_ctok_withdrawn, min_kit_withdrawn in

       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline)
    )
    (QCheck.pair (arbitrary_non_empty_cfmm one_ratio !Ligo.Tezos.level) QCheck.pos_int)

(* TODO: Write down for which inputs are the cfmm functions to succeed and
 * test the corresponding edge cases. *)

(* ************************************************************************* *)
(*                     buy_kit (property-based tests)                        *)
(* ************************************************************************* *)

(* If successful, cfmm_buy_kit always increases the ratio of
 * total_ctok/total_kit, since it adds ctok and removes kit. *)
let test_buy_kit_increases_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_increases_price"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (cfmm, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_cfmm =
    cfmm_buy_kit cfmm amount min_kit_expected deadline in
  gt_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm)

(* If successful, cfmm_buy_kit always increases the product
 * total_ctok * total_kit, because of the fees. *)
let test_buy_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_increases_product"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (cfmm, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_cfmm =
    cfmm_buy_kit cfmm amount min_kit_expected deadline in
  gt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm)

(* Successful or not, cfmm_buy_kit should never affect the number of
 * liquidity tokens extant. *)
let test_buy_kit_does_not_affect_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_does_not_affect_liquidity"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (cfmm, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_cfmm =
    cfmm_buy_kit cfmm amount min_kit_expected deadline in
  cfmm_liquidity_tokens_extant new_cfmm = cfmm_liquidity_tokens_extant cfmm

(* If successful, cfmm_buy_kit respects min_kit_expected. *)
let test_buy_kit_respects_min_kit_expected =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_respects_min_kit_expected"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (cfmm, amount, min_kit_expected, deadline) ->
  let bought_kit, _new_cfmm =
    cfmm_buy_kit cfmm amount min_kit_expected deadline in
  bought_kit >= min_kit_expected

(* If successful, cfmm_buy_kit doesn't lose kit.
 * Note that, because kits are isomorphic to naturals,
 * this also means that cfmm_buy_kit doesn't return more kit than cfmm had. *)
let test_buy_kit_preserves_kit =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_preserves_kit"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (cfmm, amount, min_kit_expected, deadline) ->
  let bought_kit, new_cfmm =
    cfmm_buy_kit cfmm amount min_kit_expected deadline in
  cfmm.kit = kit_add new_cfmm.kit bought_kit

(* If successful, cfmm_buy_kit doesn't lose ctok. *)
let test_buy_kit_preserves_ctok =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_preserves_ctok"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (cfmm, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_cfmm =
    cfmm_buy_kit cfmm amount min_kit_expected deadline in
  ctok_add cfmm.ctok amount = new_cfmm.ctok

(* ************************************************************************* *)
(*                          buy_kit (unit tests)                             *)
(* ************************************************************************* *)

let buy_kit_unit_test =
  "buy kit unit test" >:: fun _ ->
    Ligo.Tezos.reset ();
    let cfmm : cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "10_000_000n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "5_000_000n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1n"))
        ~kit_in_ctok_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in

    let expected_returned_kit = kit_of_denomination (Ligo.nat_from_literal "453_636n") in
    let expected_updated_cfmm : cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "11_000_000n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "4_546_364n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1n"))
        ~kit_in_ctok_in_prev_block:(ratio_of_int (Ligo.int_from_literal "2"))
        ~last_level:(Ligo.nat_from_literal "1n")
    in

    (* Low expectations and on time (lax): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_kit, updated_cfmm =
      cfmm_buy_kit
        cfmm
        (ctok_of_muctok (Ligo.nat_from_literal "1_000_000n"))
        (kit_of_denomination (Ligo.nat_from_literal "1n"))
        (Ligo.timestamp_from_seconds_literal 10) in
    assert_kit_equal ~expected:expected_returned_kit ~real:returned_kit;
    assert_cfmm_equal ~expected:expected_updated_cfmm ~real:updated_cfmm;

    (* Low expectations and on time (tight): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_kit, updated_cfmm =
      cfmm_buy_kit
        cfmm
        (ctok_of_muctok (Ligo.nat_from_literal "1_000_000n"))
        (kit_of_denomination (Ligo.nat_from_literal "453_636n"))
        (Ligo.timestamp_from_seconds_literal 2) in
    assert_kit_equal ~expected:expected_returned_kit ~real:returned_kit;
    assert_cfmm_equal ~expected:expected_updated_cfmm ~real:updated_cfmm;

    (* High expectations but on time (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_BuyKitPriceFailure))
      (fun () ->
         cfmm_buy_kit
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1_000_000n"))
           (kit_of_denomination (Ligo.nat_from_literal "453_637n"))
           (Ligo.timestamp_from_seconds_literal 2)
      );

    (* Low expectations but too late (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_CfmmTooLate))
      (fun () ->
         cfmm_buy_kit
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1_000_000n"))
           (kit_of_denomination (Ligo.nat_from_literal "453_636n"))
           (Ligo.timestamp_from_seconds_literal 1)
      );

    (* No ctok given: fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_BuyKitNoCtokGiven))
      (fun () ->
         cfmm_buy_kit
           cfmm
           ctok_zero
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           (Ligo.timestamp_from_seconds_literal 10)
      );

    (* No kit expected: fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_BuyKitTooLowExpectedKit))
      (fun () ->
         cfmm_buy_kit
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "0n"))
           (Ligo.timestamp_from_seconds_literal 10)
      )

(* ************************************************************************* *)
(*                     sell_kit (property-based tests)                       *)
(* ************************************************************************* *)

(* If successful, cfmm_sell_kit always decreases the ratio of
 * total_ctok/total_kit, since it removes ctok and adds kit. *)
let test_sell_kit_decreases_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_decreases_price"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
  let _bought_ctok, new_cfmm =
    cfmm_sell_kit cfmm kit_amount min_ctok_expected deadline in
  lt_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm)

(* If successful, cfmm_sell_kit always increases the product
 * total_ctok * total_kit, because of the fees. *)
let test_sell_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_increases_product"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
  let _bought_ctok, new_cfmm =
    cfmm_sell_kit cfmm kit_amount min_ctok_expected deadline in
  gt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm)

(* Successful or not, cfmm_sell_kit should never affect the number of
 * liquidity tokens extant. *)
let test_sell_kit_does_not_affect_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_does_not_affect_liquidity"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
  let _bought_ctok, new_cfmm =
    cfmm_sell_kit cfmm kit_amount min_ctok_expected deadline in
  cfmm_liquidity_tokens_extant new_cfmm = cfmm_liquidity_tokens_extant cfmm

(* If successful, cfmm_sell_kit respects min_ctok_expected. *)
let test_sell_kit_respects_min_ctok_expected =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_respects_min_ctok_expected"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
  let bought_ctok, _new_cfmm =
    cfmm_sell_kit cfmm kit_amount min_ctok_expected deadline in
  bought_ctok >= min_ctok_expected

(* If successful, selling kit preserves kit. *)
let test_sell_kit_preserves_kit =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_preserves_kit"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
  let _bought_ctok, new_cfmm =
    cfmm_sell_kit cfmm kit_amount min_ctok_expected deadline in
  new_cfmm.kit = kit_add cfmm.kit kit_amount

(* If successful, selling kit preserves ctok. *)
let test_sell_kit_preserves_ctok =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_preserves_ctok"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
  let bought_ctok, new_cfmm =
    cfmm_sell_kit cfmm kit_amount min_ctok_expected deadline in
  ctok_add new_cfmm.ctok bought_ctok = cfmm.ctok

(* ************************************************************************* *)
(*                          sell_kit (unit tests)                            *)
(* ************************************************************************* *)

let sell_kit_unit_test =
  "sell kit" >:: fun _ ->
    Ligo.Tezos.reset ();
    let cfmm : cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "10_000_000n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "5_000_000n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1n"))
        ~kit_in_ctok_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in
    let expected_returned_ctok = (ctok_of_muctok (Ligo.nat_from_literal "1_663_333n")) in
    let expected_updated_cfmm : cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "8_336_667n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "6_000_000n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1n"))
        ~kit_in_ctok_in_prev_block:(ratio_of_int (Ligo.int_from_literal "2"))
        ~last_level:(Ligo.nat_from_literal "1n")
    in

    (* Low expectations and on time (lax): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_ctok, updated_cfmm =
      cfmm_sell_kit
        cfmm
        kit_one
        (ctok_of_muctok (Ligo.nat_from_literal "1n"))
        (Ligo.timestamp_from_seconds_literal 10) in
    assert_ctok_equal ~expected:expected_returned_ctok ~real:returned_ctok;
    assert_cfmm_equal ~expected:expected_updated_cfmm ~real:updated_cfmm;

    (* Low expectations and on time (tight): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_ctok, updated_cfmm =
      cfmm_sell_kit
        cfmm
        kit_one
        (ctok_of_muctok (Ligo.nat_from_literal "1_663_333n"))
        (Ligo.timestamp_from_seconds_literal 2) in
    assert_ctok_equal ~expected:expected_returned_ctok ~real:returned_ctok;
    assert_cfmm_equal ~expected:expected_updated_cfmm ~real:updated_cfmm;

    (* High expectations but on time (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_SellKitPriceFailure))
      (fun () ->
         cfmm_sell_kit
           cfmm
           kit_one
           (ctok_of_muctok (Ligo.nat_from_literal "1_663_334n"))
           (Ligo.timestamp_from_seconds_literal 2)
      );

    (* Low expectations but too late (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_CfmmTooLate))
      (fun () ->
         cfmm_sell_kit
           cfmm
           kit_one
           (ctok_of_muctok (Ligo.nat_from_literal "1_663_333n"))
           (Ligo.timestamp_from_seconds_literal 1)
      );

    (* No kit given: fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_SellKitNoKitGiven))
      (fun () ->
         cfmm_sell_kit
           cfmm
           (kit_of_denomination (Ligo.nat_from_literal "0n"))
           (ctok_of_muctok (Ligo.nat_from_literal "1_663_333n"))
           (Ligo.timestamp_from_seconds_literal 10)
      );

    (* No ctok expected: fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_SellKitTooLowExpectedCtok))
      (fun () ->
         cfmm_sell_kit
           cfmm
           kit_one
           ctok_zero
           (Ligo.timestamp_from_seconds_literal 10)
      )

(* ************************************************************************* *)
(*             add_liquidity (non-first) (property-based tests)              *)
(* ************************************************************************* *)

(* If successful, cfmm_add_liquidity never increases the ratio of
 * total_ctok/total_kit (might leave it where it is or decrease it), since it
 * always rounds up the kit it keeps in the contract. If amount is a multiple
 * of the ctok in the cfmm contract, then the price should remain the same,
 * hence the lack of strict monotonicity. *)
let test_add_liquidity_might_decrease_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_might_decrease_price"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed
  @@ fun (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_cfmm =
    cfmm_add_liquidity cfmm amount max_kit_deposited min_lqt_minted deadline in
  leq_ratio_ratio (cfmm_kit_in_ctok new_cfmm) (cfmm_kit_in_ctok cfmm)

(* If successful, cfmm_add_liquidity always increases the product
 * total_ctok * total_kit, because we add both ctok and kit. *)
let test_add_liquidity_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_product"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed
  @@ fun (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_cfmm =
    cfmm_add_liquidity cfmm amount max_kit_deposited min_lqt_minted deadline in
  gt_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm)

(* If successful, cfmm_add_liquidity always increases the liquidity;
 * that's what it's supposed to do. *)
let test_add_liquidity_increases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_liquidity"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed
  @@ fun (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_cfmm =
    cfmm_add_liquidity cfmm amount max_kit_deposited min_lqt_minted deadline in
  cfmm_liquidity_tokens_extant new_cfmm > cfmm_liquidity_tokens_extant cfmm

(* If successful, cfmm_add_liquidity always deposits some kit,
 * implying kit_to_return = max_kit_deposited - kit_deposited < max_kit_deposited. *)
let test_add_liquidity_kit_to_return_lt_max_kit_deposited =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_kit_to_return_lt_max_kit_deposited"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed
  @@ fun (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, kit_to_return, _new_cfmm =
    cfmm_add_liquidity cfmm amount max_kit_deposited min_lqt_minted deadline in
  kit_to_return < max_kit_deposited

(* If successful, cfmm_add_liquidity does not produce less kit than min_lqt_minted *)
let test_add_liquidity_respects_min_lqt_minted =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_respects_min_lqt_minted"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed
  @@ fun (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline) ->
  let lqt_minted, _bought_kit, _new_cfmm =
    cfmm_add_liquidity cfmm amount max_kit_deposited min_lqt_minted deadline in
  lqt_minted >= min_lqt_minted

(* If successful, cfmm_add_liquidity does not produce less kit than min_lqt_minted *)
let test_add_liquidity_respects_max_kit_deposited =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_respects_max_kit_deposited"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed
  @@ fun (cfmm, amount, max_kit_deposited, min_lqt_minted, deadline) ->
  let _lqt_minted, _bought_kit, new_cfmm =
    cfmm_add_liquidity cfmm amount max_kit_deposited min_lqt_minted deadline in
  new_cfmm.kit <= kit_add cfmm.kit max_kit_deposited

(* ************************************************************************* *)
(*                 add_liquidity (non-first) (unit tests)                    *)
(* ************************************************************************* *)

let add_liquidity_unit_test =
  "add liquidity unit test" >:: fun _ ->
    Ligo.Tezos.reset ();
    let cfmm : cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "8_336_667n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "6_000_000n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1n"))
        ~kit_in_ctok_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in
    let expected_returned_liquidity = lqt_of_denomination (Ligo.nat_from_literal "2n") in
    let expected_returned_kit = kit_of_denomination (Ligo.nat_from_literal "5_605_758n") in
    let expected_updated_cfmm : cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "28_336_667n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "20_394_242n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "3n"))
        ~kit_in_ctok_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in

    let returned_liquidity, returned_kit, updated_cfmm =
      cfmm_add_liquidity
        cfmm
        (ctok_of_muctok (Ligo.nat_from_literal "20_000_000n"))
        (kit_of_denomination (Ligo.nat_from_literal "20_000_000n"))
        (lqt_of_denomination (Ligo.nat_from_literal "2n"))
        (Ligo.timestamp_from_seconds_literal 1) in
    assert_lqt_equal ~expected:expected_returned_liquidity ~real:returned_liquidity;
    assert_kit_equal ~expected:expected_returned_kit ~real:returned_kit;
    assert_cfmm_equal ~expected:expected_updated_cfmm ~real:updated_cfmm

let test_add_liquidity_failures =
  "add liquidity failure conditions" >:: fun _ ->
    Ligo.Tezos.reset ();
    let cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "1000_000_000n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "5000_000_000n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1000n"))
        ~kit_in_ctok_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n") in
    assert_raises
      (Failure (Ligo.string_of_int error_AddLiquidityNoCtokGiven))
      (fun () ->
         cfmm_add_liquidity
           cfmm
           ctok_zero
           (kit_of_denomination (Ligo.nat_from_literal "20_000_000n"))
           (lqt_of_denomination (Ligo.nat_from_literal "2n"))
           (Ligo.timestamp_from_seconds_literal 1)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_AddLiquidityNoKitGiven))
      (fun () ->
         cfmm_add_liquidity
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "0n"))
           (lqt_of_denomination (Ligo.nat_from_literal "2n"))
           (Ligo.timestamp_from_seconds_literal 1)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_AddLiquidityNoLiquidityToBeAdded))
      (fun () ->
         cfmm_add_liquidity
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           lqt_zero
           (Ligo.timestamp_from_seconds_literal 1)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_CfmmTooLate))
      (fun () ->
         Ligo.Tezos.reset ();
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "0") in (* No time passed; failure (tests equality) *)
         cfmm_add_liquidity
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           lqt_zero
           deadline
      );
    assert_raises
      (Failure (Ligo.string_of_int error_CfmmTooLate))
      (fun () ->
         Ligo.Tezos.reset ();
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* One second passed; failure (tests inequality) *)
         Ligo.Tezos.new_transaction ~seconds_passed:2 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
         cfmm_add_liquidity
           cfmm
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           lqt_zero
           deadline
      )

(* ************************************************************************* *)
(*                 remove_liquidity (property-based tests)                   *)
(* ************************************************************************* *)

(* If successful, cfmm_remove_liquidity always decreases the product
 * total_ctok * total_kit, because we remove both ctok and kit. *)
(* NOTE: That is not entirely true, because when we remove liquidity we round
 * the amounts of kit and ctok to return towards zero; they might end up being
 * zero because of this. BUT, in these cases cfmm_remove_liquidity should
 * thrown an error, so this property is expected to hold indeed, when
 * remove_liquidity succeeds. *)
let test_remove_liquidity_decreases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_product"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_ctok, _withdrawn_kit, new_cfmm =
    cfmm_remove_liquidity cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in
  leq_ratio_ratio (cfmm_kit_times_ctok new_cfmm) (cfmm_kit_times_ctok cfmm)

(* If successful, cfmm_remove_liquidity always decreases the liquidity;
 * that's what it's supposed to do. *)
let test_remove_liquidity_decreases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_liquidity"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_ctok, _withdrawn_kit, new_cfmm =
    cfmm_remove_liquidity cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in
  cfmm_liquidity_tokens_extant new_cfmm < cfmm_liquidity_tokens_extant cfmm

(* If successful, cfmm_remove_liquidity removes at least min_ctok_withdrawn ctok. *)
let test_remove_liquidity_respects_min_ctok_withdrawn =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_min_ctok_withdrawn"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline) ->
  let withdrawn_ctok, _withdrawn_kit, _new_cfmm =
    cfmm_remove_liquidity cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in
  withdrawn_ctok >= min_ctok_withdrawn

(* If successful, cfmm_remove_liquidity removes at least min_kit_withdrawn kit. *)
let test_remove_liquidity_respects_min_kit_withdrawn =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_min_kit_withdrawn"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_ctok, withdrawn_kit, _new_cfmm =
    cfmm_remove_liquidity cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in
  withdrawn_kit >= min_kit_withdrawn

(* If successful, cfmm_remove_liquidity removes no more ctok than it had. *)
let test_remove_liquidity_respects_ctok_limit =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_ctok_limit"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline) ->
  let withdrawn_ctok, _withdrawn_kit, _new_cfmm =
    cfmm_remove_liquidity cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in
  withdrawn_ctok <= cfmm.ctok

(* If successful, cfmm_remove_liquidity removes no more kit than it had. *)
let test_remove_liquidity_respects_kit_limit =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_kit_limit"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (cfmm, lqt_burned, min_ctok_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_ctok, withdrawn_kit, _new_cfmm =
    cfmm_remove_liquidity cfmm lqt_burned min_ctok_withdrawn min_kit_withdrawn deadline in
  withdrawn_kit <= cfmm.kit

(* ************************************************************************* *)
(*                 remove_liquidity (unit tests)                             *)
(* ************************************************************************* *)

let test_remove_liquidity_failures =
  "remove liquidity failure conditions" >:: fun _ ->
    Ligo.Tezos.reset ();
    let cfmm =
      cfmm_make_for_test
        ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "1000_000_000n"))
        ~kit:(kit_of_denomination (Ligo.nat_from_literal "5000_000_000n"))
        ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "1000n"))
        ~kit_in_ctok_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n") in
    let (liq, _kit, cfmm) =
      cfmm_add_liquidity
        { cfmm with ctok = ctok_add cfmm.ctok (ctok_of_muctok (Ligo.nat_from_literal "10_000_000n")) }
        (ctok_of_muctok (Ligo.nat_from_literal "101_000_000n"))
        (kit_of_denomination (Ligo.nat_from_literal "500_000_000n"))
        (lqt_of_denomination (Ligo.nat_from_literal "1n"))
        (Ligo.timestamp_from_seconds_literal 1) in
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNoLiquidityBurned))
      (fun () ->
         cfmm_remove_liquidity
           cfmm
           lqt_zero
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           (Ligo.timestamp_from_seconds_literal 100)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNoCtokWithdrawnExpected))
      (fun () ->
         cfmm_remove_liquidity
           cfmm
           liq
           ctok_zero
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           (Ligo.timestamp_from_seconds_literal 100)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNoKitWithdrawnExpected))
      (fun () ->
         cfmm_remove_liquidity
           cfmm
           liq
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "0n"))
           (Ligo.timestamp_from_seconds_literal 100)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_CfmmTooLate))
      (fun () ->
         Ligo.Tezos.reset ();
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "0") in (* No time passed; failure (tests equality) *)
         cfmm_remove_liquidity
           cfmm
           liq
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           deadline
      );
    assert_raises
      (Failure (Ligo.string_of_int error_CfmmTooLate))
      (fun () ->
         Ligo.Tezos.reset ();
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* One second passed; failure (tests inequality) *)
         Ligo.Tezos.new_transaction ~seconds_passed:2 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
         cfmm_remove_liquidity
           cfmm
           liq
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           deadline
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityTooMuchLiquidityWithdrawn))
      (fun () ->
         Ligo.Tezos.reset ();
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in
         let liq = cfmm.lqt in (* too much (equal) *)
         cfmm_remove_liquidity
           cfmm
           liq
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           deadline
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityTooMuchLiquidityWithdrawn))
      (fun () ->
         Ligo.Tezos.reset ();
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in
         let liq = Lqt.lqt_add cfmm.lqt (Lqt.lqt_of_denomination (Ligo.nat_from_literal "1n")) in (* too much (unequal) *)
         cfmm_remove_liquidity
           cfmm
           liq
           (ctok_of_muctok (Ligo.nat_from_literal "1n"))
           (kit_of_denomination (Ligo.nat_from_literal "1n"))
           deadline
      )

let cfmm_tests_from_mutations =
  "Cfmm tests from mutations" >::: [
    (* This test catches the following mutation:
     *   cfmm.ml:245 (1_000_000 => 999_999)
     *
     * ctok_withdrawn is calculated as follows:
     *   ctok_withdrawn = FLOOR ((cfmm.ctok * lqt_burned) / (tez_sf * cfmm.lqt))
     *
     * so we can catch the mutation by using the following values:
     *   lqt_burned = 1
     *   cfmm.ctok = 1_000_000 * 999_999
     *   cfmm.lqt = 999_999
     * which give
     *   ctok_withdrawn = FLOOR (999_999_000_000 / (tez_sf * 999_999))
     * and thus
     *  tez_sf = 1_000_000 => ctok_withdrawn = 1_000_000
     *  tez_sf =   999_999 => ctok_withdrawn = 1_000_001
    *)
    ("cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity (1_000_000 => 999_999)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let cfmm =
         cfmm_make_for_test
           ~ctok:(ctok_of_muctok (Ligo.nat_from_literal "999_999_000_000n"))
           ~kit:(kit_of_denomination (Ligo.nat_from_literal "999_999_000_000n"))
           ~lqt:(lqt_of_denomination (Ligo.nat_from_literal "999_999n"))
           ~kit_in_ctok_in_prev_block:one_ratio
           ~last_level:!Ligo.Tezos.level in
       let lqt_burned = Lqt.lqt_of_denomination (Ligo.nat_from_literal "1n") in
       let (ctok_withdrawn, _kit_withdrawn, _cfmm) =
         cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity
           cfmm
           lqt_burned in
       assert_ctok_equal
         ~expected:(Ctok.ctok_of_muctok (Ligo.nat_from_literal "1_000_000n"))
         ~real:ctok_withdrawn
    );

    (* This test catches the following mutation:
     *   cfmm.ml:260 (kit_sub => kit_min)
    *)
    ("cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity (kit_sub => kit_min)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let total_ctok = ctok_of_muctok (Ligo.nat_from_literal "123_456_789n") in
       let total_kit = kit_of_denomination (Ligo.nat_from_literal "37_194_834n") in
       let total_lqt = lqt_of_denomination (Ligo.nat_from_literal "999_999n") in
       let cfmm =
         cfmm_make_for_test
           ~ctok:total_ctok
           ~kit:total_kit
           ~lqt:total_lqt
           ~kit_in_ctok_in_prev_block:one_ratio
           ~last_level:!Ligo.Tezos.level in

       let lqt_burned = Lqt.lqt_of_denomination (Ligo.nat_from_literal "312_465n") in
       let (ctok_withdrawn, kit_withdrawn, updated_cfmm) =
         cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity
           cfmm
           lqt_burned in
       (* zero sum properties *)
       assert_ctok_equal
         ~expected:cfmm.ctok
         ~real:(Ctok.ctok_add ctok_withdrawn updated_cfmm.ctok);
       assert_kit_equal
         ~expected:cfmm.kit
         ~real:(Kit.kit_add kit_withdrawn updated_cfmm.kit);
       assert_lqt_equal
         ~expected:cfmm.lqt
         ~real:(Lqt.lqt_add lqt_burned updated_cfmm.lqt);
       (* To give *)
       assert_ctok_equal
         ~expected:(Ctok.ctok_of_muctok (Ligo.nat_from_literal "38_575_964n"))
         ~real:ctok_withdrawn;
       assert_kit_equal
         ~expected:(Kit.kit_of_denomination (Ligo.nat_from_literal "11_622_095n"))
         ~real:kit_withdrawn;
       (* Updated cfmm *)
       assert_ctok_equal
         ~expected:(Ctok.ctok_of_muctok (Ligo.nat_from_literal "84_880_825n"))
         ~real:updated_cfmm.ctok;
       assert_kit_equal
         ~expected:(Kit.kit_of_denomination (Ligo.nat_from_literal "25_572_739n"))
         ~real:updated_cfmm.kit;
       assert_lqt_equal
         ~expected:(Lqt.lqt_of_denomination (Ligo.nat_from_literal "687_534n"))
         ~real:updated_cfmm.lqt;
       ()
    );

    (* This test catches the following mutation:
     *   cfmm.ml:300 (kit_add => kit_max)
    *)
    ("cfmm_view_min_ctok_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity (kit_sub => kit_min)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let total_ctok = ctok_of_muctok (Ligo.nat_from_literal "123_456_789n") in
       let total_kit = kit_of_denomination (Ligo.nat_from_literal "37_194_834n") in
       let total_lqt = lqt_of_denomination (Ligo.nat_from_literal "999_999n") in
       let cfmm =
         cfmm_make_for_test
           ~ctok:total_ctok
           ~kit:total_kit
           ~lqt:total_lqt
           ~kit_in_ctok_in_prev_block:one_ratio
           ~last_level:!Ligo.Tezos.level in
       let accrual = kit_of_denomination (Ligo.nat_from_literal "8_367n") in
       let updated_cfmm = cfmm_add_accrued_kit cfmm accrual in
       assert_kit_equal
         ~expected:(kit_add cfmm.kit accrual)
         ~real:updated_cfmm.kit
    );
  ]

let suite =
  "Cfmm tests" >::: [
    (* buy_kit *)
    buy_kit_unit_test;
    test_buy_kit_increases_price;
    test_buy_kit_increases_product;
    test_buy_kit_does_not_affect_liquidity;
    test_buy_kit_respects_min_kit_expected;
    test_buy_kit_preserves_kit;
    test_buy_kit_preserves_ctok;

    (* sell_kit *)
    sell_kit_unit_test;
    test_sell_kit_decreases_price;
    test_sell_kit_increases_product;
    test_sell_kit_does_not_affect_liquidity;
    test_sell_kit_respects_min_ctok_expected;
    test_sell_kit_preserves_kit;
    test_sell_kit_preserves_ctok;

    (* add_liquidity *)
    add_liquidity_unit_test;
    test_add_liquidity_failures;
    test_add_liquidity_might_decrease_price;
    test_add_liquidity_increases_product;
    test_add_liquidity_increases_liquidity;
    test_add_liquidity_kit_to_return_lt_max_kit_deposited;
    test_add_liquidity_respects_min_lqt_minted;
    test_add_liquidity_respects_max_kit_deposited;

    (* remove liquidity *)
    (* TODO: add unit tests *)
    test_remove_liquidity_failures;
    test_remove_liquidity_decreases_product;
    test_remove_liquidity_decreases_liquidity;
    test_remove_liquidity_respects_min_ctok_withdrawn;
    test_remove_liquidity_respects_min_kit_withdrawn;
    test_remove_liquidity_respects_ctok_limit;
    test_remove_liquidity_respects_kit_limit;

    (* Unit tests that arose from mutation testing *)
    cfmm_tests_from_mutations;
  ]

let () =
  run_test_tt_main
    suite
