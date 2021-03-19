open OUnit2
open TestCommon
open Ratio
open Kit
open Uniswap
open Tickets
open UniswapTypes
open Error

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

(* Issue an arbitrary amount of kit (checker-issued) *)
let arb_positive_kit_token = QCheck.map kit_issue TestArbitrary.arb_positive_kit

(* Issue an arbitrary number of liquidity tokens (checker-issued) *)
let arb_liquidity = QCheck.map (fun x -> Ligo.abs (Ligo.int_from_literal (string_of_int x))) QCheck.(0 -- max_int)

(* Create an arbitrary state for the uniswap contract (NB: some values are fixed). *)
let arbitrary_non_empty_uniswap (kit_in_tez_in_prev_block: ratio) (last_level: Ligo.nat) =
  QCheck.map
    (fun (tez, kit, lqt) ->
       (tez, kit, lqt, uniswap_make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block ~last_level)
    )
    (QCheck.triple TestArbitrary.arb_positive_tez TestArbitrary.arb_positive_kit arb_liquidity)

(* amount >= uniswap_tez * (1 - fee) / fee *)
(* 1mukit <= min_kit_expected < FLOOR{amount * (uniswap_kit / (uniswap_tez + amount)) * FACTOR} *)
(* NB: some values are fixed *)
let make_inputs_for_buy_kit_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _kit being ignored. *)
    (fun (tez, _kit, _lqt, uniswap) ->
       let amount =
         let { num = x_num; den = x_den; } =
           div_ratio (mul_ratio (ratio_of_tez tez) (sub_ratio one_ratio Constants.uniswap_fee)) Constants.uniswap_fee in
         fraction_to_tez_ceil x_num x_den in
       let min_kit_expected = kit_of_mukit (Ligo.nat_from_literal "1n") in (* absolute minimum *)
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (uniswap, amount, min_kit_expected, deadline)
    )
    (arbitrary_non_empty_uniswap one_ratio !Ligo.Tezos.level)

(* kit >= uniswap_kit * (1 - fee) / fee *)
(* 1mutez <= min_tez_expected < FLOOR{kit * (uniswap_tez / (uniswap_kit + kit)) * FACTOR} *)
(* NB: some values are fixed *)
let make_inputs_for_sell_kit_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _tez being ignored. *)
    (fun (_tez, kit, _lqt, uniswap) ->
       let amount = (Ligo.tez_from_literal "0mutez") in
       let token =
         let { num = x_num; den = x_den; } =
           div_ratio (mul_ratio (kit_to_ratio kit) (sub_ratio one_ratio Constants.uniswap_fee)) Constants.uniswap_fee in
         kit_of_fraction_ceil x_num x_den
       in
       let min_tez_expected = Ligo.tez_from_literal "1mutez" in (* absolute minimum *)
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (uniswap, amount, token, min_tez_expected, deadline)
    )
    (arbitrary_non_empty_uniswap one_ratio !Ligo.Tezos.level)

(* amount > 0xtz *)
(* max_kit_deposited = CEIL{kit * amount / tez} *)
(* min_lqt_minted = FLOOR{lqt * amount / tez} *)
(* NB: some values are fixed *)
let make_inputs_for_add_liquidity_to_succeed_no_accrual =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. The liquidity created can be zero for example. *)
    (fun ((tez, kit, lqt, uniswap), amount) ->
       let pending_accrual = (Ligo.tez_from_literal "0mutez") in
       let max_kit_deposited =
         let { num = x_num; den = x_den; } =
           mul_ratio (kit_to_ratio kit) (make_ratio (Common.tez_to_mutez amount) (Common.tez_to_mutez tez)) in
         kit_of_fraction_ceil x_num x_den
       in
       let min_lqt_minted =
         let { num = x_num; den = x_den; } =
           mul_ratio (ratio_of_nat lqt) (make_ratio (Common.tez_to_mutez amount) (Common.tez_to_mutez tez)) in
         fraction_to_nat_floor x_num x_den
       in
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline)
    )
    (QCheck.pair (arbitrary_non_empty_uniswap one_ratio !Ligo.Tezos.level) TestArbitrary.arb_positive_tez)

(* NB: some values are fixed *)
let make_inputs_for_remove_liquidity_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. *)
    (fun ((tez, kit, lqt, uniswap), factor) ->
       let amount = (Ligo.tez_from_literal "0mutez") in

       let lqt_to_burn =
         let { num = x_num; den = x_den; } =
           div_ratio (ratio_of_nat lqt) (ratio_of_int (Ligo.int_from_literal (string_of_int factor))) in
           fraction_to_nat_floor x_num x_den
       in

       (* let lqt_to_burn = if lqt_to_burn = Ligo.int_from_literal 0 then Ligo.int_from_literal 1 else lqt_to_burn in *)

       let lqt_burned = lqt_to_burn in
       let min_tez_withdrawn =
         let { num = x_num; den = x_den; } =
           div_ratio (mul_ratio (ratio_of_tez tez) (ratio_of_nat lqt_to_burn)) (ratio_of_nat lqt) in
         fraction_to_tez_floor x_num x_den
       in
       let min_kit_withdrawn =
         let { num = x_num; den = x_den; } =
           div_ratio (mul_ratio (kit_to_ratio kit) (ratio_of_nat lqt_to_burn)) (ratio_of_nat lqt) in
         kit_of_fraction_floor x_num x_den
       in

       (* NOTE: We cannot just factor down the number of liquidity tokens
        * extant for this operation. When we remove liquidity we round the
        * amounts of kit and tez to return towards zero; they might end up
        * being zero because of this, which would make remove_liquidity fail.
        * We make the generator thus ensure that at least 1mukit and 1mutez
        * will be returned. *)
       let lqt_burned, min_tez_withdrawn, min_kit_withdrawn =
         if lqt_to_burn = Ligo.nat_from_literal "0n" || min_tez_withdrawn = (Ligo.tez_from_literal "0mutez") || min_kit_withdrawn = kit_zero then
           let lqt_to_burn =
             let least_kit_percentage = (div_ratio (kit_to_ratio (kit_of_mukit (Ligo.nat_from_literal "1n"))) (kit_to_ratio kit)) in
             let least_tez_percentage = make_ratio (Common.tez_to_mutez (Ligo.tez_from_literal "1mutez")) (Common.tez_to_mutez tez) in
             let as_q = (mul_ratio (ratio_of_nat lqt) (max_ratio least_kit_percentage least_tez_percentage)) in
             Option.get (Ligo.is_nat (Common.cdiv_int_int as_q.num as_q.den)) in
           let lqt_burned = lqt_to_burn in
           let min_tez_withdrawn =
             let { num = x_num; den = x_den; } =
               div_ratio (mul_ratio (ratio_of_tez tez) (ratio_of_nat lqt_to_burn)) (ratio_of_nat lqt) in
             fraction_to_tez_floor x_num x_den
           in
           let min_kit_withdrawn =
             let { num = x_num; den = x_den; } =
               div_ratio (mul_ratio (kit_to_ratio kit) (ratio_of_nat lqt_to_burn)) (ratio_of_nat lqt) in
             kit_of_fraction_floor x_num x_den
           in
           (lqt_burned, min_tez_withdrawn, min_kit_withdrawn)
         else
           lqt_burned, min_tez_withdrawn, min_kit_withdrawn in

       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline)
    )
    (QCheck.pair (arbitrary_non_empty_uniswap one_ratio !Ligo.Tezos.level) QCheck.pos_int)

(* TODO: Write down for which inputs are the uniswap functions to succeed and
 * test the corresponding edge cases. *)

(* ************************************************************************* *)
(*                     buy_kit (property-based tests)                        *)
(* ************************************************************************* *)

(* If successful, uniswap_buy_kit always increases the ratio of
 * total_tez/total_kit, since it adds tez and removes kit. *)
let test_buy_kit_increases_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_increases_price"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (uniswap, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_uniswap =
    uniswap_buy_kit uniswap amount min_kit_expected deadline in
  gt_ratio_ratio (uniswap_kit_in_tez new_uniswap) (uniswap_kit_in_tez uniswap)

(* If successful, uniswap_buy_kit always increases the product
 * total_tez * total_kit, because of the fees. *)
let test_buy_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_increases_product"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (uniswap, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_uniswap =
    uniswap_buy_kit uniswap amount min_kit_expected deadline in
  gt_ratio_ratio (uniswap_kit_times_tez new_uniswap) (uniswap_kit_times_tez uniswap)

(* Successful or not, uniswap_buy_kit should never affect the number of
 * liquidity tokens extant. *)
let test_buy_kit_does_not_affect_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_does_not_affect_liquidity"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (uniswap, amount, min_kit_expected, deadline) ->
  let _bought_kit, new_uniswap =
    uniswap_buy_kit uniswap amount min_kit_expected deadline in
  uniswap_liquidity_tokens_extant new_uniswap = uniswap_liquidity_tokens_extant uniswap

(* ************************************************************************* *)
(*                          buy_kit (unit tests)                             *)
(* ************************************************************************* *)

let buy_kit_unit_test =
  "buy kit unit test" >:: fun _ ->
    Ligo.Tezos.reset ();
    let uniswap : uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "10_000_000mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "5_000_000n"))
        ~lqt:(Ligo.nat_from_literal "1n")
        ~kit_in_tez_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in

    let expected_returned_kit = kit_of_mukit (Ligo.nat_from_literal "453_636n") in
    let expected_updated_uniswap : uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "11_000_000mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "4_546_364n"))
        ~lqt:(Ligo.nat_from_literal "1n")
        ~kit_in_tez_in_prev_block:(ratio_of_int (Ligo.int_from_literal "2"))
        ~last_level:(Ligo.nat_from_literal "1n")
    in

    (* Low expectations and on time (lax): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_kit, updated_uniswap =
      uniswap_buy_kit
        uniswap
        (Ligo.tez_from_literal "1_000_000mutez")
        (kit_of_mukit (Ligo.nat_from_literal "1n"))
        (Ligo.timestamp_from_seconds_literal 10) in
    assert_equal ~printer:show_kit expected_returned_kit returned_kit;
    assert_equal ~printer:show_uniswap ~cmp:eq_uniswap expected_updated_uniswap updated_uniswap;

    (* Low expectations and on time (tight): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_kit, updated_uniswap =
      uniswap_buy_kit
        uniswap
        (Ligo.tez_from_literal "1_000_000mutez")
        (kit_of_mukit (Ligo.nat_from_literal "453_636n"))
        (Ligo.timestamp_from_seconds_literal 2) in
    assert_equal ~printer:show_kit expected_returned_kit returned_kit;
    assert_equal ~printer:show_uniswap ~cmp:eq_uniswap expected_updated_uniswap updated_uniswap;

    (* High expectations but on time (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_BuyKitPriceFailure))
      (fun () ->
         uniswap_buy_kit
           uniswap
           (Ligo.tez_from_literal "1_000_000mutez")
           (kit_of_mukit (Ligo.nat_from_literal "453_637n"))
           (Ligo.timestamp_from_seconds_literal 2)
      );

    (* Low expectations but too late (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_UniswapTooLate))
      (fun () ->
         uniswap_buy_kit
           uniswap
           (Ligo.tez_from_literal "1_000_000mutez")
           (kit_of_mukit (Ligo.nat_from_literal "453_636n"))
           (Ligo.timestamp_from_seconds_literal 1)
      )

(* ************************************************************************* *)
(*                     sell_kit (property-based tests)                       *)
(* ************************************************************************* *)

(* If successful, uniswap_sell_kit always decreases the ratio of
 * total_tez/total_kit, since it removes tez and adds kit. *)
let test_sell_kit_decreases_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_decreases_price"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (uniswap, amount, token, min_tez_expected, deadline) ->
  let _bought_tez, new_uniswap =
    uniswap_sell_kit uniswap amount token min_tez_expected deadline in
  lt_ratio_ratio (uniswap_kit_in_tez new_uniswap) (uniswap_kit_in_tez uniswap)

(* If successful, uniswap_sell_kit always increases the product
 * total_tez * total_kit, because of the fees. *)
let test_sell_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_increases_product"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (uniswap, amount, token, min_tez_expected, deadline) ->
  let _bought_tez, new_uniswap =
    uniswap_sell_kit uniswap amount token min_tez_expected deadline in
  gt_ratio_ratio (uniswap_kit_times_tez new_uniswap) (uniswap_kit_times_tez uniswap)

(* Successful or not, uniswap_sell_kit should never affect the number of
 * liquidity tokens extant. *)
let test_sell_kit_does_not_affect_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_does_not_affect_liquidity"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (uniswap, amount, token, min_tez_expected, deadline) ->
  let _bought_tez, new_uniswap =
    uniswap_sell_kit uniswap amount token min_tez_expected deadline in
  uniswap_liquidity_tokens_extant new_uniswap = uniswap_liquidity_tokens_extant uniswap

(* ************************************************************************* *)
(*                          sell_kit (unit tests)                            *)
(* ************************************************************************* *)

let sell_kit_unit_test =
  "sell kit" >:: fun _ ->
    Ligo.Tezos.reset ();
    let uniswap : uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "10_000_000mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "5_000_000n"))
        ~lqt:(Ligo.nat_from_literal "1n")
        ~kit_in_tez_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in
    let expected_returned_tez = Ligo.tez_from_literal "1_663_333mutez" in
    let expected_updated_uniswap : uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "8_336_667mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "6_000_000n"))
        ~lqt:(Ligo.nat_from_literal "1n")
        ~kit_in_tez_in_prev_block:(ratio_of_int (Ligo.int_from_literal "2"))
        ~last_level:(Ligo.nat_from_literal "1n")
    in

    (* Low expectations and on time (lax): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_tez, updated_uniswap =
      uniswap_sell_kit
        uniswap
        (Ligo.tez_from_literal "0mutez")
        kit_one
        (Ligo.tez_from_literal "1mutez")
        (Ligo.timestamp_from_seconds_literal 10) in
    assert_equal ~printer:Ligo.string_of_tez expected_returned_tez returned_tez;
    assert_equal ~printer:show_uniswap ~cmp:eq_uniswap expected_updated_uniswap updated_uniswap;

    (* Low expectations and on time (tight): pass *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    let returned_tez, updated_uniswap =
      uniswap_sell_kit
        uniswap
        (Ligo.tez_from_literal "0mutez")
        kit_one
        (Ligo.tez_from_literal "1_663_333mutez")
        (Ligo.timestamp_from_seconds_literal 2) in
    assert_equal ~printer:Ligo.string_of_tez expected_returned_tez returned_tez;
    assert_equal ~printer:show_uniswap ~cmp:eq_uniswap expected_updated_uniswap updated_uniswap;

    (* High expectations but on time (tight): fail *)
    Ligo.Tezos.reset ();
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_SellKitPriceFailure))
      (fun () ->
         uniswap_sell_kit
           uniswap
           (Ligo.tez_from_literal "0mutez")
           kit_one
           (Ligo.tez_from_literal "1_663_334mutez")
           (Ligo.timestamp_from_seconds_literal 2)
      );

    (* Low expectations but too late (tight): fail *)
    Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
    assert_raises
      (Failure (Ligo.string_of_int error_UniswapTooLate))
      (fun () ->
         uniswap_sell_kit
           uniswap
           (Ligo.tez_from_literal "0mutez")
           kit_one
           (Ligo.tez_from_literal "1_663_333mutez")
           (Ligo.timestamp_from_seconds_literal 1)
      )

(* ************************************************************************* *)
(*             add_liquidity (non-first) (property-based tests)              *)
(* ************************************************************************* *)

(* If successful, uniswap_add_liquidity never increases the ratio of
 * total_tez/total_kit (might leave it where it is or decrease it), since it
 * always rounds up the kit it keeps in the contract. If amount is a multiple
 * of the tez in the uniswap contract, then the price should remain the same,
 * hence the lack of strict monotonicity. *)
let test_add_liquidity_might_decrease_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_might_decrease_price"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_uniswap =
    uniswap_add_liquidity uniswap amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  leq_ratio_ratio (uniswap_kit_in_tez new_uniswap) (uniswap_kit_in_tez uniswap)

(* If successful, uniswap_add_liquidity always increases the product
 * total_tez * total_kit, because we add both tez and kit. *)
let test_add_liquidity_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_product"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_uniswap =
    uniswap_add_liquidity uniswap amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  gt_ratio_ratio (uniswap_kit_times_tez new_uniswap) (uniswap_kit_times_tez uniswap)

(* If successful, uniswap_add_liquidity always increases the liquidity;
 * that's what it's supposed to do. *)
let test_add_liquidity_increases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_liquidity"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_uniswap =
    uniswap_add_liquidity uniswap amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  uniswap_liquidity_tokens_extant new_uniswap > uniswap_liquidity_tokens_extant uniswap

(* If successful, uniswap_add_liquidity always deposits some kit,
 * implying kit_to_return = max_kit_deposited - kit_deposited < max_kit_deposited. *)
let test_add_liquidity_kit_to_return_lt_max_kit_deposited =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_kit_to_return_lt_max_kit_deposited"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, kit_to_return, _new_uniswap =
    uniswap_add_liquidity uniswap amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  kit_to_return < max_kit_deposited

(* If successful, uniswap_add_liquidity does not produce less kit than min_lqt_minted *)
let test_add_liquidity_respects_min_lqt_minted =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_respects_min_lqt_minted"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let lqt_minted, _bought_kit, _new_uniswap =
    uniswap_add_liquidity uniswap amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  lqt_minted >= min_lqt_minted

(* If successful, uniswap_add_liquidity does not produce less kit than min_lqt_minted *)
let test_add_liquidity_respects_max_kit_deposited =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_respects_max_kit_deposited"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _lqt_minted, _bought_kit, new_uniswap =
    uniswap_add_liquidity uniswap amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  new_uniswap.kit <= kit_add uniswap.kit max_kit_deposited

(* ************************************************************************* *)
(*                 add_liquidity (non-first) (unit tests)                    *)
(* ************************************************************************* *)

let add_liquidity_unit_test =
  "add liquidity unit test" >:: fun _ ->
    Ligo.Tezos.reset ();
    let uniswap : uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "8_336_667mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "6_000_000n"))
        ~lqt:(Ligo.nat_from_literal "1n")
        ~kit_in_tez_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in
    let expected_returned_liquidity = Ligo.nat_from_literal "2n" in
    let expected_returned_kit = kit_of_mukit (Ligo.nat_from_literal "5_605_758n") in
    let expected_updated_uniswap : uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "28_336_667mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "20_394_242n"))
        ~lqt:(Ligo.nat_from_literal "3n")
        ~kit_in_tez_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n")
    in

    let returned_liquidity, returned_kit, updated_uniswap =
      uniswap_add_liquidity
        uniswap
        (Ligo.tez_from_literal "20_000_000mutez")
        (Ligo.tez_from_literal "0mutez")
        (kit_of_mukit (Ligo.nat_from_literal "20_000_000n"))
        (Ligo.nat_from_literal "2n")
        (Ligo.timestamp_from_seconds_literal 1) in
    assert_equal ~printer:Ligo.string_of_nat expected_returned_liquidity returned_liquidity;
    assert_equal ~printer:show_kit expected_returned_kit returned_kit;
    assert_equal ~printer:show_uniswap ~cmp:eq_uniswap expected_updated_uniswap updated_uniswap

let test_add_liquidity_failures =
  "add liquidity failure conditions" >:: fun _ ->
    Ligo.Tezos.reset ();
    let uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "1000_000_000mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "5000_000_000n"))
        ~lqt:(Ligo.nat_from_literal "1000n")
        ~kit_in_tez_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n") in
    assert_raises
      (Failure (Ligo.string_of_int error_AddLiquidityNoTezGiven))
      (fun () ->
         uniswap_add_liquidity
           uniswap
           (Ligo.tez_from_literal "0mutez")
           (Ligo.tez_from_literal "0mutez")
           (kit_of_mukit (Ligo.nat_from_literal "20_000_000n"))
           (Ligo.nat_from_literal "2n")
           (Ligo.timestamp_from_seconds_literal 1)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_AddLiquidityNoKitGiven))
      (fun () ->
         uniswap_add_liquidity
           uniswap
           (Ligo.tez_from_literal "1mutez")
           (Ligo.tez_from_literal "0mutez")
           (kit_of_mukit (Ligo.nat_from_literal "0n"))
           (Ligo.nat_from_literal "2n")
           (Ligo.timestamp_from_seconds_literal 1)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_AddLiquidityNoLiquidityToBeAdded))
      (fun () ->
         uniswap_add_liquidity
           uniswap
           (Ligo.tez_from_literal "1mutez")
           (Ligo.tez_from_literal "0mutez")
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           (Ligo.nat_from_literal "0n")
           (Ligo.timestamp_from_seconds_literal 1)
      )

(* ************************************************************************* *)
(*                 remove_liquidity (property-based tests)                   *)
(* ************************************************************************* *)

(* If successful, uniswap_remove_liquidity always decreases the product
 * total_tez * total_kit, because we remove both tez and kit. *)
(* NOTE: That is not entirely true, because when we remove liquidity we round
 * the amounts of kit and tez to return towards zero; they might end up being
 * zero because of this. BUT, in these cases uniswap_remove_liquidity should
 * thrown an error, so this property is expected to hold indeed, when
 * remove_liquidity succeeds. *)
let test_remove_liquidity_decreases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_product"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_tez, _withdrawn_kit, new_uniswap =
    uniswap_remove_liquidity uniswap amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  leq_ratio_ratio (uniswap_kit_times_tez new_uniswap) (uniswap_kit_times_tez uniswap)

(* If successful, uniswap_remove_liquidity always decreases the liquidity;
 * that's what it's supposed to do. *)
let test_remove_liquidity_decreases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_liquidity"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_tez, _withdrawn_kit, new_uniswap =
    uniswap_remove_liquidity uniswap amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  uniswap_liquidity_tokens_extant new_uniswap < uniswap_liquidity_tokens_extant uniswap

(* If successful, uniswap_remove_liquidity removes at least min_tez_withdrawn tez. *)
let test_remove_liquidity_respects_min_tez_withdrawn =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_min_tez_withdrawn"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let withdrawn_tez, _withdrawn_kit, _new_uniswap =
    uniswap_remove_liquidity uniswap amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  withdrawn_tez >= min_tez_withdrawn

(* If successful, uniswap_remove_liquidity removes at least min_kit_withdrawn kit. *)
let test_remove_liquidity_respects_min_kit_withdrawn =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_min_kit_withdrawn"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_tez, withdrawn_kit, _new_uniswap =
    uniswap_remove_liquidity uniswap amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  withdrawn_kit >= min_kit_withdrawn

(* If successful, uniswap_remove_liquidity removes no more tez than it had. *)
let test_remove_liquidity_respects_tez_limit =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_tez_limit"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let withdrawn_tez, _withdrawn_kit, _new_uniswap =
    uniswap_remove_liquidity uniswap amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  withdrawn_tez <= uniswap.tez

(* If successful, uniswap_remove_liquidity removes no more kit than it had. *)
let test_remove_liquidity_respects_kit_limit =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_respects_kit_limit"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_tez, withdrawn_kit, _new_uniswap =
    uniswap_remove_liquidity uniswap amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  withdrawn_kit <= uniswap.kit

(* ************************************************************************* *)
(*                 remove_liquidity (unit tests)                             *)
(* ************************************************************************* *)

let test_remove_liquidity_failures =
  "remove liquidity failure conditions" >:: fun _ ->
    Ligo.Tezos.reset ();
    let uniswap =
      uniswap_make_for_test
        ~tez:(Ligo.tez_from_literal "1000_000_000mutez")
        ~kit:(kit_of_mukit (Ligo.nat_from_literal "5000_000_000n"))
        ~lqt:(Ligo.nat_from_literal "1000n")
        ~kit_in_tez_in_prev_block:one_ratio
        ~last_level:(Ligo.nat_from_literal "0n") in
     let (liq, _kit, uniswap) =
       uniswap_add_liquidity
         uniswap
         (Ligo.tez_from_literal "101_000_000mutez")
         (Ligo.tez_from_literal "10_000_000mutez")
         (kit_of_mukit (Ligo.nat_from_literal "500_000_000n"))
         (Ligo.nat_from_literal "1n")
         (Ligo.timestamp_from_seconds_literal 1) in
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNonEmptyAmount))
      (fun () ->
         uniswap_remove_liquidity
           uniswap
           (Ligo.tez_from_literal "1mutez")
           liq
           (Ligo.tez_from_literal "1mutez")
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           (Ligo.timestamp_from_seconds_literal 100)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNoLiquidityBurned))
      (fun () ->
         uniswap_remove_liquidity
           uniswap
           (Ligo.tez_from_literal "0mutez")
           (Ligo.nat_from_literal "0n")
           (Ligo.tez_from_literal "1mutez")
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           (Ligo.timestamp_from_seconds_literal 100)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNoTezWithdrawnExpected))
      (fun () ->
         uniswap_remove_liquidity
           uniswap
           (Ligo.tez_from_literal "0mutez")
           liq
           (Ligo.tez_from_literal "0mutez")
           (kit_of_mukit (Ligo.nat_from_literal "1n"))
           (Ligo.timestamp_from_seconds_literal 100)
      );
    assert_raises
      (Failure (Ligo.string_of_int error_RemoveLiquidityNoKitWithdrawnExpected))
      (fun () ->
         uniswap_remove_liquidity
           uniswap
           (Ligo.tez_from_literal "0mutez")
           liq
           (Ligo.tez_from_literal "1mutez")
           (kit_of_mukit (Ligo.nat_from_literal "0n"))
           (Ligo.timestamp_from_seconds_literal 100)
      )

(* ************************************************************************* *)
(*                 liquidity when accruals are pending                       *)
(* ************************************************************************* *)

let pending_tez_deposit_test =
  "set pending tez deposit" >::
  (fun _ ->
     Ligo.Tezos.reset ();
     let uniswap =
       uniswap_make_for_test
         ~tez:(Ligo.tez_from_literal "1000_000_000mutez")
         ~kit:(kit_of_mukit (Ligo.nat_from_literal "5000_000_000n"))
         ~lqt:(Ligo.nat_from_literal "1000n")
         ~kit_in_tez_in_prev_block:one_ratio
         ~last_level:(Ligo.nat_from_literal "0n") in
     (* let uniswap = set_pending_accrued_tez uniswap (Ligo.tez_from_literal "1_000_000mutez") in *)

     let (liq, _kit, uniswap) =
       uniswap_add_liquidity
         uniswap
         (Ligo.tez_from_literal "101_000_000mutez")
         (Ligo.tez_from_literal "10_000_000mutez")
         (kit_of_mukit (Ligo.nat_from_literal "500_000_000n"))
         (Ligo.nat_from_literal "1n")
         (Ligo.timestamp_from_seconds_literal 1) in
     let (tez, kit, _) =
       uniswap_remove_liquidity uniswap
         (Ligo.tez_from_literal "0mutez")
         liq
         (Ligo.tez_from_literal "1mutez")
         (kit_of_mukit (Ligo.nat_from_literal "1n"))
         (Ligo.timestamp_from_seconds_literal 100) in
     assert_equal ~printer:show_kit (kit_of_mukit (Ligo.nat_from_literal "500_000_000n")) kit;
     assert_equal ~printer:Ligo.string_of_tez (Ligo.tez_from_literal "100_090_909mutez") tez;
  )

let suite =
  "Uniswap tests" >::: [
    (* buy_kit *)
    buy_kit_unit_test;
    test_buy_kit_increases_price;
    test_buy_kit_increases_product;
    test_buy_kit_does_not_affect_liquidity;

    (* sell_kit *)
    sell_kit_unit_test;
    test_sell_kit_decreases_price;
    test_sell_kit_increases_product;
    test_sell_kit_does_not_affect_liquidity;

    (* add_liquidity (first) *)
    (* TODO: add unit tests and property-based random tests *)

    (* add_liquidity (non-first) *)
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
    test_remove_liquidity_respects_min_tez_withdrawn;
    test_remove_liquidity_respects_min_kit_withdrawn;
    test_remove_liquidity_respects_tez_limit;
    test_remove_liquidity_respects_kit_limit;

    pending_tez_deposit_test;
  ]
