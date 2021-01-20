open OUnit2
open TestCommon

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let level0 = Level.of_int 0
let level1 = Level.of_int 1
let tezos0 =
  Tezos.{
    now = Ligo.timestamp_from_seconds_literal 0;
    level = level0;
    self = Ligo.address_from_literal "checker";
    amount = Ligo.tez_from_mutez_literal 0;
    sender = Ligo.address_from_literal "somebody";
  }

(* Issue an arbitrary amount of kit (checker-issued) *)
let arb_positive_kit_token = QCheck.map (Kit.issue ~tezos:tezos0) TestArbitrary.arb_positive_kit

(* Issue an arbitrary number of liquidity tokens (checker-issued) *)
let arb_liquidity = QCheck.map (fun x -> Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.abs (Ligo.int_from_literal x))) QCheck.(0 -- max_int)

(* Create an arbitrary state for the uniswap contract (NB: some values are fixed). *)
let arbitrary_non_empty_uniswap (kit_in_tez_in_prev_block: Ratio.t) (last_level: Level.t) =
  QCheck.map
    (fun (tez, kit, lqt) ->
       (tez, kit, lqt, Uniswap.make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block ~last_level)
    )
    (QCheck.triple TestArbitrary.arb_positive_tez arb_positive_kit_token arb_liquidity)

(* amount >= uniswap_tez * (1 - fee) / fee *)
(* 1mukit <= min_kit_expected < FLOOR{amount * (uniswap_kit / (uniswap_tez + amount)) * FACTOR} *)
(* NB: some values are fixed *)
let make_inputs_for_buy_kit_to_succeed =
  let tezos = tezos0 in
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _kit being ignored. *)
    (fun (tez, _kit, _lqt, uniswap) ->
       let amount = Ratio.to_tez_ceil (Ratio.div (Ratio.mul (Ratio.of_tez tez) (Ratio.sub Ratio.one Constants.uniswap_fee)) Constants.uniswap_fee) in
       let min_kit_expected = Kit.of_mukit (Ligo.int_from_literal 1) in (* absolute minimum *)
       let deadline = Ligo.add_timestamp_int tezos.now (Ligo.int_from_literal 1) in (* always one second later *)
       (uniswap, amount, min_kit_expected, tezos, deadline)
    )
    (arbitrary_non_empty_uniswap Ratio.one tezos.level)

(* kit >= uniswap_kit * (1 - fee) / fee *)
(* 1mutez <= min_tez_expected < FLOOR{kit * (uniswap_tez / (uniswap_kit + kit)) * FACTOR} *)
(* NB: some values are fixed *)
let make_inputs_for_sell_kit_to_succeed =
  let tezos = tezos0 in
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _tez being ignored. *)
    (fun (_tez, kit, _lqt, uniswap) ->
       let amount = (Ligo.tez_from_mutez_literal 0) in
       let token =
         let kit, _same_ticket = Kit.read_kit kit in
         Kit.issue ~tezos (Kit.of_ratio_ceil (Ratio.div (Ratio.mul (Kit.to_ratio kit) (Ratio.sub Ratio.one Constants.uniswap_fee)) Constants.uniswap_fee)) in
       let min_tez_expected = Ligo.tez_from_mutez_literal 1 in (* absolute minimum *)
       let deadline = Ligo.add_timestamp_int tezos.now (Ligo.int_from_literal 1) in (* always one second later *)
       (uniswap, amount, token, min_tez_expected, tezos, deadline)
    )
    (arbitrary_non_empty_uniswap Ratio.one tezos.level)

(* amount > 0xtz *)
(* max_kit_deposited = CEIL{kit * amount / tez} *)
(* min_lqt_minted = FLOOR{lqt * amount / tez} *)
(* NB: some values are fixed *)
let make_inputs_for_add_liquidity_to_succeed_no_accrual =
  let tezos = tezos0 in
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. The liquidity created can be zero for example. *)
    (fun ((tez, kit, lqt, uniswap), amount) ->
       let pending_accrual = (Ligo.tez_from_mutez_literal 0) in
       let max_kit_deposited =
         let kit, _same_ticket = Kit.read_kit kit in
         Kit.issue ~tezos (Kit.of_ratio_ceil (Ratio.mul (Kit.to_ratio kit) (Ratio.make (Common.tez_to_mutez amount) (Common.tez_to_mutez tez)))) in
       let min_lqt_minted =
         let (_, _, lqt), _same_ticket = Tezos.read_ticket lqt in
         Ratio.to_nat_floor (Ratio.mul (Ratio.of_nat lqt) (Ratio.make (Common.tez_to_mutez amount) (Common.tez_to_mutez tez))) in
       let deadline = Ligo.add_timestamp_int tezos.now (Ligo.int_from_literal 1) in (* always one second later *)
       (uniswap, tezos, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline)
    )
    (QCheck.pair (arbitrary_non_empty_uniswap Ratio.one tezos.level) TestArbitrary.arb_positive_tez)

(* NB: some values are fixed *)
let make_inputs_for_remove_liquidity_to_succeed =
  let tezos = tezos0 in
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. *)
    (fun ((tez, kit, lqt, uniswap), factor) ->
       let amount = (Ligo.tez_from_mutez_literal 0) in

       let kit, _same_kit_ticket = Kit.read_kit kit in
       let (_, _, lqt), _same_lqt_ticket = Tezos.read_ticket lqt in
       let lqt_to_burn = Ratio.to_nat_floor (Ratio.div (Ratio.of_nat lqt) (Ratio.of_int factor)) in
       (* let lqt_to_burn = if lqt_to_burn = Ligo.int_from_literal 0 then Ligo.int_from_literal 1 else lqt_to_burn in *)

       let lqt_burned = Uniswap.issue_liquidity_tokens ~tezos lqt_to_burn in
       let min_tez_withdrawn = Ratio.to_tez_floor (Ratio.div (Ratio.mul (Ratio.of_tez tez) (Ratio.of_nat lqt_to_burn)) (Ratio.of_nat lqt)) in
       let min_kit_withdrawn = Kit.of_ratio_floor (Ratio.div (Ratio.mul (Kit.to_ratio kit) (Ratio.of_nat lqt_to_burn)) (Ratio.of_nat lqt)) in

       (* NOTE: We cannot just factor down the number of liquidity tokens
        * extant for this operation. When we remove liquidity we round the
        * amounts of kit and tez to return towards zero; they might end up
        * being zero because of this, which would make remove_liquidity fail.
        * We make the generator thus ensure that at least 1mukit and 1mutez
        * will be returned. *)
       let lqt_burned, min_tez_withdrawn, min_kit_withdrawn =
         if lqt_to_burn = Ligo.nat_from_literal 0 || min_tez_withdrawn = (Ligo.tez_from_mutez_literal 0) || min_kit_withdrawn = Kit.zero then
           let lqt_to_burn =
             let least_kit_percentage = (Ratio.div (Kit.to_ratio (Kit.of_mukit (Ligo.int_from_literal 1))) (Kit.to_ratio kit)) in
             let least_tez_percentage = Ratio.make (Common.tez_to_mutez (Ligo.tez_from_mutez_literal 1)) (Common.tez_to_mutez tez) in
             let as_q = (Ratio.mul (Ratio.of_nat lqt) (Ratio.max least_kit_percentage least_tez_percentage)) in
             Option.get (Ligo.is_nat (Ligo.cdiv_int_int (Ratio.num as_q) (Ratio.den as_q))) in
           let lqt_burned = Uniswap.issue_liquidity_tokens ~tezos lqt_to_burn in
           let min_tez_withdrawn = Ratio.to_tez_floor (Ratio.div (Ratio.mul (Ratio.of_tez tez) (Ratio.of_nat lqt_to_burn)) (Ratio.of_nat lqt)) in
           let min_kit_withdrawn = Kit.of_ratio_floor (Ratio.div (Ratio.mul (Kit.to_ratio kit) (Ratio.of_nat lqt_to_burn)) (Ratio.of_nat lqt)) in
           (lqt_burned, min_tez_withdrawn, min_kit_withdrawn)
         else
           lqt_burned, min_tez_withdrawn, min_kit_withdrawn in

       let deadline = Ligo.add_timestamp_int tezos.now (Ligo.int_from_literal 1) in (* always one second later *)
       (uniswap, tezos, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline)
    )
    (QCheck.pair (arbitrary_non_empty_uniswap Ratio.one tezos.level) QCheck.pos_int)

(* TODO: Write down for which inputs are the uniswap functions to succeed and
 * test the corresponding edge cases. *)

(* ************************************************************************* *)
(*                     buy_kit (property-based tests)                        *)
(* ************************************************************************* *)

(* If successful, Uniswap.buy_kit always increases the ratio of
 * total_tez/total_kit, since it adds tez and removes kit. *)
let test_buy_kit_increases_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_increases_price"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (uniswap, amount, min_kit_expected, tezos, deadline) ->
  let _bought_kit, new_uniswap = assert_ok @@
    Uniswap.buy_kit uniswap ~amount ~min_kit_expected ~tezos ~deadline in
  Ratio.gt (Uniswap.kit_in_tez new_uniswap) (Uniswap.kit_in_tez uniswap)

(* If successful, Uniswap.buy_kit always increases the product
 * total_tez * total_kit, because of the fees. *)
let test_buy_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_increases_product"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (uniswap, amount, min_kit_expected, tezos, deadline) ->
  let _bought_kit, new_uniswap = assert_ok @@
    Uniswap.buy_kit uniswap ~amount ~min_kit_expected ~tezos ~deadline in
  Ratio.gt (Uniswap.kit_times_tez new_uniswap) (Uniswap.kit_times_tez uniswap)

(* Successful or not, Uniswap.buy_kit should never affect the number of
 * liquidity tokens extant. *)
let test_buy_kit_does_not_affect_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_buy_kit_does_not_affect_liquidity"
    ~count:property_test_count
    make_inputs_for_buy_kit_to_succeed
  @@ fun (uniswap, amount, min_kit_expected, tezos, deadline) ->
  let _bought_kit, new_uniswap = assert_ok @@
    Uniswap.buy_kit uniswap ~amount ~min_kit_expected ~tezos ~deadline in
  Uniswap.liquidity_tokens_extant new_uniswap = Uniswap.liquidity_tokens_extant uniswap

(* ************************************************************************* *)
(*                          buy_kit (unit tests)                             *)
(* ************************************************************************* *)

let buy_kit_unit_test =
  "buy kit unit test" >:: fun _ ->
    let uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Ligo.tez_from_mutez_literal 10_000_000)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 5_000_000)))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 1))
        ~kit_in_tez_in_prev_block:Ratio.one
        ~last_level:level0
    in

    let expected_returned_kit = Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 453_636)) in
    let expected_updated_uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Ligo.tez_from_mutez_literal 11_000_000)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 4_546_364)))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 1))
        ~kit_in_tez_in_prev_block:(Ratio.of_int 2)
        ~last_level:level1
    in

    (* Low expectations and on time (lax): pass *)
    let returned_kit, updated_uniswap = assert_ok @@
      Uniswap.buy_kit
        uniswap
        ~amount:(Ligo.tez_from_mutez_literal 1_000_000)
        ~min_kit_expected:(Kit.of_mukit (Ligo.int_from_literal 1))
        ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
        ~deadline:(Ligo.timestamp_from_seconds_literal 10) in
    assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* Low expectations and on time (tight): pass *)
    let returned_kit, updated_uniswap = assert_ok @@
      Uniswap.buy_kit
        uniswap
        ~amount:(Ligo.tez_from_mutez_literal 1_000_000)
        ~min_kit_expected:(Kit.of_mukit (Ligo.int_from_literal 453_636))
        ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
        ~deadline:(Ligo.timestamp_from_seconds_literal 1) in
    assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* High expectations but on time (tight): fail *)
    assert_failwith Uniswap.BuyKitPriceFailure @@
    Uniswap.buy_kit
      uniswap
      ~amount:(Ligo.tez_from_mutez_literal 1_000_000)
      ~min_kit_expected:(Kit.of_mukit (Ligo.int_from_literal 453_637))
      ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
      ~deadline:(Ligo.timestamp_from_seconds_literal 1);

    (* Low expectations but too late (tight): fail *)
    assert_failwith Uniswap.UniswapTooLate @@
    Uniswap.buy_kit
      uniswap
      ~amount:(Ligo.tez_from_mutez_literal 1_000_000)
      ~min_kit_expected:(Kit.of_mukit (Ligo.int_from_literal 453_636))
      ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 1; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
      ~deadline:(Ligo.timestamp_from_seconds_literal 1)

(* ************************************************************************* *)
(*                     sell_kit (property-based tests)                       *)
(* ************************************************************************* *)

(* If successful, Uniswap.sell_kit always decreases the ratio of
 * total_tez/total_kit, since it removes tez and adds kit. *)
let test_sell_kit_decreases_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_decreases_price"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (uniswap, amount, token, min_tez_expected, tezos, deadline) ->
  let _bought_tez, new_uniswap = assert_ok @@
    Uniswap.sell_kit uniswap ~amount token ~min_tez_expected ~tezos ~deadline in
  Ratio.lt (Uniswap.kit_in_tez new_uniswap) (Uniswap.kit_in_tez uniswap)

(* If successful, Uniswap.sell_kit always increases the product
 * total_tez * total_kit, because of the fees. *)
let test_sell_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_increases_product"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (uniswap, amount, token, min_tez_expected, tezos, deadline) ->
  let _bought_tez, new_uniswap = assert_ok @@
    Uniswap.sell_kit uniswap ~amount token ~min_tez_expected ~tezos ~deadline in
  Ratio.gt (Uniswap.kit_times_tez new_uniswap) (Uniswap.kit_times_tez uniswap)

(* Successful or not, Uniswap.sell_kit should never affect the number of
 * liquidity tokens extant. *)
let test_sell_kit_does_not_affect_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_does_not_affect_liquidity"
    ~count:property_test_count
    make_inputs_for_sell_kit_to_succeed
  @@ fun (uniswap, amount, token, min_tez_expected, tezos, deadline) ->
  let _bought_tez, new_uniswap = assert_ok @@
    Uniswap.sell_kit uniswap ~amount token ~min_tez_expected ~tezos ~deadline in
  Uniswap.liquidity_tokens_extant new_uniswap = Uniswap.liquidity_tokens_extant uniswap

(* ************************************************************************* *)
(*                          sell_kit (unit tests)                            *)
(* ************************************************************************* *)

let sell_kit_unit_test =
  "sell kit" >:: fun _ ->
    let uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Ligo.tez_from_mutez_literal 10_000_000)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 5_000_000)))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 1))
        ~kit_in_tez_in_prev_block:Ratio.one
        ~last_level:level0
    in
    let expected_returned_tez = Ligo.tez_from_mutez_literal 1_663_333 in
    let expected_updated_uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Ligo.tez_from_mutez_literal 8_336_667)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 6_000_000)))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 1))
        ~kit_in_tez_in_prev_block:(Ratio.of_int 2)
        ~last_level:level1
    in

    (* Low expectations and on time (lax): pass *)
    let returned_tez, updated_uniswap = assert_ok @@
      Uniswap.sell_kit
        uniswap
        ~amount:(Ligo.tez_from_mutez_literal 0)
        (Kit.issue ~tezos:tezos0 Kit.one)
        ~min_tez_expected:(Ligo.tez_from_mutez_literal 1)
        ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
        ~deadline:(Ligo.timestamp_from_seconds_literal 10) in
    assert_equal ~printer:Ligo.string_of_tez expected_returned_tez returned_tez;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* Low expectations and on time (tight): pass *)
    let returned_tez, updated_uniswap = assert_ok @@
      Uniswap.sell_kit
        uniswap
        ~amount:(Ligo.tez_from_mutez_literal 0)
        (Kit.issue ~tezos:tezos0 Kit.one)
        ~min_tez_expected:(Ligo.tez_from_mutez_literal 1_663_333)
        ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
        ~deadline:(Ligo.timestamp_from_seconds_literal 1) in
    assert_equal ~printer:Ligo.string_of_tez expected_returned_tez returned_tez;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* High expectations but on time (tight): fail *)
    assert_failwith Uniswap.SellKitPriceFailure @@
    Uniswap.sell_kit
      uniswap
      ~amount:(Ligo.tez_from_mutez_literal 0)
      (Kit.issue ~tezos:tezos0 Kit.one)
      ~min_tez_expected:(Ligo.tez_from_mutez_literal 1_663_334)
      ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
      ~deadline:(Ligo.timestamp_from_seconds_literal 1);

    (* Low expectations but too late (tight): fail *)
    assert_failwith Uniswap.UniswapTooLate @@
    Uniswap.sell_kit
      uniswap
      ~amount:(Ligo.tez_from_mutez_literal 0)
      (Kit.issue ~tezos:tezos0 Kit.one)
      ~min_tez_expected:(Ligo.tez_from_mutez_literal 1_663_333)
      ~tezos:Tezos.{now = Ligo.timestamp_from_seconds_literal 1; level = level1; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;}
      ~deadline:(Ligo.timestamp_from_seconds_literal 1)

(* ************************************************************************* *)
(*             add_liquidity (non-first) (property-based tests)              *)
(* ************************************************************************* *)

(* If successful, Uniswap.add_liquidity never increases the ratio of
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
  @@ fun (uniswap, tezos, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_uniswap = assert_ok @@
    Uniswap.add_liquidity uniswap ~tezos ~amount ~pending_accrual ~max_kit_deposited ~min_lqt_minted ~deadline in
  Ratio.leq (Uniswap.kit_in_tez new_uniswap) (Uniswap.kit_in_tez uniswap)

(* If successful, Uniswap.add_liquidity always increases the product
 * total_tez * total_kit, because we add both tez and kit. *)
let test_add_liquidity_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_product"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, tezos, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_uniswap = assert_ok @@
    Uniswap.add_liquidity uniswap ~tezos ~amount ~pending_accrual ~max_kit_deposited ~min_lqt_minted ~deadline in
  Ratio.gt (Uniswap.kit_times_tez new_uniswap) (Uniswap.kit_times_tez uniswap)

(* If successful, Uniswap.add_liquidity always increases the liquidity;
 * that's what it's supposed to do. *)
let test_add_liquidity_increases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_liquidity"
    ~count:property_test_count
    make_inputs_for_add_liquidity_to_succeed_no_accrual
  @@ fun (uniswap, tezos, amount, pending_accrual, max_kit_deposited, min_lqt_minted, deadline) ->
  let _bought_liquidity, _bought_kit, new_uniswap = assert_ok @@
    Uniswap.add_liquidity uniswap ~tezos ~amount ~pending_accrual ~max_kit_deposited ~min_lqt_minted ~deadline in
  Uniswap.liquidity_tokens_extant new_uniswap > Uniswap.liquidity_tokens_extant uniswap

(* ************************************************************************* *)
(*                 add_liquidity (non-first) (unit tests)                    *)
(* ************************************************************************* *)

let add_liquidity_unit_test =
  "add liquidity unit test" >:: fun _ ->
    let uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Ligo.tez_from_mutez_literal 8_336_667)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 6_000_000)))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 1))
        ~kit_in_tez_in_prev_block:Ratio.one
        ~last_level:level0
    in
    let expected_returned_liquidity = Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 2) in
    let expected_returned_kit = Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 5_605_758)) in
    let expected_updated_uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Ligo.tez_from_mutez_literal 28_336_667)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 20_394_242)))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 3))
        ~kit_in_tez_in_prev_block:Ratio.one
        ~last_level:level0
    in
    let tezos = Tezos.{now = Ligo.timestamp_from_seconds_literal 0; level = level0; self = tezos0.self; amount = tezos0.amount; sender = tezos0.sender;} in

    let returned_liquidity, returned_kit, updated_uniswap = assert_ok @@
      Uniswap.add_liquidity
        uniswap
        ~tezos
        ~amount:(Ligo.tez_from_mutez_literal 20_000_000)
        ~pending_accrual:(Ligo.tez_from_mutez_literal 0)
        ~max_kit_deposited:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 20_000_000)))
        ~min_lqt_minted:(Ligo.nat_from_literal 2)
        ~deadline:(Ligo.timestamp_from_seconds_literal 1) in
    assert_equal ~printer:Uniswap.show_liquidity expected_returned_liquidity returned_liquidity;
    assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap

(* ************************************************************************* *)
(*                 remove_liquidity (property-based tests)                   *)
(* ************************************************************************* *)

(* If successful, Uniswap.remove_liquidity always decreases the product
 * total_tez * total_kit, because we remove both tez and kit. *)
(* NOTE: That is not entirely true, because when we remove liquidity we round
 * the amounts of kit and tez to return towards zero; they might end up being
 * zero because of this. BUT, in these cases Uniswap.remove_liquidity should
 * thrown an error, so this property is expected to hold indeed, when
 * remove_liquidity succeeds. *)
let test_remove_liquidity_decreases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_product"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, tezos, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_tez, _withdrawn_kit, new_uniswap = assert_ok @@
    Uniswap.remove_liquidity uniswap ~tezos ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline in
  Ratio.leq (Uniswap.kit_times_tez new_uniswap) (Uniswap.kit_times_tez uniswap)

(* If successful, Uniswap.remove_liquidity always decreases the liquidity;
 * that's what it's supposed to do. *)
let test_remove_liquidity_decreases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_liquidity"
    ~count:property_test_count
    make_inputs_for_remove_liquidity_to_succeed
  @@ fun (uniswap, tezos, amount, lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline) ->
  let _withdrawn_tez, _withdrawn_kit, new_uniswap = assert_ok @@
    Uniswap.remove_liquidity uniswap ~tezos ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline in
  Uniswap.liquidity_tokens_extant new_uniswap < Uniswap.liquidity_tokens_extant uniswap

(* ************************************************************************* *)
(*                 liquidity when accruals are pending                       *)
(* ************************************************************************* *)

let pending_tez_deposit_test =
  "set pending tez deposit" >::
  (fun _ ->
     let uniswap =
       Uniswap.make_for_test
         ~tez:(Ligo.tez_from_mutez_literal  1000_000_000)
         ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 5000_000_000)))
         ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 (Ligo.nat_from_literal 1000))
         ~kit_in_tez_in_prev_block:Ratio.one
         ~last_level:level0 in
     (* let uniswap = set_pending_accrued_tez uniswap (Ligo.tez_from_mutez_literal 1_000_000) in *)

     match Uniswap.add_liquidity
             uniswap
             ~tezos:tezos0
             ~amount:(Ligo.tez_from_mutez_literal 101_000_000)
             ~pending_accrual:(Ligo.tez_from_mutez_literal 10_000_000)
             ~max_kit_deposited:(Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 500_000_000)))
             ~min_lqt_minted:(Ligo.nat_from_literal 1)
             ~deadline:(Ligo.timestamp_from_seconds_literal 1)
     with
     | Error _ -> assert_string "adding liquidity failed"
     | Ok (liq, _kit, uniswap) ->
       match
         Uniswap.remove_liquidity uniswap
           ~tezos:tezos0
           ~amount:(Ligo.tez_from_mutez_literal 0)
           ~lqt_burned:liq
           ~min_tez_withdrawn:(Ligo.tez_from_mutez_literal 1)
           ~min_kit_withdrawn:(Kit.of_mukit (Ligo.int_from_literal 1))
           ~deadline:(Ligo.timestamp_from_seconds_literal 100)
       with
       | Error _ -> assert_string "removing liquidity failed"
       | Ok (tez, kit, _) ->
         assert_equal ~printer:Kit.show_token (Kit.issue ~tezos:tezos0 (Kit.of_mukit (Ligo.int_from_literal 500_000_000))) kit;
         assert_equal ~printer:Ligo.string_of_tez (Ligo.tez_from_mutez_literal 100_090_909) tez;
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
    test_add_liquidity_might_decrease_price;
    test_add_liquidity_increases_product;
    test_add_liquidity_increases_liquidity;

    (* remove liquidity *)
    (* TODO: add unit tests *)
    test_remove_liquidity_decreases_product;
    test_remove_liquidity_decreases_liquidity;

    pending_tez_deposit_test;
  ]
