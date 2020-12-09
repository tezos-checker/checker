open OUnit2
open TestCommon

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let level0 = Level.of_int 0
let level1 = Level.of_int 1
let checker_address = Address.of_string "checker"
let tezos0 = Tezos.{now = Timestamp.of_seconds 0; level = level0; self = checker_address;}

(* Issue an arbitrary amount of kit (checker-issued) *)
let arb_positive_kit_token = QCheck.map (Kit.issue ~tezos:tezos0) TestArbitrary.arb_positive_kit

(* Issue an arbitrary number of liquidity tokens (checker-issued) *)
let arb_liquidity = QCheck.map (Uniswap.issue_liquidity_tokens ~tezos:tezos0) QCheck.(0 -- max_int)

(* Create an arbitrary state for the uniswap contract (NB: some values are fixed). *)
let arbitrary_uniswap =
  QCheck.map
    (fun (tez, kit, lqt) ->
       (tez, kit, lqt, Uniswap.make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block:Q.one ~last_level:level0)
    )
    (QCheck.triple TestArbitrary.arb_positive_tez arb_positive_kit_token arb_liquidity)

(* amount > uniswap_tez * (1 - fee) / fee *)
(* 1mukit < min_kit_expected < FLOOR{amount * (uniswap_kit / (uniswap_tez + amount)) * FACTOR} *)
let make_inputs_for_buy_kit_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _kit being ignored. *)
    (fun (tez, _kit, _lqt, uniswap) ->
       let amount = Tez.of_q_ceil Q.(Tez.to_q tez * (one - Constants.uniswap_fee) / Constants.uniswap_fee) in
       let min_kit_expected = Kit.of_mukit 1 in (* absolute minimum *)
       (uniswap, amount, min_kit_expected, tezos0, Timestamp.of_seconds 1)
    )
    arbitrary_uniswap

(* Some more expected properties to check:
   - Uniswap.buy_kit should not affect the number of liquidity tokens.
   - Uniswap.sell_kit should not affect the number of liquidity tokens.
   - TODO: Write down for which inputs are the uniswap functions to succeed.
*)

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
  Q.(Uniswap.kit_in_tez new_uniswap > Uniswap.kit_in_tez uniswap)

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
  Q.(Uniswap.kit_times_tez new_uniswap > Uniswap.kit_times_tez uniswap)

(* ************************************************************************* *)
(*                          buy_kit (unit tests)                             *)
(* ************************************************************************* *)

let buy_kit_unit_test =
  "buy kit unit test" >:: fun _ ->
    let uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Tez.of_mutez 10_000_000)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 5_000_000))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
        ~kit_in_tez_in_prev_block:Q.one
        ~last_level:level0
    in

    let expected_returned_kit = Kit.issue ~tezos:tezos0 (Kit.of_mukit 453_636) in
    let expected_updated_uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Tez.of_mutez 11_000_000)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 4_546_364))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
        ~kit_in_tez_in_prev_block:(Q.of_int 2)
        ~last_level:level1
    in

    (* Low expectations and on time (lax): pass *)
    let returned_kit, updated_uniswap = assert_ok @@
      Uniswap.buy_kit
        uniswap
        ~amount:Tez.one
        ~min_kit_expected:(Kit.of_mukit 1)
        ~tezos:Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;}
        ~deadline:(Timestamp.of_seconds 10) in
    assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* Low expectations and on time (tight): pass *)
    let returned_kit, updated_uniswap = assert_ok @@
      Uniswap.buy_kit
        uniswap
        ~amount:Tez.one
        ~min_kit_expected:(Kit.of_mukit 453_636)
        ~tezos:Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;}
        ~deadline:(Timestamp.of_seconds 1) in
    assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* High expectations but on time (tight): fail *)
    assert_failwith Uniswap.BuyKitPriceFailure @@
    Uniswap.buy_kit
      uniswap
      ~amount:Tez.one
      ~min_kit_expected:(Kit.of_mukit 453_637)
      ~tezos:Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;}
      ~deadline:(Timestamp.of_seconds 1);

    (* Low expectations but too late (tight): fail *)
    assert_failwith Uniswap.UniswapTooLate @@
    Uniswap.buy_kit
      uniswap
      ~amount:Tez.one
      ~min_kit_expected:(Kit.of_mukit 453_636)
      ~tezos:Tezos.{now = Timestamp.of_seconds 1; level = level1; self = checker_address;}
      ~deadline:(Timestamp.of_seconds 1)

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
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* If successful, Uniswap.sell_kit always increases the product
 * total_tez * total_kit, because of the fees. *)
let test_sell_kit_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_sell_kit_increases_product"
    ~count:property_test_count
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* ************************************************************************* *)
(*                          sell_kit (unit tests)                            *)
(* ************************************************************************* *)

let sell_kit_unit_test =
  "sell kit" >:: fun _ ->
    let uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Tez.of_mutez 10_000_000)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 5_000_000))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
        ~kit_in_tez_in_prev_block:Q.one
        ~last_level:level0
    in
    let expected_returned_tez = Tez.of_mutez 1_663_333 in
    let expected_updated_uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Tez.of_mutez 8_336_667)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 6_000_000))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
        ~kit_in_tez_in_prev_block:(Q.of_int 2)
        ~last_level:level1
    in

    (* Low expectations and on time (lax): pass *)
    let returned_tez, updated_uniswap = assert_ok @@
      Uniswap.sell_kit
        uniswap
        ~amount:Tez.zero
        (Kit.issue ~tezos:tezos0 Kit.one)
        ~min_tez_expected:(Tez.of_mutez 1)
        ~tezos:Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;}
        ~deadline:(Timestamp.of_seconds 10) in
    assert_equal ~printer:Tez.show expected_returned_tez returned_tez;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* Low expectations and on time (tight): pass *)
    let returned_tez, updated_uniswap = assert_ok @@
      Uniswap.sell_kit
        uniswap
        ~amount:Tez.zero
        (Kit.issue ~tezos:tezos0 Kit.one)
        ~min_tez_expected:(Tez.of_mutez 1_663_333)
        ~tezos:Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;}
        ~deadline:(Timestamp.of_seconds 1) in
    assert_equal ~printer:Tez.show expected_returned_tez returned_tez;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap;

    (* High expectations but on time (tight): fail *)
    assert_failwith Uniswap.SellKitPriceFailure @@
    Uniswap.sell_kit
      uniswap
      ~amount:Tez.zero
      (Kit.issue ~tezos:tezos0 Kit.one)
      ~min_tez_expected:(Tez.of_mutez 1_663_334)
      ~tezos:Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;}
      ~deadline:(Timestamp.of_seconds 1);

    (* Low expectations but too late (tight): fail *)
    assert_failwith Uniswap.UniswapTooLate @@
    Uniswap.sell_kit
      uniswap
      ~amount:Tez.zero
      (Kit.issue ~tezos:tezos0 Kit.one)
      ~min_tez_expected:(Tez.of_mutez 1_663_333)
      ~tezos:Tezos.{now = Timestamp.of_seconds 1; level = level1; self = checker_address;}
      ~deadline:(Timestamp.of_seconds 1)

(* ************************************************************************* *)
(*             add_liquidity (non-first) (property-based tests)              *)
(* ************************************************************************* *)

(* If successful, Uniswap.add_liquidity never increases the ratio of
 * total_tez/total_kit (might leave it where it is or decrease it), since it
 * always rounds up the kit it keeps in the contract. *)
let test_add_liquidity_might_decrease_price =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_might_decrease_price"
    ~count:property_test_count
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* If successful, Uniswap.add_liquidity always increases the product
 * total_tez * total_kit, because we add both tez and kit. *)
let test_add_liquidity_increases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_product"
    ~count:property_test_count
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* If successful, Uniswap.add_liquidity always increases the liquidity;
 * that's what it's supposed to do. *)
let test_add_liquidity_increases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_add_liquidity_increases_liquidity"
    ~count:property_test_count
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* ************************************************************************* *)
(*                 add_liquidity (non-first) (unit tests)                    *)
(* ************************************************************************* *)

let add_liquidity_unit_test =
  "add liquidity unit test" >:: fun _ ->
    let uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Tez.of_mutez 8_336_667)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 6_000_000))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
        ~kit_in_tez_in_prev_block:Q.one
        ~last_level:level0
    in
    let expected_returned_liquidity = Uniswap.issue_liquidity_tokens ~tezos:tezos0 2 in
    let expected_returned_tez = Tez.zero in
    let expected_returned_kit = Kit.issue ~tezos:tezos0 (Kit.of_mukit 5_605_758) in
    let expected_updated_uniswap : Uniswap.t =
      Uniswap.make_for_test
        ~tez:(Tez.of_mutez 28_336_667)
        ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 20_394_242))
        ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 3)
        ~kit_in_tez_in_prev_block:Q.one
        ~last_level:level0
    in
    let tezos = Tezos.{now = Timestamp.of_seconds 0; level = level0; self = checker_address;} in

    let returned_liquidity, returned_tez, returned_kit, updated_uniswap = assert_ok @@
      Uniswap.add_liquidity
        uniswap
        ~tezos
        ~amount:(Tez.of_mutez 20_000_000)
        ~pending_accrual:Tez.zero
        ~max_kit_deposited:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 20_000_000))
        ~min_lqt_minted:2
        ~deadline:(Timestamp.of_seconds 1) in
    assert_equal ~printer:Uniswap.show_liquidity expected_returned_liquidity returned_liquidity;
    assert_equal ~printer:Tez.show expected_returned_tez returned_tez;
    assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
    assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap

(* ************************************************************************* *)
(*                 remove_liquidity (property-based tests)                   *)
(* ************************************************************************* *)

(* If successful, Uniswap.remove_liquidity always decreases the product
 * total_tez * total_kit, because we remove both tez and kit. *)
let test_remove_liquidity_decreases_product =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_product"
    ~count:property_test_count
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* If successful, Uniswap.remove_liquidity always decreases the liquidity;
 * that's what it's supposed to do. *)
let test_remove_liquidity_decreases_liquidity =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_remove_liquidity_decreases_liquidity"
    ~count:property_test_count
    QCheck.unit
  @@ fun () ->
  true (* TODO *)

(* ************************************************************************* *)
(*                 liquidity when accruals are pending                       *)
(* ************************************************************************* *)

let pending_tez_deposit_test =
  "set pending tez deposit" >::
  (fun _ ->
     let uniswap =
       Uniswap.make_for_test
         ~tez:(Tez.of_mutez 1000_000_000)
         ~kit:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 5000_000_000))
         ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1000)
         ~kit_in_tez_in_prev_block:Q.one
         ~last_level:level0 in
     (* let uniswap = set_pending_accrued_tez uniswap (Tez.of_mutez 1_000_000) in *)

     match Uniswap.add_liquidity
             uniswap
             ~tezos:tezos0
             ~amount:(Tez.of_mutez 101_000_000)
             ~pending_accrual:(Tez.of_mutez 10_000_000)
             ~max_kit_deposited:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 500_000_000))
             ~min_lqt_minted:1
             ~deadline:(Timestamp.of_seconds 1)
     with
     | Error _ -> assert_string "adding liquidity failed"
     | Ok (liq, _tez, _kit, uniswap) ->
       match Uniswap.remove_liquidity uniswap ~tezos:tezos0 ~amount:Tez.zero ~lqt_burned:liq ~min_tez_withdrawn:Tez.zero ~min_kit_withdrawn:Kit.zero ~deadline:(Timestamp.of_seconds 100) with
       | Error _ -> assert_string "removing liquidity failed"
       | Ok (tez, kit, _) -> 
         assert_equal ~printer:Kit.show_token (Kit.issue ~tezos:tezos0 (Kit.of_mukit 500_000_000)) kit;
         assert_equal ~printer:Tez.show (Tez.of_mutez 100_090_909) tez;
  )

let suite =
  "Uniswap tests" >::: [
    (* buy_kit *)
    buy_kit_unit_test;
    test_buy_kit_increases_price;
    test_buy_kit_increases_product;

    (* sell_kit *)
    sell_kit_unit_test;
    test_sell_kit_decreases_price;
    test_sell_kit_increases_product;

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
