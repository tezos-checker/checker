open OUnit2
open TestCommon

let level0 = Level.of_int 0
let level1 = Level.of_int 1

let checker_address = Address.of_string "checker"

let tezos0 = Tezos.{now = Timestamp.of_seconds 0; level = level0; self = checker_address;}

(* Expected properties to check:

Uniswap.buy_kit:
- tez_out / kit_out > tez_in / kit_in: because we add tez and remove kit
- tez_out * kit_out > tez_in * kit_in: because of the fees, the product is not preserved, but increases.
- liquidity should remain as is

Uniswap.sell_kit:
- tez_out / kit_out < tez_in / kit_in: because we add kit and remove tez
- tez_out * kit_out > tez_in * kit_in: because of the fees, the product is not preserved, but increases.
- liquidity should remain as is

Uniswap.add_liquidity (non-first provider):
- tez_out / kit_out <= tez_in / kit_in: long story short: because we round up the kit we keep
- tez_out * kit_out >  tez_in * kit_in: because we add both kit and tez
- liquidity_out > liquidity_in: because we add liquidity

Uniswap.remove_liquidity:
- tez_out / kit_out ?? tez_in / kit_in: NO IDEA. FOR BOTH THE CONTRACT ROUNDS UP WHAT IT KEEPS.
- tez_out * kit_out <  tez_in * kit_in: because we remove both kit and tez
- liquidity_out < liquidity_in: because we remove liquidity
*)

let suite =
  "Uniswap tests" >::: [
    "buy kit" >::
    (fun _ ->
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
           ~deadline:(Timestamp.of_seconds 1);
    );

    "sell kit" >::
    (fun _ ->
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
           ~deadline:(Timestamp.of_seconds 1);
    );

    "add liquidity" >::
    (fun _ ->
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
           ~max_kit_deposited:(Kit.issue ~tezos:tezos0 (Kit.of_mukit 20_000_000))
           ~min_lqt_minted:2
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:Uniswap.show_liquidity expected_returned_liquidity returned_liquidity;
       assert_equal ~printer:Tez.show expected_returned_tez returned_tez;
       assert_equal ~printer:Kit.show_token expected_returned_kit returned_kit;
       assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap
    );
  ]
