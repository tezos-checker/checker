
open OUnit2

(* It is unfortunate (but reasonable): but we cannot derive show for open
 * types. Turn the result into an option so that we can print it. *)
type buy_kit_res  = (Kit.t * Uniswap.t) option [@@deriving show]
type sell_kit_res = (Tez.t * Uniswap.t) option [@@deriving show]

let level0 = Level.of_int 0
let level1 = Level.of_int 1

let checker_address = Address.of_string "checker"

let tezos0 = Tezos.{now = Timestamp.of_seconds 0; level = level0; self = checker_address;}

let suite =
  "Uniswap tests" >::: [
    "buy kit" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         Uniswap.make_for_test
           ~tez:(Tez.of_mutez 10_000_000)
           ~kit:(Kit.of_mukit 5_000_000)
           ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
           ~kit_in_tez_in_prev_block:Q.one
           ~last_level:level0
       in

       let expected_returned_kit = Kit.of_mukit 453_636 in
       let expected_updated_uniswap : Uniswap.t =
         Uniswap.make_for_test
           ~tez:(Tez.of_mutez 11_000_000)
           ~kit:(Kit.of_mukit 4_546_364)
           ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
           ~kit_in_tez_in_prev_block:(Q.of_int 2)
           ~last_level:level1
       in

       (* Low expectations and on time (lax): pass *)
       let tezos1 = Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;} in
       let result_1 =
         Result.to_option
         @@ Uniswap.buy_kit
           uniswap
           ~amount:Tez.one
           ~min_kit_expected:(Kit.of_mukit 1)
           ~tezos:tezos1
           ~deadline:(Timestamp.of_seconds 10) in
       assert_equal ~printer:show_buy_kit_res (Some (expected_returned_kit, expected_updated_uniswap)) result_1;

       (* Low expectations and on time (tight): pass *)
       let tezos2 = Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;} in
       let result_2 =
         Result.to_option
         @@ Uniswap.buy_kit
           uniswap
           ~amount:Tez.one
           ~min_kit_expected:(Kit.of_mukit 453_636)
           ~tezos:tezos2
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:show_buy_kit_res (Some (expected_returned_kit, expected_updated_uniswap)) result_2;

       (* High expectations but on time (tight): fail *)
       let tezos3 = Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;} in
       let result_3 =
         Result.to_option
         @@ Uniswap.buy_kit
           uniswap
           ~amount:Tez.one
           ~min_kit_expected:(Kit.of_mukit 453_637)
           ~tezos:tezos3
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:show_buy_kit_res None result_3;

       (* Low expectations but too late (tight): fail *)
       let tezos4 = Tezos.{now = Timestamp.of_seconds 1; level = level1; self = checker_address;} in
       let result_4 =
         Result.to_option
         @@ Uniswap.buy_kit
           uniswap
           ~amount:Tez.one
           ~min_kit_expected:(Kit.of_mukit 453_636)
           ~tezos:tezos4
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:show_buy_kit_res None result_4;
    );

    "sell kit" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         Uniswap.make_for_test
           ~tez:(Tez.of_mutez 10_000_000)
           ~kit:(Kit.of_mukit 5_000_000)
           ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
           ~kit_in_tez_in_prev_block:Q.one
           ~last_level:level0
       in
       let expected_returned_tez = Tez.of_mutez 1_663_333 in
       let expected_updated_uniswap : Uniswap.t =
         Uniswap.make_for_test
           ~tez:(Tez.of_mutez 8_336_667)
           ~kit:(Kit.of_mukit 6_000_000)
           ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
           ~kit_in_tez_in_prev_block:(Q.of_int 2)
           ~last_level:level1
       in

       (* Low expectations and on time (lax): pass *)
       let tezos1 = Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;} in
       let result_1 =
         Result.to_option
         @@ Uniswap.sell_kit
           uniswap
           ~amount:Tez.zero
           Kit.one
           ~min_tez_expected:(Tez.of_mutez 1)
           ~tezos:tezos1
           ~deadline:(Timestamp.of_seconds 10) in
       assert_equal ~printer:show_sell_kit_res (Some (expected_returned_tez, expected_updated_uniswap)) result_1;

       (* Low expectations and on time (tight): pass *)
       let tezos2 = Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;} in
       let result_2 =
         Result.to_option
         @@ Uniswap.sell_kit
           uniswap
           ~amount:Tez.zero
           Kit.one
           ~min_tez_expected:(Tez.of_mutez 1_663_333)
           ~tezos:tezos2
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:show_sell_kit_res (Some (expected_returned_tez, expected_updated_uniswap)) result_2;

       (* High expectations but on time (tight): fail *)
       let tezos3 = Tezos.{now = Timestamp.of_seconds 0; level = level1; self = checker_address;} in
       let result_3 =
         Result.to_option
         @@ Uniswap.sell_kit
           uniswap
           ~amount:Tez.zero
           Kit.one
           ~min_tez_expected:(Tez.of_mutez 1_663_334)
           ~tezos:tezos3
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:show_sell_kit_res None result_3;

       (* Low expectations but too late (tight): fail *)
       let tezos4 = Tezos.{now = Timestamp.of_seconds 1; level = level1; self = checker_address;} in
       let result_4 =
         Result.to_option
         @@ Uniswap.sell_kit
           uniswap
           ~amount:Tez.zero
           Kit.one
           ~min_tez_expected:(Tez.of_mutez 1_663_333)
           ~tezos:tezos4
           ~deadline:(Timestamp.of_seconds 1) in
       assert_equal ~printer:show_sell_kit_res None result_4;
    );

    "add liquidity" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         Uniswap.make_for_test
           ~tez:(Tez.of_mutez 8_336_667)
           ~kit:(Kit.of_mukit 6_000_000)
           ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 1)
           ~kit_in_tez_in_prev_block:Q.one
           ~last_level:level0
       in
       let expected_returned_liquidity = Uniswap.issue_liquidity_tokens ~tezos:tezos0 2 in
       let expected_returned_tez = Tez.zero in
       let expected_returned_kit = Kit.of_mukit 5_605_758 in
       let expected_updated_uniswap : Uniswap.t =
         Uniswap.make_for_test
           ~tez:(Tez.of_mutez 28_336_667)
           ~kit:(Kit.of_mukit 20_394_242)
           ~lqt:(Uniswap.issue_liquidity_tokens ~tezos:tezos0 3)
           ~kit_in_tez_in_prev_block:Q.one
           ~last_level:level0
       in
       let tezos = Tezos.{now = Timestamp.of_seconds 0; level = level0; self = checker_address;} in
       match Uniswap.add_liquidity
               uniswap
               ~tezos
               ~amount:(Tez.of_mutez 20_000_000)
               ~max_kit_deposited:(Kit.of_mukit 20_000_000)
               ~min_lqt_minted:2
               ~deadline:(Timestamp.of_seconds 1)
       with
       | Error _err -> failwith "buy liquidity: the impossible happened!"
       | Ok (returned_liquidity, returned_tez, returned_kit, updated_uniswap) ->
         assert_equal ~printer:Uniswap.show_liquidity expected_returned_liquidity returned_liquidity;
         assert_equal ~printer:Tez.show expected_returned_tez returned_tez;
         assert_equal ~printer:Kit.show expected_returned_kit returned_kit;
         assert_equal ~printer:Uniswap.show expected_updated_uniswap updated_uniswap
    );
  ]
