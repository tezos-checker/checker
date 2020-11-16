
open OUnit2

type lq = Uniswap.liquidity [@@deriving show]
type us = Uniswap.t [@@deriving show]
type kt = Kit.t [@@deriving show]
type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

(* It is unfortunate (but reasonable): but we cannot derive show for open
 * types. Turn the result into an option so that we can print it. *)
type buy_kit_res  = (Kit.t * Uniswap.t) option [@@deriving show]
type sell_kit_res = (Tez.t * Uniswap.t) option [@@deriving show]

let suite =
  "Uniswap tests" >::: [
    "buy kit" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         { tez = Tez.of_mutez 10_000_000;
           kit = Kit.of_mukit 5_000_000;
           total_liquidity_tokens = Uniswap.liquidity_of_int 1;
         } in

       let expected_returned_kit = Kit.of_mukit 453_636 in
       let expected_updated_uniswap : Uniswap.t =
         { tez = Tez.of_mutez 11_000_000;
           kit = Kit.of_mukit 4_546_364;
           total_liquidity_tokens = Uniswap.liquidity_of_int 1;
         } in

       (* Low expectations and on time (lax): pass *)
       let result_1 = Result.to_option (Uniswap.buy_kit uniswap ~amount:Tez.one ~min_kit_expected:(Kit.of_mukit 1) ~now:(Timestamp.of_seconds 0) ~deadline:(Timestamp.of_seconds 10)) in
       assert_equal ~printer:show_buy_kit_res (Some (expected_returned_kit, expected_updated_uniswap)) result_1;

       (* Low expectations and on time (tight): pass *)
       let result_2 = Result.to_option (Uniswap.buy_kit uniswap ~amount:Tez.one ~min_kit_expected:(Kit.of_mukit 453_636) ~now:(Timestamp.of_seconds 0) ~deadline:(Timestamp.of_seconds 1)) in
       assert_equal ~printer:show_buy_kit_res (Some (expected_returned_kit, expected_updated_uniswap)) result_2;

       (* High expectations but on time (tight): fail *)
       let result_3 = Result.to_option (Uniswap.buy_kit uniswap ~amount:Tez.one ~min_kit_expected:(Kit.of_mukit 453_637) ~now:(Timestamp.of_seconds 0) ~deadline:(Timestamp.of_seconds 1)) in
       assert_equal ~printer:show_buy_kit_res None result_3;

       (* Low expectations but too late (tight): fail *)
       let result_4 = Result.to_option (Uniswap.buy_kit uniswap ~amount:Tez.one ~min_kit_expected:(Kit.of_mukit 453_636) ~now:(Timestamp.of_seconds 1) ~deadline:(Timestamp.of_seconds 1)) in
       assert_equal ~printer:show_buy_kit_res None result_4;
    );

    "sell kit" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         { tez = Tez.of_mutez 10_000_000;
           kit = Kit.of_mukit 5_000_000;
           total_liquidity_tokens = Uniswap.liquidity_of_int 1;
         } in
       let expected_returned_tez = Tez.of_mutez 1_663_333 in
       let expected_updated_uniswap : Uniswap.t =
         { tez = Tez.of_mutez 8_336_667;
           kit = Kit.of_mukit 6_000_000;
           total_liquidity_tokens = Uniswap.liquidity_of_int 1;
         } in

       (* Low expectations and on time (lax): pass *)
       let result_1 = Result.to_option (Uniswap.sell_kit uniswap ~amount:Tez.zero Kit.one ~min_tez_expected:(Tez.of_mutez 1) ~now:(Timestamp.of_seconds 0) ~deadline:(Timestamp.of_seconds 10)) in
       assert_equal ~printer:show_sell_kit_res (Some (expected_returned_tez, expected_updated_uniswap)) result_1;

       (* Low expectations and on time (tight): pass *)
       let result_2 = Result.to_option (Uniswap.sell_kit uniswap ~amount:Tez.zero Kit.one ~min_tez_expected:(Tez.of_mutez 1_663_333) ~now:(Timestamp.of_seconds 0) ~deadline:(Timestamp.of_seconds 1)) in
       assert_equal ~printer:show_sell_kit_res (Some (expected_returned_tez, expected_updated_uniswap)) result_2;

       (* High expectations but on time (tight): fail *)
       let result_3 = Result.to_option (Uniswap.sell_kit uniswap ~amount:Tez.zero Kit.one ~min_tez_expected:(Tez.of_mutez 1_663_334) ~now:(Timestamp.of_seconds 0) ~deadline:(Timestamp.of_seconds 1)) in
       assert_equal ~printer:show_sell_kit_res None result_3;

       (* Low expectations but too late (tight): fail *)
       let result_4 = Result.to_option (Uniswap.sell_kit uniswap ~amount:Tez.zero Kit.one ~min_tez_expected:(Tez.of_mutez 1_663_333) ~now:(Timestamp.of_seconds 1) ~deadline:(Timestamp.of_seconds 1)) in
       assert_equal ~printer:show_sell_kit_res None result_4;
    );

    "add liquidity" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         { tez = Tez.of_mutez 8_336_667;
           kit = Kit.of_mukit 6_000_000;
           total_liquidity_tokens = Uniswap.liquidity_of_int 1;
         } in
       let expected_returned_liquidity = Uniswap.liquidity_of_int 2 in
       let expected_returned_tez = Tez.zero in
       let expected_returned_kit = Kit.of_mukit 5_605_758 in
       let expected_updated_uniswap : Uniswap.t =
         { tez = Tez.of_mutez 28_336_667;
           kit = Kit.of_mukit 20_394_242;
           total_liquidity_tokens = Uniswap.liquidity_of_int 3;
         } in
       match Uniswap.add_liquidity
               uniswap
               ~amount:(Tez.of_mutez 20_000_000)
               ~max_kit_deposited:(Kit.of_mukit 20_000_000)
               ~min_lqt_minted:(Uniswap.liquidity_of_int 2)
               ~now:(Timestamp.of_seconds 0)
               ~deadline:(Timestamp.of_seconds 1)
       with
       | Error _err -> failwith "buy liquidity: the impossible happened!"
       | Ok (returned_liquidity, returned_tez, returned_kit, updated_uniswap) ->
           assert_equal ~printer:show_lq expected_returned_liquidity returned_liquidity;
           assert_equal ~printer:show_tz expected_returned_tez returned_tez;
           assert_equal ~printer:show_kt expected_returned_kit returned_kit;
           assert_equal ~printer:show_us expected_updated_uniswap updated_uniswap
    );
  ]
