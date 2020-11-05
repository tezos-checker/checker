
open Uniswap
open FixedPoint
open Tez
open Kit
open OUnit2

type lq = Uniswap.liquidity [@@deriving show]
type us = Uniswap.t [@@deriving show]
type kt = Kit.t [@@deriving show]
type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "Uniswap tests" >::: [
    "sell kit" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         { tez = Tez.of_string "10.0";
           kit = Kit.of_string "5.0";
           total_liquidity_tokens = 1;
         } in
       let expected_returned_tez = Tez.of_string "1.663333" in
       let expected_returned_kit = Kit.of_string "0.000000" in
       let expected_updated_uniswap : Uniswap.t =
         { tez = Tez.of_string "8.336667";
           kit = Kit.of_string "6.000000";
           total_liquidity_tokens = 1;
         } in
       let returned_tez, returned_kit, updated_uniswap = Uniswap.sell_kit uniswap (Kit.of_string "1.0") in
       assert_equal ~printer:show_tz expected_returned_tez returned_tez;
       assert_equal ~printer:show_kt expected_returned_kit returned_kit;
       assert_equal ~printer:show_us expected_updated_uniswap updated_uniswap
    );

    "buy liquidity" >::
    (fun _ ->
       let uniswap : Uniswap.t =
         { tez = Tez.of_string "8.336667";
           kit = Kit.of_string "6.000000";
           total_liquidity_tokens = 1;
         } in
       let expected_returned_liquidity = Uniswap.liquidity_of_int 2 in
       let expected_returned_tez = Tez.of_string "0.000000" in
       let expected_returned_kit = Kit.of_string "5.605759" in
       let expected_updated_uniswap : Uniswap.t =
         { tez = Tez.of_string "28.336667";
           kit = Kit.of_string "20.394241";
           total_liquidity_tokens = 3;
         } in
       let returned_liquidity, returned_tez, returned_kit, updated_uniswap =
         Uniswap.buy_liquidity uniswap (Tez.of_string "20.0") (Kit.of_string "20.0") in
       assert_equal ~printer:show_lq expected_returned_liquidity returned_liquidity;
       assert_equal ~printer:show_tz expected_returned_tez returned_tez;
       assert_equal ~printer:show_kt expected_returned_kit returned_kit;
       assert_equal ~printer:show_us expected_updated_uniswap updated_uniswap
    );
  ]
