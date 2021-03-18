open Ratio
open Kit
open UniswapTypes

let alice_addr = Ligo.address_from_literal "alice_addr"
let bob_addr = Ligo.address_from_literal "bob_addr"
let charles_key_hash = Ligo.key_hash_from_literal "charles_key_hash"

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

let uniswap_make_for_test ~tez ~kit ~lqt ~kit_in_tez_in_prev_block ~last_level =
  { tez = tez;
    kit = kit;
    lqt = lqt;
    kit_in_tez_in_prev_block = kit_in_tez_in_prev_block;
    last_level = last_level;
  }

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
