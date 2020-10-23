open FixedPoint
open Format
open Huxian
open Kit
open Tez

let burrow_experiment () =
  (* OTHER EXAMPLES *)
  (* Unwarranted liquidation for *)
  (* let initial_burrow = { minted_kit = Kit.of_float 10.0; collateral = Tez.of_float 10.0; } in *)
  (* Partial liquidation for *)
  (* let initial_burrow = { minted_kit = Kit.of_float 20.0; collateral = Tez.of_float 10.0; } in *)
  (* Complete liquidation (deplete the collateral, but keep the burrow) for *)
  (* let initial_burrow = { minted_kit = Kit.of_float 100.0; collateral = Tez.of_float 10.0; } in *)
  (* Complete liquidation (close the burrow) for *)
  (* let initial_burrow = { minted_kit = Kit.of_float 100.0; collateral = Tez.of_float 1.001; } in *)
  (* DEFAULT *)
  let initial_burrow = { minted_kit = Kit.of_float 20.0; collateral = Tez.of_float 10.0; } in
  printf "\n=== Initial burrow state ===\n";
  print_string @@ show_burrow initial_burrow;
  let params =
    { q = FixedPoint.of_float 1.015;
      index = Tez.of_float 0.32;
      protected_index = Tez.of_float 0.36;
      target = FixedPoint.of_float 1.08;
      drift = FixedPoint.of_float 0.0;
      drift' = FixedPoint.of_float 0.0;
    } in
  printf "\n=== Checker parameters ===\n";
  print_string @@ show_checker_parameters params;

  printf "\n=== State of affairs ===\n";
  printf "Overburrowed          : %B\n" (is_overburrowed params initial_burrow);
  printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params initial_burrow);
  printf "\n=== Liquidation request outcome ===\n";
  let liquidation_result = request_liquidation params initial_burrow in
  print_liquidation_result liquidation_result;

  printf "\n=== State of affairs ===\n";
  match liquidation_result with
  | (outcome,_,_,_,b) ->
    if outcome = Close then
      printf "There is no burrow left to consider.\n"
    else
      printf "Overburrowed          : %B\n" (is_overburrowed params b);
    printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params b)

let uniswap_experiment () =
  let uniswap =
    { tez = Tez.of_float 10.0;
      kit = Kit.of_float 5.0;
      total_liquidity_tokens = 1;
    } in
  let (tez, kit, uniswap) = sell_kit uniswap (Kit.of_float 1.0) in
  printf "Returned tez: %a\n" Tez.pp tez;
  printf "Returned kit: %a\n" Kit.pp kit;
  print_uniswap uniswap;
  print_newline ();
  let (liq, tez, kit, uniswap) = buy_liquidity uniswap (Tez.of_float 20.0) (Kit.of_float 20.0) in
  printf "Returned liquidity: %d\n" liq;
  printf "Returned tez: %a\n" Tez.pp tez;
  printf "Returned kit: %a\n" Kit.pp kit;
  print_uniswap uniswap

let step_experiment () =
  let initial_parameters = { q = FixedPoint.of_float 0.9;
                             index = Tez.of_float 0.36;
                             target = FixedPoint.of_float 1.08;
                             protected_index = Tez.of_float 0.35;
                             drift = FixedPoint.of_float 0.0;
                             drift' = FixedPoint.of_float 0.0;
                           } in
  let interblock_time = Seconds 3600 in
  let new_index = 0.34 in
  let tez_per_kit = 0.305 in
  let new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
  printf "\n=== Initial checker parameters ===\n";
  print_string @@ show_checker_parameters initial_parameters;
  printf "\n=== New checker parameters ===\n";
  print_string @@ show_checker_parameters new_parameters


let () =
  burrow_experiment ();
  (* uniswap_experiment (); *)
  (* step_experiment (); *)
  printf "\ndone.\n"

