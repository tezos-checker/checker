open Address
open FixedPoint
open Format
open Huxian
open Kit
include Parameters
open Tez

let burrow_experiment () =
  let params =
    { q = FixedPoint.of_float 1.015;
      index = Tez.of_float 0.32;
      protected_index = Tez.of_float 0.36;
      target = FixedPoint.of_float 1.08;
      drift = FixedPoint.of_float 0.0;
      drift' = FixedPoint.of_float 0.0;
      burrow_fee_index = FixedPoint.of_float 1.0;
      imbalance_index = FixedPoint.of_float 1.0;
      outstanding_kit = Kit.one; (* TODO: What should that be? *)
      circulating_kit = Kit.zero; (* TODO: What should that be? *)
    } in
  printf "\n=== Checker parameters ===\n";
  print_string @@ Parameters.show_parameters params;
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
  let initial_burrow =
    { owner = Address.of_string "someone";
      delegate = None;
      collateral = Tez.of_float 10.0;
      minted_kit = Kit.of_float 20.0;
      adjustment_index = compute_adjustment_index params;
      collateral_at_auction = Tez.of_float 0.0;
    } in
  printf "\n=== Initial burrow state ===\n";
  print_string @@ show_burrow initial_burrow;

  printf "\n=== State of affairs ===\n";
  printf "Overburrowed          : %B\n" (is_overburrowed params initial_burrow);
  printf "Liquidatable          : %B\n" (is_liquidatable params initial_burrow);
  printf "\n=== Liquidation request outcome ===\n";
  let liquidation_result = request_liquidation params initial_burrow in
  print_string @@ show_liquidation_result liquidation_result;

  printf "\n=== State of affairs ===\n";
  if liquidation_result.outcome = Close then
    printf "There is no burrow left to consider.\n"
  else (
    printf "Overburrowed          : %B\n" (is_overburrowed params liquidation_result.burrow_state);
    printf "Liquidatable          : %B\n" (is_liquidatable params liquidation_result.burrow_state)
  )

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
                             burrow_fee_index = FixedPoint.of_float 1.0;
                             imbalance_index = FixedPoint.of_float 1.0;
                             outstanding_kit = Kit.one; (* TODO: What should that be? *)
                             circulating_kit = Kit.zero; (* TODO: What should that be? *)
                           } in
  let interblock_time = Seconds 3600 in
  let new_index = 0.34 in
  let tez_per_kit = 0.305 in
  let total_accrual_to_uniswap, new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
  printf "\n=== Initial checker parameters ===\n";
  print_string @@ show_parameters initial_parameters;
  printf "\n=== New checker parameters ===\n";
  print_string @@ show_parameters new_parameters;
  printf "\n=== Total accrual to uniswap ===\n";
  print_string @@ Kit.show_kit total_accrual_to_uniswap

let () =
  burrow_experiment ();
  (* uniswap_experiment (); *)
  (* step_experiment (); *)
  printf "\ndone.\n"

