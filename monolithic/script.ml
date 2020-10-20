open Huxian
open Format

let burrow_experiment () =
  (* OTHER EXAMPLES *)
  (* Unwarranted liquidation for
     let initial_burrow = { minted_kit = 10.0; collateral = 10.0; } in *)
  (* Partial liquidation for
     let initial_burrow = { minted_kit = 20.0; collateral = 10.0; } in *)
  (* Complete liquidation (deplete the collateral, but keep the burrow) for
     let initial_burrow = { minted_kit = 100.0; collateral = 10.0; } in *)
  (* Complete liquidation (close the burrow) for
     let initial_burrow = { minted_kit = 100.0; collateral = 1.001; } in *)
  let initial_burrow =
    { minted_kit = 20.0;
      collateral = 10.0;
    } in
  printf "\n=== Initial burrow state ===\n";
  print_burrow initial_burrow;
  let params =
    { q = 1.015;
      index = 0.32;
      protected_index = 0.36;
      target = 1.08;
      drift = 0.0;
      drift' = 0.0;
    } in
  printf "\n=== Checker parameters ===\n";
  print_string (format_checker_parameters params);

  printf "\n=== State of affairs ===\n";
  printf "Overburrowed          : %B\n" (is_overburrowed params initial_burrow);
  printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params initial_burrow);
  printf "\n=== Liquidation request outcome ===\n";
  let liquidation_result = request_liquidation params initial_burrow in
  print_liquidation_result liquidation_result;

  printf "\n=== State of affairs ===\n";
  match liquidation_result with
  | Partial (_,_,_,b) | Complete (_,_,_,b) | Unwarranted b ->
     printf "Overburrowed          : %B\n" (is_overburrowed params b);
     printf "Liquidatable          : %B\n" (should_burrow_be_liquidated params b)
  | Close (_,_,_) ->
     printf "There is no burrow left to consider.\n"

let uniswap_experiment () =
  let uniswap = { tez=10.; kit=5.; total_liquidity_tokens=1 }; in
  let (tez, kit, uniswap) = sell_kit uniswap 1. in
  printf "Returned tez: %f\n" tez;
  printf "Returned kit: %f\n" kit;
  print_uniswap uniswap;
  print_newline ();
  let (liq, tez, kit, uniswap) = buy_liquidity uniswap 20. 20. in
  printf "Returned liquidity: %d\n" liq;
  printf "Returned tez: %f\n" tez;
  printf "Returned kit: %f\n" kit;
  print_uniswap uniswap

let step_experiment () =
  let initial_parameters = { q = 0.9;
                             index = 0.36;
                             target = 1.08;
                             protected_index = 0.35;
                             drift = 0.0;
                             drift' = 0.0;
                           } in
  let interblock_time = Seconds 3600 in
  let new_index = 0.34 in
  let tez_per_kit = 0.305 in
  let new_parameters = step_parameters interblock_time new_index tez_per_kit initial_parameters in
  printf "\n=== Initial checker parameters ===\n";
  print_string (format_checker_parameters initial_parameters);
  printf "\n=== New checker parameters ===\n";
  print_string (format_checker_parameters new_parameters)

let () =
  burrow_experiment ();
  (* uniswap_experiment (); *)
  (* step_experiment (); *)
  printf "\ndone.\n"

