open Address
open Burrow
open Timestamp
open FixedPoint
open Format
open Kit
open Parameters
open Tez
open Liquidation

let burrow_experiment () =
  let params : Parameters.t =
    { q = FixedPoint.of_string "1.015";
      index = Tez.of_string "0.32";
      protected_index = Tez.of_string "0.36";
      target = FixedPoint.of_string "1.08";
      drift = FixedPoint.of_string "0.0";
      drift' = FixedPoint.of_string "0.0";
      burrow_fee_index = FixedPoint.of_string "1.0";
      imbalance_index = FixedPoint.of_string "1.0";
      outstanding_kit = Kit.one; (* TODO: What should that be? *)
      circulating_kit = Kit.zero; (* TODO: What should that be? *)
      last_touched = Timestamp.of_seconds 1;
    } in
  printf "\n=== Checker parameters ===\n";
  print_string @@ Parameters.show params;
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
  let initial_burrow : Burrow.t =
    { has_creation_deposit = true;
      owner = Address.of_string "192837";
      delegate = None;
      collateral = Tez.of_string "10.0";
      minted_kit = Kit.of_string "20.0";
      adjustment_index = Parameters.compute_adjustment_index params;
      collateral_at_auction = Tez.of_string "0.0";
      last_touched = Timestamp.of_seconds 0;
    } in
  printf "\n=== Initial burrow state ===\n";
  print_string @@ Burrow.show initial_burrow;

  printf "\n=== State of affairs ===\n";
  printf "Overburrowed          : %B\n" (Burrow.is_overburrowed params initial_burrow);
  printf "Overburrowed (optim.) : %B\n" (Burrow.is_optimistically_overburrowed params initial_burrow);
  printf "Liquidatable          : %B\n" (Burrow.is_liquidatable params initial_burrow);
  printf "\n=== Liquidation request outcome ===\n";
  let liquidation_result = request_liquidation params initial_burrow in
  print_string @@ show_liquidation_result liquidation_result;

  printf "\n=== State of affairs ===\n";
  if liquidation_result.outcome = Close then
    printf "There is no burrow left to consider.\n"
  else (
    printf "Overburrowed          : %B\n" (Burrow.is_overburrowed params liquidation_result.burrow_state);
    printf "Overburrowed (optim.) : %B\n" (Burrow.is_optimistically_overburrowed params liquidation_result.burrow_state);
    printf "Liquidatable          : %B\n" (Burrow.is_liquidatable params liquidation_result.burrow_state)
  )

let () =
  burrow_experiment ();
  printf "\ndone.\n"

