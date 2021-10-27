open Cfmm
open CfmmTypes
open CheckerTypes
open Common

let[@inline] calculate_kit_in_tez
    (state_cfmm: cfmm)
    (state_last_ctez_in_tez: (Ligo.nat * Ligo.nat) option)
    (_state_external_contracts: external_contracts)
  : ratio =
  (* 1. Get the price of kit in ctez from the cfmm. To avoid having cfmm users
   *    trying to manipulate the price, we use the last price of kit in ctez
   *    observed, not the one in the current block. *)
  let { num = num_ctez; den = den_kit; } = cfmm_kit_in_ctez_in_prev_block state_cfmm in
  (* 2. Get the price of ctez in tez from storage (last observed). Use tez/ctez
   *    = 1 as the default price if none was observed. *)
  let { num = num_tez; den = den_ctez; } = match state_last_ctez_in_tez with
    | None -> one_ratio
    | Some (n, d) -> make_ratio (Ligo.int n) (Ligo.int d) in
  (* FIXME: Emit an operation asking to update the price *)
  { num = Ligo.mul_int_int num_ctez num_tez;
    den = Ligo.mul_int_int den_kit den_ctez;
  }
