open Cfmm
open CfmmTypes
open Common

let[@inline] calculate_kit_in_tez
    (state_cfmm: cfmm)
    (state_last_ctez_in_tez: ratio option)
  : ratio =
  (* 1. Get the price of kit in ctok from the cfmm. To avoid having cfmm users
   *    trying to manipulate the price, we use the last price of kit in ctok
   *    observed, not the one in the current block. *)
  let { num = num_ctok; den = den_kit; } = cfmm_kit_in_ctok_in_prev_block state_cfmm in
  (* 2. Get the price of ctez in tez from storage (last observed). Use tez/ctez
   *    = 1 as the default price if none was observed. *)
  let { num = num_tez; den = den_ctez; } = match state_last_ctez_in_tez with
    | None -> one_ratio
    | Some price -> price in
  (* FIXME: fix the discrepancy between ctok and ctez *)
  (* 3. kit_in_tez = kit_in_ctok * ctez_in_tez *)
  { num = Ligo.mul_int_int num_ctok num_tez;
    den = Ligo.mul_int_int den_kit den_ctez;
  }
