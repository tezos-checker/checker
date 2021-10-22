open Cfmm
open CfmmTypes
(* open CheckerTypes *)
open Common

let[@inline] calculate_kit_in_tez (state_cfmm: cfmm) (* (_state_external_contracts: external_contracts) *) : ratio =
  (* 1. Get the price of kit in ctez from the cfmm. To avoid having cfmm users
   *    trying to manipulate the price, we use the last price of kit in ctez
   *    observed, not the one in the current block. *)
  let kit_in_ctez_in_prev_block = cfmm_kit_in_ctez_in_prev_block state_cfmm in
  let kit_in_tez_in_prev_block = kit_in_ctez_in_prev_block in (* FIXME: times ctez_in_tez *)
  kit_in_tez_in_prev_block
