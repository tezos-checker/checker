open Cfmm
open CfmmTypes
open Common
open Error
open CheckerTypes

let[@inline] calculate_kit_in_tez
    (state_cfmm: cfmm)
    (state_last_ctez_in_tez: ratio option)
    (state_external_contracts: external_contracts)
  : (ratio * LigoOp.operation list) =
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
  let price =
    { num = Ligo.mul_int_int num_ctok num_tez;
      den = Ligo.mul_int_int den_kit den_ctez;
    } in

  (* Create an operation to ask the ctez cfmm to send updated values. Emit
   * this operation next to the one requesting prices from oracles, at the
   * end, so that the system parameters do not change between touching
   * different slices. *)
  let op_ctez_price =
    let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receive_ctez_marginal_price" !Ligo.Tezos.self_address : ((Ligo.nat * Ligo.nat) Ligo.contract) option) with
      | Some cb -> cb
      | None -> (Ligo.failwith error_GetEntrypointOptFailureReceiveCtezMarginalPrice : (Ligo.nat * Ligo.nat) Ligo.contract) in
    LigoOp.Tezos.nat_nat_contract_transaction
      cb
      (Ligo.tez_from_literal "0mutez")
      (get_ctez_cfmm_price_entrypoint state_external_contracts) in

  (price, [op_ctez_price])
