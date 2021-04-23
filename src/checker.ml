open Ratio
open Ctez
open Kit
open Avl
open Parameters
open CfmmTypes
open Cfmm
open Burrow
open LiquidationAuction
open LiquidationAuctionPrimitiveTypes
open LiquidationAuctionTypes
open Common
open Constants
open Tickets
open BurrowTypes
open CheckerTypes
open Error
open Fa12Types
open Fa2Interface

(* BEGIN_OCAML *)
let assert_checker_invariants (state: checker) : unit =
  (* Check if the auction pointerfest kind of make sense. *)
  assert_liquidation_auction_invariants state.liquidation_auctions;
  (* Per-burrow assertions *)
  List.iter
    (fun (burrow_address, burrow) ->
       assert_burrow_invariants burrow;

       match Ligo.Big_map.find_opt burrow_address state.liquidation_auctions.burrow_slices with
       | None ->
         assert (burrow_collateral_at_auction burrow = Ligo.tez_from_literal "0mutez");
       | Some slices ->
         (* Check if the linked list of slices are correct, and the amount of
          * tez inside is consistent with collateral_at_auction.
         *)
         let rec go
             (curr: leaf_ptr)
             (prev: leaf_ptr option) : Ligo.tez =
           let slice =
             avl_read_leaf
               (state.liquidation_auctions.avl_storage)
               curr in
           assert (slice.contents.burrow = burrow_address);
           assert (slice.younger = prev);
           match slice.older with
           | Some next ->
             Ligo.add_tez_tez slice.contents.tez (go next (Some curr))
           | None ->
             assert (curr = slices.oldest_slice);
             slice.contents.tez in
         let actual_collateral = go slices.youngest_slice None in
         assert (burrow_collateral_at_auction burrow = actual_collateral)
    )
    (Ligo.Big_map.bindings state.burrows)
(* END_OCAML *)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

let[@inline] find_burrow (burrows: burrow_map) (burrow_id: burrow_id) : burrow =
  match Ligo.Big_map.find_opt burrow_id burrows with
  | None -> (Ligo.failwith error_NonExistentBurrow : burrow)
  | Some burrow -> burrow

(* Looks up a burrow_id from state, and checks if the resulting burrow does
 * not have any completed liquidation slices that need to be claimed before
 * any operation. *)
let ensure_burrow_has_no_unclaimed_slices (auctions: liquidation_auctions) (burrow_id: burrow_id) : unit =
  if is_burrow_done_with_liquidations auctions burrow_id
  then ()
  else Ligo.failwith error_BurrowHasCompletedLiquidation

(* Ensure that there is no tez given. To prevent accidental fund loss. *)
let ensure_no_tez_given () =
  if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez"
  then Ligo.failwith error_UnwantedTezGiven
  else ()

let[@inline] entrypoint_create_burrow (state, delegate_opt: checker * Ligo.key_hash option) =
  let burrow = burrow_create state.parameters !Ligo.Tezos.sender !Ligo.Tezos.amount delegate_opt in
  let op1, burrow_address =
    LigoOp.Tezos.create_contract
      (fun (p, s : burrow_parameter * burrow_storage) ->
         if !Ligo.Tezos.sender <> s then
           (failwith "B1" : LigoOp.operation list * burrow_storage)
         else
           match p with
           | BurrowSetDelegate kho ->
             ([LigoOp.Tezos.set_delegate kho], s)
           | BurrowStoreTez ->
             (([] : LigoOp.operation list), s)
           | BurrowSendTezTo p ->
             let (tez, addr) = p in
             let op = match (LigoOp.Tezos.get_contract_opt addr : unit Ligo.contract option) with
               | Some c -> LigoOp.Tezos.unit_transaction () tez c
               | None -> (failwith "B2" : LigoOp.operation) in
             ([op], s)
           | BurrowSendSliceToChecker tz ->
             let op = match (LigoOp.Tezos.get_entrypoint_opt "%receiveLiquidationSlice" !Ligo.Tezos.sender : unit Ligo.contract option) with
               | Some c -> LigoOp.Tezos.unit_transaction () tz c
               | None -> (failwith "B3" : LigoOp.operation) in
             ([op], s)
      )
      delegate_opt
      !Ligo.Tezos.amount (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
      checker_address in
  let op2 = match (LigoOp.Tezos.get_entrypoint_opt "%transferAddress" !Ligo.Tezos.sender : Ligo.address Ligo.contract option) with
    | Some c -> LigoOp.Tezos.address_transaction burrow_address (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferAddress : LigoOp.operation) in

  let updated_state = {state with burrows = Ligo.Big_map.update burrow_address (Some burrow) state.burrows} in

  ([op1; op2], updated_state)

let entrypoint_touch_burrow (state, burrow_id: checker * burrow_id) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let updated_burrow = burrow_touch state.parameters burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  (([]: LigoOp.operation list), state)

let entrypoint_deposit_tez (state, burrow_id: checker * burrow_id) : (LigoOp.operation list * checker) =
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let updated_burrow = burrow_deposit_tez state.parameters !Ligo.Tezos.amount burrow in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" burrow_id : unit Ligo.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
    ([op], {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows})
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

let entrypoint_mint_kit (state, p: checker * (burrow_id * kit)) : LigoOp.operation list * checker =
  let burrow_id, kit = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let burrow = burrow_mint_kit state.parameters kit burrow in
    let state =
      { state with
        burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows;
        parameters = add_outstanding_and_circulating_kit state.parameters kit;
        fa2_state = ledger_issue_kit (state.fa2_state, !Ligo.Tezos.sender, kit);
      } in
    (([]: LigoOp.operation list), state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

let entrypoint_withdraw_tez (state, p: checker * (Ligo.tez * burrow_id)) : LigoOp.operation list * checker =
  let tez, burrow_id = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let burrow = burrow_withdraw_tez state.parameters tez burrow in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows} in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id : (Ligo.tez * Ligo.address) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (tez, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
    ([op], state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

let entrypoint_burn_kit (state, p: checker * (burrow_id * kit)) : LigoOp.operation list * checker =
  let burrow_id, kit = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let updated_burrow = burrow_burn_kit state.parameters kit burrow in
    (* TODO: What should happen if the following is violated? *)
    assert (state.parameters.circulating_kit >= kit);
    let state =
      {state with
       burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
       parameters =
         remove_outstanding_kit
           (remove_circulating_kit state.parameters kit)
           kit;
       fa2_state = ledger_withdraw_kit (state.fa2_state, !Ligo.Tezos.sender, kit);
      } in
    (([]: LigoOp.operation list), state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

let entrypoint_activate_burrow (state, burrow_id: checker * burrow_id) : LigoOp.operation list * checker =
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let updated_burrow = burrow_activate state.parameters !Ligo.Tezos.amount burrow in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" burrow_id : unit Ligo.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    ([op], state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

let entrypoint_deactivate_burrow (state, burrow_id: checker * burrow_id) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let (updated_burrow, returned_tez) = burrow_deactivate state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id : (Ligo.tez * Ligo.address) Ligo.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (returned_tez, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c (* NOTE: returned_tez inlcudes creation deposit! *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
    ([op], updated_state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

let entrypoint_set_burrow_delegate (state, p: checker * (burrow_id * Ligo.key_hash option)) : LigoOp.operation list * checker =
  let burrow_id, delegate_opt = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let updated_burrow = burrow_set_delegate state.parameters delegate_opt burrow in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSetDelegate" burrow_id : Ligo.key_hash option Ligo.contract option) with
      | Some c -> LigoOp.Tezos.opt_key_hash_transaction delegate_opt (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSetDelegate : LigoOp.operation) in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    ([op], state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

(* TODO: Arthur: one time we might want to trigger garbage collection of
 * slices is during a liquidation. a liquidation creates one slice, so if we
 * clear one pending slice when that happens it won't grow unbounded (yes,
 * there are degenerate cases where the queue starts growing much faster that
 * the auctions are happening and in those instances it could grow unbounded,
 * but roughly speaking in most cases it should average out) *)
let[@inline] entrypoint_mark_for_liquidation (state, burrow_id: checker * burrow_id) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in

  let
    { liquidation_reward = liquidation_reward;
      tez_to_auction = tez_to_auction;
      burrow_state = updated_burrow;
    } = match burrow_request_liquidation state.parameters burrow with
    | None -> (Ligo.failwith error_NotLiquidationCandidate : liquidation_details)
    | Some type_and_details -> let _, details = type_and_details in details
  in
  let contents =
    { burrow = burrow_id;
      tez = tez_to_auction;
      min_kit_for_unwarranted = compute_min_kit_for_unwarranted state.parameters burrow tez_to_auction;
    } in

  let (updated_liquidation_auctions, _) =
    liquidation_auction_send_to_auction state.liquidation_auctions contents in

  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () liquidation_reward c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in

  ( [op],
    { state with
      burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
      liquidation_auctions = updated_liquidation_auctions;
    }
  )

(* Cancel the liquidation of a slice. *)
let entrypoint_cancel_liquidation_slice (state, leaf_ptr: checker * leaf_ptr) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let (cancelled, auctions) = liquidation_auctions_cancel_slice state.liquidation_auctions leaf_ptr in
  let burrow = find_burrow state.burrows cancelled.burrow in
  if !Ligo.Tezos.sender = burrow_owner burrow then
    let burrow = burrow_return_slice_from_auction cancelled burrow in
    let state =
      { state with
        burrows = Ligo.Big_map.add cancelled.burrow burrow state.burrows;
        liquidation_auctions = auctions;
      } in
    assert_checker_invariants state;
    (([]:  LigoOp.operation list), state)
  else
    (Ligo.failwith error_AuthenticationError : LigoOp.operation list * checker)

(* NOTE: It prepends the operation to the list of operations given. This means
 * that if we entrypoint_touch a list of liquidation slices, the order of operations is
 * reversed. *)
let touch_liquidation_slice
    (ops: LigoOp.operation list)
    (auctions: liquidation_auctions)
    (state_burrows: burrow_map)
    (leaf_ptr: leaf_ptr)
  : (LigoOp.operation list * liquidation_auctions * burrow_map * kit) =

  let slice, outcome, auctions = liquidation_auctions_pop_completed_slice auctions leaf_ptr in

  (* How much kit should be given to the burrow and how much should be burned. *)
  (* FIXME: we treat each slice in a lot separately, so Sum(kit_to_repay_i +
   * kit_to_burn_i)_{1..n} might not add up to outcome.winning_bid.kit, due
   * to truncation. That could be a problem; the extra kit, no matter how
   * small, must be dealt with (e.g. be removed from the circulating kit).
   *
   *   kit_corresponding_to_slice =
   *     FLOOR (outcome.winning_bid.kit * (leaf.tez / outcome.sold_tez))
   *   penalty =
   *     CEIL (kit_corresponding_to_slice * penalty_percentage)  , if (corresponding_kit < leaf.min_kit_for_unwarranted)
   *     zero                                                    , otherwise
   *   kit_to_repay = kit_corresponding_to_slice - penalty
  *)
  let kit_to_repay, kit_to_burn =
    let corresponding_kit =
      kit_of_fraction_floor
        (Ligo.mul_int_int (tez_to_mutez slice.tez) (kit_to_mukit_int outcome.winning_bid.kit))
        (Ligo.mul_int_int (tez_to_mutez outcome.sold_tez) kit_scaling_factor_int)
    in
    let penalty =
      let { num = num_lp; den = den_lp; } = liquidation_penalty in
      if corresponding_kit < slice.min_kit_for_unwarranted then
        kit_of_fraction_ceil
          (Ligo.mul_int_int (kit_to_mukit_int corresponding_kit) num_lp)
          (Ligo.mul_int_int kit_scaling_factor_int den_lp)
      else
        kit_zero
    in
    (kit_sub corresponding_kit penalty, penalty)
  in

  let state_burrows =
    let burrow = match Ligo.Big_map.find_opt slice.burrow state_burrows with
      | None -> (failwith "TODO: Check if this case can happen." : burrow)
      | Some b -> b
    in
    Ligo.Big_map.update
      slice.burrow
      (Some (burrow_return_kit_from_auction slice kit_to_repay burrow))
      state_burrows in

  (* Signal the burrow to send the tez to checker. *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendSliceToChecker" slice.burrow : Ligo.tez Ligo.contract option) with
    | Some c -> LigoOp.Tezos.tez_transaction slice.tez (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendSliceToChecker : LigoOp.operation) in
  ((op :: ops), auctions, state_burrows, kit_to_burn)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let rec touch_liquidation_slices_rec
    (ops, state_liquidation_auctions, state_burrows, old_kit_to_burn, slices: LigoOp.operation list * liquidation_auctions * burrow_map * kit * leaf_ptr list)
  : (LigoOp.operation list * liquidation_auctions * burrow_map * kit) =
  match slices with
  | [] -> (ops, state_liquidation_auctions, state_burrows, old_kit_to_burn)
  | x::xs ->
    let new_ops, new_state_liquidation_auctions, new_state_burrows, new_kit_to_burn =
      touch_liquidation_slice ops state_liquidation_auctions state_burrows x in
    touch_liquidation_slices_rec (new_ops, new_state_liquidation_auctions, new_state_burrows, kit_add old_kit_to_burn new_kit_to_burn, xs)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let[@inline] entrypoint_touch_liquidation_slices (state, slices: checker * leaf_ptr list): (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  (* NOTE: the order of the operations is reversed here (wrt to the order of
   * the slices), but hopefully we don't care in this instance about this. *)
  let
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_price = state_last_price;
      fa2_state = state_fa2_state;
    } = state in

  let new_ops, state_liquidation_auctions, state_burrows, kit_to_burn =
    touch_liquidation_slices_rec (([]: LigoOp.operation list), state_liquidation_auctions, state_burrows, kit_zero, slices) in
  let state_parameters = remove_circulating_kit state_parameters kit_to_burn in

  let new_state =
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_price = state_last_price;
      fa2_state = state_fa2_state;
    } in
  assert_checker_invariants new_state;
  (new_ops, new_state)

(* ************************************************************************* *)
(**                                 CFMM                                     *)
(* ************************************************************************* *)

let entrypoint_buy_kit (state, p: checker * (ctez * kit * Ligo.timestamp)) : LigoOp.operation list * checker =
  let ctez, min_kit_expected, deadline = p in
  let _ = ensure_no_tez_given () in
  let (kit_tokens, updated_cfmm) = cfmm_buy_kit state.cfmm ctez min_kit_expected deadline in
  let transfer =
    { address_from = !Ligo.Tezos.sender;
      address_to = checker_address;
      value = ctez_to_muctez_nat ctez;
    } in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : transfer Ligo.contract option) with
    | Some c -> (LigoOp.Tezos.fa12_transfer_transaction transfer (Ligo.tez_from_literal "0mutez") c)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : LigoOp.operation) in
  ( [op],
    { state with
      cfmm = updated_cfmm;
      (* when sending/receiving kit to/from uniswap, we destroy/create it. an alternative would be to instead
       * transfer them to/from the checker's account via an FA2 transfer call.
       *)
      fa2_state = ledger_issue_kit (state.fa2_state, !Ligo.Tezos.sender, kit_tokens);
    }
  )

let entrypoint_sell_kit (state, p: checker * (kit * ctez * Ligo.timestamp)) : LigoOp.operation list * checker =
  let kit, min_ctez_expected, deadline = p in
  let _ = ensure_no_tez_given () in
  let (ctez, updated_cfmm) = cfmm_sell_kit state.cfmm kit min_ctez_expected deadline in
  let transfer =
    { address_from = checker_address;
      address_to = !Ligo.Tezos.sender;
      value = ctez_to_muctez_nat ctez;
    } in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : transfer Ligo.contract option) with
    | Some c -> (LigoOp.Tezos.fa12_transfer_transaction transfer (Ligo.tez_from_literal "0mutez") c)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : LigoOp.operation) in
  ( [op],
    { state with
      cfmm = updated_cfmm;
      fa2_state = ledger_withdraw_kit (state.fa2_state, !Ligo.Tezos.sender, kit);
    }
  )

let entrypoint_add_liquidity (state, p: checker * (ctez * kit * Ligo.nat * Ligo.timestamp)) : LigoOp.operation list * checker =
  let ctez_deposited, max_kit_deposited, min_lqt_minted, deadline = p in
  let _ = ensure_no_tez_given () in
  let (lqt_tokens, kit_tokens, updated_cfmm) =
    cfmm_add_liquidity state.cfmm ctez_deposited max_kit_deposited min_lqt_minted deadline in
  let transfer =
    { address_from = !Ligo.Tezos.sender;
      address_to = checker_address;
      value = ctez_to_muctez_nat ctez_deposited;
    } in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : transfer Ligo.contract option) with
    | Some c -> (LigoOp.Tezos.fa12_transfer_transaction transfer (Ligo.tez_from_literal "0mutez") c)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : LigoOp.operation) in
  ( [op],
    { state with
      cfmm = updated_cfmm;
      fa2_state =
        let st = state.fa2_state in
        let st = ledger_withdraw_kit (st, !Ligo.Tezos.sender, kit_sub max_kit_deposited kit_tokens) in
        let st = ledger_issue_liquidity (st, !Ligo.Tezos.sender, lqt_tokens) in
        st
    })

let entrypoint_remove_liquidity (state, p: checker * (Ligo.nat * ctez * kit * Ligo.timestamp)) : LigoOp.operation list * checker =
  let lqt_burned, min_ctez_withdrawn, min_kit_withdrawn, deadline = p in
  let _ = ensure_no_tez_given () in
  let (ctez, kit_tokens, updated_cfmm) =
    cfmm_remove_liquidity state.cfmm lqt_burned min_ctez_withdrawn min_kit_withdrawn deadline in
  let transfer =
    { address_from = checker_address;
      address_to = !Ligo.Tezos.sender;
      value = ctez_to_muctez_nat ctez;
    } in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transfer" ctez_fa12_address : transfer Ligo.contract option) with
    | Some c -> (LigoOp.Tezos.fa12_transfer_transaction transfer (Ligo.tez_from_literal "0mutez") c)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : LigoOp.operation) in
  ( [op],
    { state with
      cfmm = updated_cfmm;
      fa2_state =
        let st = state.fa2_state in
        let st = ledger_withdraw_liquidity (st, !Ligo.Tezos.sender, lqt_burned) in
        let st = ledger_issue_kit (st, !Ligo.Tezos.sender, kit_tokens) in
        st
    })

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

let entrypoint_liquidation_auction_place_bid (state, kit: checker * kit) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in

  let bid = { address=(!Ligo.Tezos.sender); kit=kit; } in
  let current_auction = liquidation_auction_get_current_auction state.liquidation_auctions in

  let (new_current_auction, bid_details) = liquidation_auction_place_bid current_auction bid in
  let bid_ticket = issue_liquidation_auction_bid_ticket bid_details in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferLABidTicket" !Ligo.Tezos.sender : liquidation_auction_bid_content Ligo.ticket Ligo.contract option) with
    | Some c -> LigoOp.Tezos.la_bid_transaction bid_ticket (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferLABidTicket : LigoOp.operation) in
  ( [op],
    { state with
      liquidation_auctions=
        { state.liquidation_auctions with
          current_auction = Some new_current_auction;
        };
      fa2_state = ledger_withdraw_kit (state.fa2_state, !Ligo.Tezos.sender, kit)
    }
  )

let entrypoint_liquidation_auction_reclaim_bid (state, bid_details: checker * liquidation_auction_bid) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let kit = liquidation_auction_reclaim_bid state.liquidation_auctions bid_details in
  ( ([]: LigoOp.operation list),
    { state with
      fa2_state =
        ledger_issue_kit (state.fa2_state, !Ligo.Tezos.sender, kit)
    })

let entrypoint_liquidation_auction_claim_win (state, bid_details: checker * liquidation_auction_bid) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let (tez, liquidation_auctions) = liquidation_auction_reclaim_winning_bid state.liquidation_auctions bid_details in
  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () tez c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in
  ([op], {state with liquidation_auctions = liquidation_auctions })

(* TODO: Maybe we should provide an entrypoint for increasing a losing bid.
 * *)

let[@inline] entrypoint_receive_slice_from_burrow (state, _: checker * unit) : (LigoOp.operation list * checker) =
  (* NOTE: do we have to register somewhere that we have received this tez? *)
  let _burrow = find_burrow state.burrows !Ligo.Tezos.sender in (* only accept from burrows! *)
  (([]: LigoOp.operation list), state)

(* ************************************************************************* *)
(**                              TOUCHING                                    *)
(* ************************************************************************* *)

(** Calculate how much is right now the reward for touching the main checker
  * contract. We use a bracketed calculation, where for the first
  * touch_reward_low_bracket seconds the reward increases by touch_low_reward
  * per second, and after that by touch_high_reward per second. *)
let calculate_touch_reward (last_touched: Ligo.timestamp) : kit =
  let duration_in_seconds = Ligo.sub_timestamp_timestamp !Ligo.Tezos.now last_touched in
  let low_duration = min_int duration_in_seconds touch_reward_low_bracket in
  let high_duration =
    max_int
      (Ligo.int_from_literal "0")
      (Ligo.sub_int_int duration_in_seconds touch_reward_low_bracket) in

  (* reward = FLOOR (low_duration * touch_low_reward + high_duration * touch_high_reward) *)
  let { num = num_tlr; den = den_tlr; } = touch_low_reward in
  let { num = num_thr; den = den_thr; } = touch_high_reward in
  kit_of_fraction_floor
    (Ligo.add_int_int
       (Ligo.mul_int_int low_duration  (Ligo.mul_int_int num_tlr den_thr))
       (Ligo.mul_int_int high_duration (Ligo.mul_int_int num_thr den_tlr))
    )
    (Ligo.mul_int_int den_tlr den_thr)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let rec touch_oldest
    (ops, state_liquidation_auctions, state_burrows, old_kit_to_burn, maximum: LigoOp.operation list * liquidation_auctions * burrow_map * kit * int)
  : (LigoOp.operation list * liquidation_auctions * burrow_map * kit) =
  if maximum <= 0 then
    (ops, state_liquidation_auctions, state_burrows, old_kit_to_burn)
  else
    match liquidation_auction_oldest_completed_liquidation_slice state_liquidation_auctions with
    | None -> (ops, state_liquidation_auctions, state_burrows, old_kit_to_burn)
    | Some leaf ->
      let new_ops, new_state_liquidation_auctions, new_state_burrows, new_kit_to_burn =
        touch_liquidation_slice ops state_liquidation_auctions state_burrows leaf in
      touch_oldest (new_ops, new_state_liquidation_auctions, new_state_burrows, kit_add old_kit_to_burn new_kit_to_burn, maximum - 1)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order in which the things are expected to happen). However, all inputs
 * to those operations are computed in the correct order, and, with two
 * exceptions (1. setting the delegate, and 2. call/callback to the oract), all
 * of the operations are outwards calls, to other contracts (no callbacks). It
 * should be safe to leave the order of the transaction reversed. *)
let touch_with_index (state: checker) (index:Ligo.tez) : (LigoOp.operation list * checker) =
  assert (state.parameters.last_touched <= !Ligo.Tezos.now); (* FIXME: I think this should be translated to LIGO actually. *)
  let _ = ensure_no_tez_given () in
  if state.parameters.last_touched = !Ligo.Tezos.now then
    (* Do nothing if up-to-date (idempotence) *)
    (([]: LigoOp.operation list), state)
  else
    (* TODO: What is the right order in which to do things here? We use the
     * last observed kit_in_tez price from cfmm to update the parameters,
     * which return kit to be added to the cfmm contract. Gotta make sure we
     * do things in the right order here. *)

    (* 1: Reward the contract toucher out of thin air to give to the contract toucher, and update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state.parameters.last_touched in
    let state =
      { state with
        parameters = add_circulating_kit state.parameters reward;
        fa2_state = ledger_issue_kit (state.fa2_state, !Ligo.Tezos.sender, reward);
      } in

    (* 2: Update the system parameters *)
    let total_accrual_to_cfmm, updated_parameters =
      let kit_in_tez_in_prev_block = (cfmm_kit_in_ctez_in_prev_block state.cfmm) in (* FIXME: times ctez_in_tez *)
      parameters_touch index kit_in_tez_in_prev_block state.parameters
    in

    (* 3: Add accrued burrowing fees to the cfmm sub-contract *)
    let updated_cfmm = cfmm_add_accrued_kit state.cfmm total_accrual_to_cfmm in

    (* 5: Update auction-related info (e.g. start a new auction) *)
    let updated_liquidation_auctions =
      liquidation_auction_touch
        state.liquidation_auctions
        (* Start the auction using the current liquidation price. We could
         * also have calculated the price right now directly using the oracle
         * feed as (tz_t * q_t), or use the current minting price, but using
         * the liquidation price is the safest option. *)
        (minting_price updated_parameters) in (* FIXME: minting vs. liquidation? comment and code disagree! *)

    (* 6: Touch oldest liquidation slices *)
    (* TODO: Touch only runs at most once per block. But it might be beneficial to run this step
     * without that restriction. *)
    let state =
      { state with
        parameters = updated_parameters;
        cfmm = updated_cfmm;
        liquidation_auctions = updated_liquidation_auctions;
      } in

    (* TODO: Figure out how many slices we can process per checker entrypoint_touch.*)
    let ops, state =
      let
        { burrows = state_burrows;
          cfmm = state_cfmm;
          parameters = state_parameters;
          liquidation_auctions = state_liquidation_auctions;
          last_price = state_last_price;
          fa2_state = state_fa2_state;
        } = state in
      let ops, state_liquidation_auctions, state_burrows, kit_to_burn =
        touch_oldest (([]: LigoOp.operation list), state_liquidation_auctions, state_burrows, kit_zero, number_of_slices_to_process) in
      let state_parameters = remove_circulating_kit state_parameters kit_to_burn in
      let new_state =
        { burrows = state_burrows;
          cfmm = state_cfmm;
          parameters = state_parameters;
          liquidation_auctions = state_liquidation_auctions;
          last_price = state_last_price;
          fa2_state = state_fa2_state;
        } in
      (ops, new_state) in

    assert_checker_invariants state;

    (* Create operations to ask the oracles to send updated values. *)
    let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receivePrice" !Ligo.Tezos.self_address : (Ligo.nat Ligo.contract) option) with
      | Some cb -> cb
      | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : Ligo.nat Ligo.contract) in
    let oracle = match (LigoOp.Tezos.get_entrypoint_opt oracle_entrypoint oracle_address : (Ligo.nat Ligo.contract) Ligo.contract option) with
      | Some c -> c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint : (Ligo.nat Ligo.contract) Ligo.contract) in
    let op = LigoOp.Tezos.nat_contract_transaction cb (Ligo.tez_from_literal "0mutez") oracle in
    let ops = (op :: ops) in (* FIXME: op should be at the end, not the beginning *)

    (ops, state)

let entrypoint_touch (state, _: checker * unit) : (LigoOp.operation list * checker) =
  let index = match state.last_price with
    | None -> state.parameters.index (* use the old one *)
    | Some i -> Ligo.mul_nat_tez i (Ligo.tez_from_literal "1mutez") in (* FIXME: Is the nat supposed to represent tez? *)
  touch_with_index state index

(* ************************************************************************* *)
(**                               ORACLE                                     *)
(* ************************************************************************* *)

let entrypoint_receive_price (state, price: checker * Ligo.nat) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  if !Ligo.Tezos.sender <> oracle_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * checker)
  else
    (([]: LigoOp.operation list), {state with last_price = Some price})

(* ************************************************************************* *)
(**                               FA2                                        *)
(* ************************************************************************* *)

let entrypoint_transfer (checker, xs: checker * fa2_transfer list) : (LigoOp.operation list * checker) =
  ( ([]: LigoOp.operation list),
    { checker with
      fa2_state = fa2_run_transfer (checker.fa2_state, xs)
    }
  )

(* FIXME
let entrypoint_balance_of (checker, param: checker * fa2_balance_of_param) : (LigoOp.operation list * checker) =
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (checker.fa2_state, requests) in
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  ( [op], checker )
*)

let entrypoint_update_operators (checker, xs: checker * fa2_update_operator list) : (LigoOp.operation list * checker) =
  ( ([]: LigoOp.operation list),
    { checker with
       fa2_state = fa2_run_update_operators (checker.fa2_state, xs)
    }
  )

(* ************************************************************************* *)
(**                           CHECKER PARAMETERS                             *)
(* ************************************************************************* *)

(** User-facing checker parameters. These include non-serializable tickets. *)
type checker_params =
    Touch of unit
  | CreateBurrow of Ligo.key_hash option
  | DepositTez of burrow_id
  | WithdrawTez of (Ligo.tez * burrow_id)
  | MintKit of (burrow_id * kit)
  | BurnKit of (burrow_id * kit)
  | ActivateBurrow of burrow_id
  | DeactivateBurrow of burrow_id
  | MarkForLiquidation of burrow_id
  | TouchLiquidationSlices of leaf_ptr list
  | CancelLiquidationSlice of leaf_ptr
  | TouchBurrow of burrow_id
  | SetBurrowDelegate of (burrow_id * Ligo.key_hash option)
  | BuyKit of (ctez * kit * Ligo.timestamp)
  | SellKit of (kit * ctez * Ligo.timestamp)
  | AddLiquidity of (ctez * kit * Ligo.nat * Ligo.timestamp)
  | RemoveLiquidity of (liquidity * ctez * kit * Ligo.timestamp)
  | LiquidationAuctionPlaceBid of kit
  | LiquidationAuctionReclaimBid of liquidation_auction_bid_ticket
  | LiquidationAuctionClaimWin of liquidation_auction_bid_ticket
  | ReceiveSliceFromBurrow of unit
  | ReceivePrice of Ligo.nat
  | Transfer of fa2_transfer list
(* FIXME
  | Balance_of of fa2_balance_of_param
*)
  | Update_operators of fa2_update_operator list

(* noop *)
let[@inline] deticketify_touch (p: unit) : unit = p

(* noop *)
let[@inline] deticketify_create_burrow (p: Ligo.key_hash option) : Ligo.key_hash option = p

(* noop *)
let[@inline] deticketify_deposit_tez (p: burrow_id) : burrow_id = p

(* noop *)
let[@inline] deticketify_withdraw_tez (p: Ligo.tez * burrow_id) : Ligo.tez * burrow_id = p

(* noop *)
let[@inline] deticketify_mint_kit (p: burrow_id * kit) : burrow_id * kit = p

(* noop *)
let[@inline] deticketify_burn_kit (p: burrow_id * kit) : burrow_id * kit = p

(* noop *)
let[@inline] deticketify_activate_burrow (p: burrow_id) : burrow_id = p

(* noop *)
let[@inline] deticketify_deactivate_burrow (p: burrow_id) : burrow_id = p

(* noop *)
let[@inline] deticketify_mark_for_liquidation (burrow_id: burrow_id) : burrow_id = burrow_id

(* noop *)
let[@inline] deticketify_touch_liquidation_slices (p: leaf_ptr list) : leaf_ptr list = p

(* noop *)
let[@inline] deticketify_cancel_liquidation_slice (p: leaf_ptr) : leaf_ptr = p

(* noop *)
let[@inline] deticketify_touch_burrow (burrow_id: burrow_id) : burrow_id = burrow_id

(* noop *)
let[@inline] deticketify_set_burrow_delegate (p: burrow_id * Ligo.key_hash option) : burrow_id * Ligo.key_hash option = p

(* noop *)
let[@inline] deticketify_buy_kit (p: ctez * kit * Ligo.timestamp) : ctez * kit * Ligo.timestamp = p

(* removes tickets *)
let[@inline] deticketify_sell_kit (kit, ctez, deadline: kit * ctez * Ligo.timestamp) : kit * ctez * Ligo.timestamp =
  (kit, ctez, deadline)

(* removes tickets *)
let[@inline] deticketify_add_liquidity (ctez, kit, lqt, deadline: ctez * kit * Ligo.nat * Ligo.timestamp) : ctez * kit * liquidity * Ligo.timestamp =
  (ctez, kit, lqt, deadline)

(* removes tickets *)
let[@inline] deticketify_remove_liquidity (lqt, ctez, kit, deadline: liquidity * ctez * kit * Ligo.timestamp) : liquidity * ctez * kit * Ligo.timestamp =
  (lqt, ctez, kit, deadline)

(* removes tickets *)
let[@inline] deticketify_liquidation_auction_place_bid (kit: kit) : kit =
  kit

(* removes tickets *)
let[@inline] deticketify_liquidation_auction_reclaim_bid (bid_ticket: liquidation_auction_bid_ticket) : liquidation_auction_bid =
  ensure_valid_liquidation_auction_bid_ticket bid_ticket

(* removes tickets *)
let[@inline] deticketify_liquidation_auction_claim_win (bid_ticket: liquidation_auction_bid_ticket) : liquidation_auction_bid =
  ensure_valid_liquidation_auction_bid_ticket bid_ticket

(* noop *)
let[@inline] deticketify_receive_slice_from_burrow (p: unit) : unit = p

(* noop *)
let[@inline] deticketify_receive_price (p: Ligo.nat) : Ligo.nat = p

(* noop *)
let[@inline] deticketify_transfer (p: fa2_transfer list) : fa2_transfer list = p

(* FIXME
(* noop *)
let[@inline] deticketify_balance_of (p: fa2_balance_of_param) : fa2_balance_of_param = p
*)

(* noop *)
let[@inline] deticketify_update_operators (p: fa2_update_operator list) : fa2_update_operator list = p
