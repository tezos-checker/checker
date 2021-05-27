open Ratio
open Ctez
open Kit
open Avl
open Parameters
open Cfmm
open CfmmTypes
open Burrow
open LiquidationAuction
open LiquidationAuctionPrimitiveTypes
open LiquidationAuctionTypes
open Common
open Constants
open BurrowTypes
open CheckerTypes
open Error
open Fa12Interface
open Fa2Interface
open Mem

(* BEGIN_OCAML *)
let assert_checker_invariants (state: checker) : unit =
  (* Check if the auction pointerfest kind of make sense. *)
  assert_liquidation_auction_invariants state.liquidation_auctions;
  (* Check that the total kit tracked on the fa2 ledger is consistent with
   * (i.e., equal to) the circulating kit as stored in the state parameters. *)
  assert (fa2_get_total_kit_balance state.fa2_state = kit_to_mukit_nat state.parameters.circulating_kit);
  (* Check that the total kit that checker owns (plus one - because of the
   * phantom kit token in the cfmm) is at least as much as the total kit in the
   * cfmm. Of course there can be more, e.g., from pending bids, or because of
   * imperfect liquidations. *)
  assert (kit_add (kit_of_mukit (fa2_get_balance (state.fa2_state, !Ligo.Tezos.self_address, kit_token_id))) (kit_of_mukit (Ligo.nat_from_literal "1n")) >= state.cfmm.kit);
  (* Check that the total number of liquidity tokens tracked on the fa2 ledger
   * is consistent with (i.e., 1 token less than - because of the phantom lqt
   * token in the cfmm) the total number of liquidity tokens in the cfmm. *)
  assert (Ligo.add_nat_nat (fa2_get_total_lqt_balance state.fa2_state) (Ligo.nat_from_literal "1n") = state.cfmm.lqt);
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
(**                           EXTERNAL_CONTRACTS                             *)
(* ************************************************************************* *)

let[@inline] get_transfer_ctez_entrypoint (external_contracts: external_contracts): fa12_transfer Ligo.contract =
  match (LigoOp.Tezos.get_entrypoint_opt "%transfer" external_contracts.ctez : fa12_transfer Ligo.contract option) with
  | Some c -> c
  | None -> (Ligo.failwith error_GetEntrypointOptFailureFA12Transfer : fa12_transfer Ligo.contract)

let[@inline] get_oracle_entrypoint (external_contracts: external_contracts): (Ligo.nat Ligo.contract) Ligo.contract =
  match (LigoOp.Tezos.get_entrypoint_opt oracle_entrypoint external_contracts.oracle: (Ligo.nat Ligo.contract) Ligo.contract option) with
  | Some c -> c
  | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint: (Ligo.nat Ligo.contract) Ligo.contract)

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

(* Ensure that the given pointer exists and that it points to a Root node. *)
let[@inline] ensure_valid_avl_ptr (mem: mem) (avl_ptr: avl_ptr) : unit =
  match mem_get_opt mem (match avl_ptr with AVLPtr r -> r) with
  | Some (Root _) -> ()
  | _ -> Ligo.failwith error_InvalidAvlPtr

(* Ensure that the given pointer exists and that it points to a Leaf node. *)
let[@inline] ensure_valid_leaf_ptr (mem: mem) (leaf_ptr: leaf_ptr) : unit =
  match mem_get_opt mem (match leaf_ptr with LeafPtr r -> r) with
  | Some (Leaf _) -> ()
  | _ -> Ligo.failwith error_InvalidLeafPtr

let[@inline] entrypoint_create_burrow (state, (burrow_no, delegate_opt): checker * (Ligo.nat * Ligo.key_hash option)) =
  assert_checker_invariants state;
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let () = if Ligo.Big_map.mem burrow_id state.burrows
    then Ligo.failwith error_BurrowAlreadyExists
    else () in
  let op, burrow_address =
    LigoOp.Tezos.create_contract
      (fun (p, storage : burrow_parameter * burrow_storage) ->
         if !Ligo.Tezos.sender <> storage.checker_address then
           (failwith "B1" : LigoOp.operation list * burrow_storage)
         else
           match p with
           | BurrowSetDelegate kho ->
             ([LigoOp.Tezos.set_delegate kho], storage)
           | BurrowStoreTez ->
             (([] : LigoOp.operation list), storage)
           | BurrowSendTezTo p ->
             let (tez, addr) = p in
             let op = match (LigoOp.Tezos.get_contract_opt addr : unit Ligo.contract option) with
               | Some c -> LigoOp.Tezos.unit_transaction () tez c
               | None -> (failwith "B2" : LigoOp.operation) in
             ([op], storage)
           | BurrowSendSliceToChecker tz ->
             let op = match (LigoOp.Tezos.get_entrypoint_opt "%receive_slice_from_burrow" !Ligo.Tezos.sender : burrow_id Ligo.contract option) with
               | Some c -> LigoOp.Tezos.address_nat_transaction storage.burrow_id tz c
               | None -> (failwith "B3" : LigoOp.operation) in
             ([op], storage)
      )
      delegate_opt
      !Ligo.Tezos.amount (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
      {checker_address = !Ligo.Tezos.self_address; burrow_id = burrow_id; } in

  let burrow = burrow_create state.parameters burrow_address !Ligo.Tezos.amount delegate_opt in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows} in
  assert_checker_invariants state;
  ([op], state)

let entrypoint_touch_burrow (state, burrow_id: checker * burrow_id) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let updated_burrow = burrow_touch state.parameters burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  assert_checker_invariants state;
  (([]: LigoOp.operation list), state)

let entrypoint_deposit_tez (state, burrow_no: checker * Ligo.nat) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let updated_burrow = burrow_deposit_tez state.parameters !Ligo.Tezos.amount burrow in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" (burrow_address burrow) : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  assert_checker_invariants state;
  ([op], state)

let entrypoint_mint_kit (state, (burrow_no, kit): checker * (Ligo.nat * kit)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = burrow_mint_kit state.parameters kit burrow in
  let state =
    { state with
      burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows;
      parameters = add_outstanding_and_circulating_kit state.parameters kit;
      fa2_state = ledger_issue_kit (state.fa2_state, !Ligo.Tezos.sender, kit);
    } in
  assert_checker_invariants state;
  (([]: LigoOp.operation list), state)

let entrypoint_withdraw_tez (state, (tez, burrow_no): checker * (Ligo.tez * Ligo.nat)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let burrow = burrow_withdraw_tez state.parameters tez burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows} in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" (burrow_address burrow): (Ligo.tez * Ligo.address) Ligo.contract option) with
    | Some c -> LigoOp.Tezos.tez_address_transaction (tez, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
  assert_checker_invariants state;
  ([op], state)

let entrypoint_burn_kit (state, (burrow_no, kit): checker * (Ligo.nat * kit)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let updated_burrow = burrow_burn_kit state.parameters kit burrow in
  (* Note: there should be no way to remove more kit from circulation than what
   * is already in circulation. If anyone tries to do so, it means that they
   * try to remove kit they do not own. So, either
   * [remove_outstanding_and_circulating_kit] below will fail first (illegal
   * subtraction), or [ledger_withdraw_kit] below will fail first (attempt to
   * move kit that is not owned by the sender). Either way I (George) think
   * that this assertion is only useful for our unit tests. *)
  assert (state.parameters.circulating_kit >= kit);
  let state =
    {state with
     burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
     parameters = remove_outstanding_and_circulating_kit state.parameters kit;
     fa2_state = ledger_withdraw_kit (state.fa2_state, !Ligo.Tezos.sender, kit);
    } in
  assert_checker_invariants state;
  (([]: LigoOp.operation list), state)

let entrypoint_activate_burrow (state, burrow_no: checker * Ligo.nat) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let updated_burrow = burrow_activate state.parameters !Ligo.Tezos.amount burrow in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" (burrow_address burrow) : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  assert_checker_invariants state;
  ([op], state)

let entrypoint_deactivate_burrow (state, (burrow_no, receiver): checker * (Ligo.nat * Ligo.address)) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let (updated_burrow, returned_tez) = burrow_deactivate state.parameters burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" (burrow_address burrow): (Ligo.tez * Ligo.address) Ligo.contract option) with
    | Some c -> LigoOp.Tezos.tez_address_transaction (returned_tez, receiver) (Ligo.tez_from_literal "0mutez") c (* NOTE: returned_tez inlcudes creation deposit! *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
  assert_checker_invariants state;
  ([op], state)

let entrypoint_set_burrow_delegate (state, (burrow_no, delegate_opt): checker * (Ligo.nat * Ligo.key_hash option)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in
  let burrow_id = (!Ligo.Tezos.sender, burrow_no) in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let updated_burrow = burrow_set_delegate state.parameters delegate_opt burrow in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSetDelegate" (burrow_address burrow) : Ligo.key_hash option Ligo.contract option) with
    | Some c -> LigoOp.Tezos.opt_key_hash_transaction delegate_opt (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSetDelegate : LigoOp.operation) in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  assert_checker_invariants state;
  ([op], state)

(* TODO: Arthur: one time we might want to trigger garbage collection of
 * slices is during a liquidation. a liquidation creates one slice, so if we
 * clear one pending slice when that happens it won't grow unbounded (yes,
 * there are degenerate cases where the queue starts growing much faster that
 * the auctions are happening and in those instances it could grow unbounded,
 * but roughly speaking in most cases it should average out) *)
let[@inline] entrypoint_mark_for_liquidation (state, burrow_id: checker * burrow_id) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
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

  let state =
    if Ligo.eq_tez_tez tez_to_auction (Ligo.tez_from_literal "0mutez") then
      (* If the slice would be empty, don't create it. *)
      { state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows; }
    else
      (* Otherwise do. *)
      let contents =
        { burrow = burrow_id;
          tez = tez_to_auction;
          min_kit_for_unwarranted = compute_min_kit_for_unwarranted state.parameters burrow tez_to_auction;
        } in
      let (updated_liquidation_auctions, _leaf_ptr) =
        liquidation_auction_send_to_auction state.liquidation_auctions contents in
      { state with
        burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
        liquidation_auctions = updated_liquidation_auctions;
      } in

  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () liquidation_reward c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in

  assert_checker_invariants state;
  ([op], state)

(* Cancel the liquidation of a slice. *)
let entrypoint_cancel_liquidation_slice (state, leaf_ptr: checker * leaf_ptr) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in
  let _ = ensure_valid_leaf_ptr state.liquidation_auctions.avl_storage leaf_ptr in
  let (cancelled, auctions) = liquidation_auctions_cancel_slice state.liquidation_auctions leaf_ptr in
  let (burrow_owner, _) = cancelled.burrow in
  let burrow = find_burrow state.burrows cancelled.burrow in
  let _ =
    if burrow_is_cancellation_warranted state.parameters burrow cancelled.tez
    then ()
    else Ligo.failwith error_UnwarrantedCancellation in
  if !Ligo.Tezos.sender = burrow_owner then
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
  : (LigoOp.operation list * liquidation_auctions * burrow_map * kit * kit) =

  let _ = ensure_valid_leaf_ptr auctions.avl_storage leaf_ptr in

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
      let liquidation_was_warranted =
        match slice.min_kit_for_unwarranted with
        | None -> true
        | Some min_kit_for_unwarranted -> corresponding_kit < min_kit_for_unwarranted in
      if liquidation_was_warranted then
        kit_of_fraction_ceil
          (Ligo.mul_int_int (kit_to_mukit_int corresponding_kit) num_lp)
          (Ligo.mul_int_int kit_scaling_factor_int den_lp)
      else
        kit_zero
    in
    (kit_sub corresponding_kit penalty, penalty)
  in

  let burrow = find_burrow state_burrows slice.burrow in
  let state_burrows =
    Ligo.Big_map.update
      slice.burrow
      (Some (burrow_return_kit_from_auction slice kit_to_repay burrow))
      state_burrows in

  (* Signal the burrow to send the tez to checker. *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendSliceToChecker" (burrow_address burrow): Ligo.tez Ligo.contract option) with
    | Some c -> LigoOp.Tezos.tez_transaction slice.tez (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendSliceToChecker : LigoOp.operation) in
  ((op :: ops), auctions, state_burrows, kit_to_repay, kit_to_burn)

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let rec touch_liquidation_slices_rec
    (ops, state_liquidation_auctions, state_burrows, old_kit_to_repay, old_kit_to_burn, slices: LigoOp.operation list * liquidation_auctions * burrow_map * kit * kit * leaf_ptr list)
  : (LigoOp.operation list * liquidation_auctions * burrow_map * kit * kit) =
  match slices with
  | [] -> (ops, state_liquidation_auctions, state_burrows, old_kit_to_repay, old_kit_to_burn)
  | x::xs ->
    let new_ops, new_state_liquidation_auctions, new_state_burrows, new_kit_to_repay, new_kit_to_burn =
      touch_liquidation_slice ops state_liquidation_auctions state_burrows x in
    touch_liquidation_slices_rec
      ( new_ops,
        new_state_liquidation_auctions,
        new_state_burrows,
        kit_add old_kit_to_repay new_kit_to_repay,
        kit_add old_kit_to_burn new_kit_to_burn,
        xs
      )

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order the input slices were processed in). However, since the operations
 * computed are independent from each other, this needs not be a problem. *)
let[@inline] entrypoint_touch_liquidation_slices (state, slices: checker * leaf_ptr list): (LigoOp.operation list * checker) =
  assert_checker_invariants state;
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
      external_contracts = state_external_contracts;
    } = state in

  let new_ops, state_liquidation_auctions, state_burrows, kit_to_repay, kit_to_burn =
    touch_liquidation_slices_rec (([]: LigoOp.operation list), state_liquidation_auctions, state_burrows, kit_zero, kit_zero, slices) in
  let state_parameters =
    let state_parameters = remove_outstanding_and_circulating_kit state_parameters kit_to_repay in
    let state_parameters = remove_circulating_kit state_parameters kit_to_burn in
    state_parameters in

  let state_fa2_state =
    ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.self_address, kit_add kit_to_repay kit_to_burn) in

  let state =
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_price = state_last_price;
      fa2_state = state_fa2_state;
      external_contracts = state_external_contracts;
    } in
  assert_checker_invariants state;
  (new_ops, state)

(* ************************************************************************* *)
(**                                 CFMM                                     *)
(* ************************************************************************* *)

let entrypoint_buy_kit (state, p: checker * (ctez * kit * Ligo.timestamp)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let ctez, min_kit_expected, deadline = p in
  let _ = ensure_no_tez_given () in
  let (kit_tokens, updated_cfmm) = cfmm_buy_kit state.cfmm ctez min_kit_expected deadline in
  let transfer =
    { address_from = !Ligo.Tezos.sender;
      address_to = !Ligo.Tezos.self_address;
      value = ctez_to_muctez_nat ctez;
    } in
  let op =
        LigoOp.Tezos.fa12_transfer_transaction
          transfer (Ligo.tez_from_literal "0mutez")
          (get_transfer_ctez_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.self_address, kit_tokens) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.sender, kit_tokens) in
    state_fa2_state in

  assert (fa2_get_total_kit_balance state.fa2_state = fa2_get_total_kit_balance state_fa2_state); (* preservation of kit *)
  assert (fa2_get_total_lqt_balance state.fa2_state = fa2_get_total_lqt_balance state_fa2_state); (* preservation of lqt *)

  let state =
    { state with
      cfmm = updated_cfmm;
      (* when sending/receiving kit to/from uniswap, we destroy/create it. an
       * alternative would be to instead transfer them to/from the checker's
       * account via an FA2 transfer call. *)
      fa2_state = state_fa2_state;
    } in

  assert_checker_invariants state;

  ([op], state)

let entrypoint_sell_kit (state, p: checker * (kit * ctez * Ligo.timestamp)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let kit, min_ctez_expected, deadline = p in
  let _ = ensure_no_tez_given () in
  let (ctez, updated_cfmm) = cfmm_sell_kit state.cfmm kit min_ctez_expected deadline in
  let transfer =
    { address_from = !Ligo.Tezos.self_address;
      address_to = !Ligo.Tezos.sender;
      value = ctez_to_muctez_nat ctez;
    } in
  let op =
    LigoOp.Tezos.fa12_transfer_transaction
      transfer
      (Ligo.tez_from_literal "0mutez")
      (get_transfer_ctez_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.sender, kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.self_address, kit) in
    state_fa2_state in

  assert (fa2_get_total_kit_balance state.fa2_state = fa2_get_total_kit_balance state_fa2_state); (* preservation of kit *)
  assert (fa2_get_total_lqt_balance state.fa2_state = fa2_get_total_lqt_balance state_fa2_state); (* preservation of lqt *)

  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in
  assert_checker_invariants state;

  ([op], state)

let entrypoint_add_liquidity (state, p: checker * (ctez * kit * Ligo.nat * Ligo.timestamp)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let ctez_deposited, max_kit_deposited, min_lqt_minted, deadline = p in
  let _ = ensure_no_tez_given () in
  let (lqt_tokens, kit_tokens, updated_cfmm) =
    cfmm_add_liquidity state.cfmm ctez_deposited max_kit_deposited min_lqt_minted deadline in
  let transfer =
    { address_from = !Ligo.Tezos.sender;
      address_to = !Ligo.Tezos.self_address;
      value = ctez_to_muctez_nat ctez_deposited;
    } in
  let op =
    LigoOp.Tezos.fa12_transfer_transaction
      transfer
      (Ligo.tez_from_literal "0mutez")
      (get_transfer_ctez_entrypoint state.external_contracts) in

  let deposited_kit = kit_sub max_kit_deposited kit_tokens in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.sender, deposited_kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.self_address, deposited_kit) in
    let state_fa2_state = ledger_issue_liquidity (state_fa2_state, !Ligo.Tezos.sender, lqt_tokens) in (* create *)
    state_fa2_state in

  assert (fa2_get_total_kit_balance state.fa2_state = fa2_get_total_kit_balance state_fa2_state); (* preservation of kit *)
  assert (Ligo.add_nat_nat (fa2_get_total_lqt_balance state.fa2_state) lqt_tokens = fa2_get_total_lqt_balance state_fa2_state);

  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in

  assert_checker_invariants state;

  ([op], state)

let entrypoint_remove_liquidity (state, p: checker * (Ligo.nat * ctez * kit * Ligo.timestamp)) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let lqt_burned, min_ctez_withdrawn, min_kit_withdrawn, deadline = p in
  let _ = ensure_no_tez_given () in
  let (ctez, kit_tokens, updated_cfmm) =
    cfmm_remove_liquidity state.cfmm lqt_burned min_ctez_withdrawn min_kit_withdrawn deadline in
  let transfer =
    { address_from = !Ligo.Tezos.self_address;
      address_to = !Ligo.Tezos.sender;
      value = ctez_to_muctez_nat ctez;
    } in
  let op =
    LigoOp.Tezos.fa12_transfer_transaction
      transfer
      (Ligo.tez_from_literal "0mutez")
      (get_transfer_ctez_entrypoint state.external_contracts) in

  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    let state_fa2_state = ledger_withdraw_liquidity (state_fa2_state, !Ligo.Tezos.sender, lqt_burned) in (* destroy *)
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.self_address, kit_tokens) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.sender, kit_tokens) in
    state_fa2_state in

  assert (fa2_get_total_kit_balance state.fa2_state = fa2_get_total_kit_balance state_fa2_state); (* preservation of kit *)
  assert (fa2_get_total_lqt_balance state.fa2_state = Ligo.add_nat_nat (fa2_get_total_lqt_balance state_fa2_state) lqt_burned);

  let state =
    { state with
      cfmm = updated_cfmm;
      fa2_state = state_fa2_state;
    } in

  assert_checker_invariants state;

  ([op], state)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

let entrypoint_liquidation_auction_place_bid (state, kit: checker * kit) : LigoOp.operation list * checker =
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in

  let bid = { address=(!Ligo.Tezos.sender); kit=kit; } in
  let current_auction = liquidation_auction_get_current_auction state.liquidation_auctions in

  let (new_current_auction, old_winning_bid) = liquidation_auction_place_bid current_auction bid in

  (* Update the fa2_state: (a) restore the old winning bid from checker to its
   * original owner (if such a bid exists), and (b) move [kit] from the
   * bidder's account to checker's account. By performing the operation in this
   * order we allow users to just increase their bid without having to have in
   * their accounts [old_bid + new_bid] kit; having [new_bid] is enough. *)
  let state_fa2_state =
    let state_fa2_state = state.fa2_state in
    (* credit the old winning bid to its owner, if necessary *)
    let state_fa2_state =
      match old_winning_bid with
      | None -> state_fa2_state (* nothing to do *)
      | Some old_winning_bid ->
        let state_fa2_state = ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.self_address, old_winning_bid.kit) in
        let state_fa2_state = ledger_issue_kit (state_fa2_state, old_winning_bid.address, old_winning_bid.kit) in
        state_fa2_state in
    assert (fa2_get_total_kit_balance state.fa2_state = fa2_get_total_kit_balance state_fa2_state); (* preservation of kit *)

    (* credit the new winning bid to checker *)
    let state_fa2_state = ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.sender, kit) in
    let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.self_address, kit) in
    assert (fa2_get_total_kit_balance state.fa2_state = fa2_get_total_kit_balance state_fa2_state); (* preservation of kit *)
    state_fa2_state in

  let state =
    { state with
      liquidation_auctions=
        { state.liquidation_auctions with
          current_auction = Some new_current_auction;
        };
      fa2_state = state_fa2_state;
    } in

  assert_checker_invariants state;

  (([]: LigoOp.operation list), state)

let entrypoint_liquidation_auction_claim_win (state, auction_id: checker * liquidation_auction_id) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in
  let _ = ensure_valid_avl_ptr state.liquidation_auctions.avl_storage auction_id in
  let (tez, liquidation_auctions) = liquidation_auction_claim_win state.liquidation_auctions auction_id in
  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit Ligo.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () tez c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in
  let state = {state with liquidation_auctions = liquidation_auctions } in
  assert_checker_invariants state;

  ([op], state)

let[@inline] entrypoint_receive_slice_from_burrow (state, burrow_id: checker * burrow_id) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let burrow = find_burrow state.burrows burrow_id in (* only accept from burrows! *)
  if !Ligo.Tezos.sender = burrow_address burrow
  then (([]: LigoOp.operation list), state)
  else (Ligo.failwith error_AuthenticationError : (LigoOp.operation list * checker))

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
    (ops, state_liquidation_auctions, state_burrows, old_kit_to_repay, old_kit_to_burn, maximum: LigoOp.operation list * liquidation_auctions * burrow_map * kit * kit * int)
  : (LigoOp.operation list * liquidation_auctions * burrow_map * kit * kit) =
  if maximum <= 0 then
    (ops, state_liquidation_auctions, state_burrows, old_kit_to_repay, old_kit_to_burn)
  else
    match liquidation_auction_oldest_completed_liquidation_slice state_liquidation_auctions with
    | None -> (ops, state_liquidation_auctions, state_burrows, old_kit_to_repay, old_kit_to_burn)
    | Some leaf ->
      let new_ops, new_state_liquidation_auctions, new_state_burrows, new_kit_to_repay, new_kit_to_burn =
        touch_liquidation_slice ops state_liquidation_auctions state_burrows leaf in
      touch_oldest
        ( new_ops,
          new_state_liquidation_auctions,
          new_state_burrows,
          kit_add old_kit_to_repay new_kit_to_repay,
          kit_add old_kit_to_burn new_kit_to_burn,
          maximum - 1
        )

(* NOTE: The list of operations returned is in reverse order (with respect to
 * the order in which the things are expected to happen). However, all inputs
 * to those operations are computed in the correct order, and, with two
 * exceptions (1. setting the delegate, and 2. call/callback to the oract), all
 * of the operations are outwards calls, to other contracts (no callbacks). It
 * should be safe to leave the order of the transaction reversed. *)
let touch_with_index (state: checker) (index:Ligo.tez) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let
    { burrows = state_burrows;
      cfmm = state_cfmm;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      last_price = state_last_price;
      fa2_state = state_fa2_state;
      external_contracts = state_external_contracts;
    } = state in
  assert (state.parameters.last_touched <= !Ligo.Tezos.now); (* FIXME: I think this should be translated to LIGO actually. *)
  let _ = ensure_no_tez_given () in
  if state_parameters.last_touched = !Ligo.Tezos.now then
    (* Do nothing if up-to-date (idempotence) *)
    (([]: LigoOp.operation list), state)
  else
    (* TODO: What is the right order in which to do things here? We use the
     * last observed kit_in_tez price from cfmm to update the parameters,
     * which return kit to be added to the cfmm contract. Gotta make sure we
     * do things in the right order here. *)

    (* 1: Mint some kit out of thin air to reward the contract toucher, and
     * update the circulating kit accordingly.*)
    let state_parameters, state_fa2_state =
      let reward = calculate_touch_reward state_parameters.last_touched in
      let state_parameters = add_circulating_kit state_parameters reward in
      let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.sender, reward) in
      state_parameters, state_fa2_state in

    (* 2: Update the system parameters and add accrued burrowing fees to the
     * cfmm sub-contract. *)
    let state_parameters, state_cfmm, state_fa2_state =
      let kit_in_tez_in_prev_block = (cfmm_kit_in_ctez_in_prev_block state_cfmm) in (* FIXME: times ctez_in_tez *)
      let total_accrual_to_cfmm, state_parameters = parameters_touch index kit_in_tez_in_prev_block state_parameters in (* NOTE: circulating kit here already inlcludes the accrual to the CFMM. *)
      let state_cfmm = cfmm_add_accrued_kit state_cfmm total_accrual_to_cfmm in
      let state_fa2_state = ledger_issue_kit (state_fa2_state, !Ligo.Tezos.self_address, total_accrual_to_cfmm) in
      state_parameters, state_cfmm, state_fa2_state in

    (* 3: Update auction-related info (e.g. start a new auction). Note that we
     * always start auctions using the current liquidation price. We could also
     * have calculated the price right now directly using the oracle feed as
     * (tz_t * q_t), or use the current minting price, but using the
     * liquidation price is the safest option. *)
    let state_liquidation_auctions =
      liquidation_auction_touch state_liquidation_auctions (liquidation_price state_parameters) in

    (* 4: Touch oldest liquidation slices *)
    (* TODO: Touch only runs at most once per block. But it might be beneficial
     * to run this step without that restriction. *)

    (* Create an operation to ask the oracles to send updated values. This
       should be the last operation we emit, so that the system parameters do
       not change between touching different slices. *)
    let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receive_price" !Ligo.Tezos.self_address : (Ligo.nat Ligo.contract) option) with
      | Some cb -> cb
      | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : Ligo.nat Ligo.contract) in
    let op =
       LigoOp.Tezos.nat_contract_transaction
         cb
         (Ligo.tez_from_literal "0mutez")
         (get_oracle_entrypoint state_external_contracts) in

    (* TODO: Figure out how many slices we can process per checker entrypoint_touch.*)
    let ops, state =
      let ops, state_liquidation_auctions, state_burrows, kit_to_repay, kit_to_burn =
        touch_oldest ([op], state_liquidation_auctions, state_burrows, kit_zero, kit_zero, number_of_slices_to_process) in
      let state_parameters =
        let state_parameters = remove_outstanding_and_circulating_kit state_parameters kit_to_repay in
        let state_parameters = remove_circulating_kit state_parameters kit_to_burn in
        state_parameters in
      let state_fa2_state =
        ledger_withdraw_kit (state_fa2_state, !Ligo.Tezos.self_address, kit_add kit_to_repay kit_to_burn) in

      let new_state =
        { burrows = state_burrows;
          cfmm = state_cfmm;
          parameters = state_parameters;
          liquidation_auctions = state_liquidation_auctions;
          last_price = state_last_price;
          fa2_state = state_fa2_state;
          external_contracts = state_external_contracts;
        } in

      (ops, new_state) in

    assert_checker_invariants state;

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
  assert_checker_invariants state;
  let _ = ensure_no_tez_given () in
  if !Ligo.Tezos.sender <> state.external_contracts.oracle then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * checker)
  else
    (([]: LigoOp.operation list), {state with last_price = Some price})

(* ************************************************************************* *)
(**                               FA2                                        *)
(* ************************************************************************* *)

let strict_entrypoint_transfer (state, xs: checker * fa2_transfer list) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let state = { state with fa2_state = fa2_run_transfer (state.fa2_state, xs) } in
  assert_checker_invariants state;
  (([]: LigoOp.operation list), state)

let[@inline] strict_entrypoint_balance_of (state, param: checker * fa2_balance_of_param) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let { requests = requests; callback = callback; } = param in
  let response = fa2_run_balance_of (state.fa2_state, requests) in
  let op = LigoOp.Tezos.fa2_balance_of_response_transaction response (Ligo.tez_from_literal "0mutez") callback in
  assert_checker_invariants state;
  ([op], state)

let entrypoint_update_operators (state, xs: checker * fa2_update_operator list) : (LigoOp.operation list * checker) =
  assert_checker_invariants state;
  let state = { state with fa2_state = fa2_run_update_operators (state.fa2_state, xs) } in
  assert_checker_invariants state;
  (([]: LigoOp.operation list), state)

(* ************************************************************************* *)
(**                               VIEWS                                      *)
(* ************************************************************************* *)

let view_buy_kit_min_kit_expected (ctez, state: ctez * checker) : kit =
  assert_checker_invariants state;
  let (kit, _cfmm) = cfmm_view_min_kit_expected_buy_kit state.cfmm ctez in
  kit

let view_sell_kit_min_ctez_expected (kit, state: kit * checker) : ctez =
  assert_checker_invariants state;
  let (ctez, _cfmm) = cfmm_view_min_ctez_expected_cfmm_sell_kit state.cfmm kit in
  ctez

let view_add_liquidity_max_kit_deposited (ctez, state: ctez * checker) : kit =
  assert_checker_invariants state;
  let (_lqt, kit, _cfmm) = cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity state.cfmm ctez in
  kit

let view_add_liquidity_min_lqt_minted (ctez, state: ctez * checker) : liquidity =
  assert_checker_invariants state;
  let (lqt, _kit, _cfmm) = cfmm_view_max_kit_deposited_min_lqt_minted_cfmm_add_liquidity state.cfmm ctez in
  lqt

let view_remove_liquidity_min_ctez_withdrawn (lqt, state: liquidity * checker) : ctez =
  assert_checker_invariants state;
  let (ctez, _kit, _cfmm) = cfmm_view_min_ctez_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity state.cfmm lqt in
  ctez

let view_remove_liquidity_min_kit_withdrawn (lqt, state: liquidity * checker) : kit =
  assert_checker_invariants state;
  let (_ctez, kit, _cfmm) = cfmm_view_min_ctez_withdrawn_min_kit_withdrawn_cfmm_remove_liquidity state.cfmm lqt in
  kit

let view_burrow_max_mintable_kit (burrow_id, state: burrow_id * checker) : kit =
  assert_checker_invariants state;
  burrow_max_mintable_kit state.parameters (find_burrow state.burrows burrow_id)

let view_is_burrow_overburrowed (burrow_id, state: burrow_id * checker) : bool =
  assert_checker_invariants state;
  burrow_is_overburrowed state.parameters (find_burrow state.burrows burrow_id)

let view_is_burrow_liquidatable (burrow_id, state: burrow_id * checker) : bool =
  assert_checker_invariants state;
  burrow_is_liquidatable state.parameters (find_burrow state.burrows burrow_id)

(* ************************************************************************* *)
(**                            FA2_VIEWS                                     *)
(* ************************************************************************* *)

let view_get_balance ((owner, token_id), state: (Ligo.address * fa2_token_id) * checker) : Ligo.nat =
  assert_checker_invariants state;
  fa2_get_balance (state.fa2_state, owner, token_id)

let view_total_supply (token_id, state: fa2_token_id * checker) : Ligo.nat =
  assert_checker_invariants state;
  if token_id = kit_token_id then
    kit_to_mukit_nat state.parameters.circulating_kit
  else if token_id = liquidity_token_id then
    begin
      match Ligo.is_nat (Ligo.sub_nat_nat state.cfmm.lqt (Ligo.nat_from_literal "1n")) with
      | None -> failwith "impossible"
      | Some lqt -> lqt
    end
  else
    failwith "FA2_TOKEN_UNDEFINED"

let view_all_tokens ((), _state: unit * checker) : fa2_token_id list =
  assert_checker_invariants _state;
  fa2_all_tokens

let view_is_operator ((owner, (operator, token_id)), state: (Ligo.address * (Ligo.address * fa2_token_id)) * checker): bool =
  assert_checker_invariants state;
  fa2_is_operator (state.fa2_state, owner, operator, token_id)

(* TODO
This corresponds to the "Custom" method specified in TZIP-12 [1]. We should either implement this one or the "Basic" method.

[1]: https://gitlab.com/tzip/tzip/-/blob/4b3c67/proposals/tzip-12/tzip-12.md#token-metadata-storage-access

let view_token_metadata (_token_id, _state: fa2_token_id * checker) : fa2_token_id * (string, Ligo.bytes) Ligo.map =
  failwith "FA2_NOT_IMPLEMENTED"
*)
