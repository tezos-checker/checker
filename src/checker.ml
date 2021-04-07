open Ratio
open Kit
open Avl
open Permission
open Parameters
open Uniswap
open Burrow
open DelegationAuction
open DelegationAuctionTypes
open LiquidationAuction
open LiquidationAuctionPrimitiveTypes
open LiquidationAuctionTypes
open Common
open Constants
open Tickets
open BurrowTypes
open CheckerTypes
open Error

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

type ty_create_burrow = checker * Ligo.key_hash option ->  LigoOp.operation list * checker
let[@inline] create_burrow (state, delegate_opt: checker * Ligo.key_hash option) =
  let burrow = burrow_create state.parameters !Ligo.Tezos.amount delegate_opt in
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
             let op = match (LigoOp.Tezos.get_contract_opt addr : unit LigoOp.contract option) with
               | Some c -> LigoOp.Tezos.unit_transaction () tez c
               | None -> (failwith "B2" : LigoOp.operation) in
             ([op], s)
           | BurrowSendSliceToChecker tz ->
             let op = match (LigoOp.Tezos.get_entrypoint_opt "%receiveLiquidationSlice" !Ligo.Tezos.sender : unit LigoOp.contract option) with
               | Some c -> LigoOp.Tezos.unit_transaction () tz c
               | None -> (failwith "B3" : LigoOp.operation) in
             ([op], s)
      )
      delegate_opt
      !Ligo.Tezos.amount (* NOTE!!! The creation deposit is in the burrow too, even if we don't consider it to be collateral! *)
      checker_address in
  let admin_ticket = issue_permission_ticket Admin burrow_address (Ligo.nat_from_literal "0n") in
  let op2 = match (LigoOp.Tezos.get_entrypoint_opt "%transferPermission" !Ligo.Tezos.sender : permission LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.perm_transaction admin_ticket (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferPermission : LigoOp.operation) in

  let op3 = match (LigoOp.Tezos.get_entrypoint_opt "%transferAddress" !Ligo.Tezos.sender : Ligo.address LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.address_transaction burrow_address (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferAddress : LigoOp.operation) in

  let updated_state = {state with burrows = Ligo.Big_map.update burrow_address (Some burrow) state.burrows} in

  ([op1; op2; op3], updated_state)

type ty_touch_burrow = checker * burrow_id -> LigoOp.operation list * checker
let touch_burrow (state, burrow_id: checker * burrow_id) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let updated_burrow = burrow_touch state.parameters burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  (([]: LigoOp.operation list), state)

type ty_deposit_tez = checker * (permission option * burrow_id) -> LigoOp.operation list * checker
let deposit_tez (state, p: checker * (permission option * burrow_id)) : (LigoOp.operation list * checker) =
  let permission, burrow_id = p in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let is_allowed =
    if burrow_allow_all_tez_deposits burrow then
      true
    else
      let permission = ensure_permission_is_present permission in
      let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
      does_right_allow_tez_deposits r
  in
  if is_allowed then
    let updated_burrow = burrow_deposit_tez state.parameters !Ligo.Tezos.amount burrow in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" burrow_id : unit LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
    ([op], {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows})
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_mint_kit = checker * (permission * burrow_id * kit) -> LigoOp.operation list * checker
let mint_kit (state, p: checker * (permission * burrow_id * kit)) : LigoOp.operation list * checker =
  let permission, burrow_id, kit = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if does_right_allow_kit_minting r then
    (* the permission should support minting kit. *)
    let burrow = burrow_mint_kit state.parameters kit burrow in
    let kit_tokens = kit_issue kit in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation) in
    let state =
      { state with
        burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows;
        parameters = add_outstanding_and_circulating_kit state.parameters kit;
      } in
    ([op], state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_withdraw_tez = checker * (permission * Ligo.tez * burrow_id) -> LigoOp.operation list * checker
let withdraw_tez (state, p: checker * (permission * Ligo.tez * burrow_id)) : LigoOp.operation list * checker =
  let (permission, tez, burrow_id) = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if does_right_allow_tez_withdrawals r then
    (* the permission should support withdrawing tez. *)
    let burrow = burrow_withdraw_tez state.parameters tez burrow in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows} in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id : (Ligo.tez * Ligo.address) LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (tez, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
    ([op], state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_burn_kit = checker * (permission option * burrow_id * kit_token) -> LigoOp.operation list * checker
let burn_kit (state, p: checker * (permission option * burrow_id * kit_token)) : LigoOp.operation list * checker =
  let permission, burrow_id, kit = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let kit = ensure_valid_kit_token kit in (* destroyed *)
  let is_allowed =
    if burrow_allow_all_kit_burnings burrow then
      true
    else
      let permission = ensure_permission_is_present permission in
      let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
      does_right_allow_kit_burning r
  in
  if is_allowed then
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
      } in
    (([]: LigoOp.operation list), state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_activate_burrow = checker * (permission * burrow_id) -> LigoOp.operation list * checker
let activate_burrow (state, p: checker * (permission * burrow_id)) : LigoOp.operation list * checker =
  let permission, burrow_id = p in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if is_admin_right r then
    (* only admins can activate burrows. *)
    let updated_burrow = burrow_activate state.parameters !Ligo.Tezos.amount burrow in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" burrow_id : unit LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    ([op], state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_deactivate_burrow = checker * (permission * burrow_id) -> LigoOp.operation list * checker
let deactivate_burrow (state, p: checker * (permission * burrow_id)) : (LigoOp.operation list * checker) =
  let permission, burrow_id = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if is_admin_right r then
    (* only admins (and checker itself, due to liquidations) can deactivate burrows. *)
    let (updated_burrow, returned_tez) = burrow_deactivate state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id : (Ligo.tez * Ligo.address) LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (returned_tez, !Ligo.Tezos.sender) (Ligo.tez_from_literal "0mutez") c (* NOTE: returned_tez inlcudes creation deposit! *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
    ([op], updated_state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_set_burrow_delegate = checker * (permission * burrow_id * Ligo.key_hash option) -> LigoOp.operation list * checker
let set_burrow_delegate (state, p: checker * (permission * burrow_id * Ligo.key_hash option)) : LigoOp.operation list * checker =
  let permission, burrow_id, delegate_opt = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if does_right_allow_setting_delegate r then
    (* the permission should support setting the delegate. *)
    let updated_burrow = burrow_set_delegate state.parameters delegate_opt burrow in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSetDelegate" burrow_id : Ligo.key_hash option LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.opt_key_hash_transaction delegate_opt (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSetDelegate : LigoOp.operation) in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    ([op], state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_make_permission = checker * (permission * burrow_id * rights) -> LigoOp.operation list * checker
let make_permission (state, p: checker * (permission * burrow_id * rights)) : LigoOp.operation list * checker =
  let permission, burrow_id, right = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if is_admin_right r then
    (* only admins can create permissions. *)
    let ticket = issue_permission_ticket right burrow_id (burrow_permission_version burrow) in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferPermission" !Ligo.Tezos.sender : permission LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.perm_transaction ticket (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferPermission : LigoOp.operation) in
    ([op], state) (* unchanged state *)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

type ty_invalidate_all_permissions = checker * (permission * burrow_id) -> LigoOp.operation list * checker
let invalidate_all_permissions (state, p: checker * (permission * burrow_id)) : LigoOp.operation list * checker =
  let permission, burrow_id = p in
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if is_admin_right r then
    (* only admins can invalidate all permissions. *)
    let updated_version, updated_burrow = burrow_increase_permission_version state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let admin_ticket = issue_permission_ticket Admin burrow_id updated_version in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferPermission" !Ligo.Tezos.sender : permission LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.perm_transaction admin_ticket (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferPermission : LigoOp.operation) in
    ([op], updated_state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

(* TODO: Arthur: one time we might want to trigger garbage collection of
 * slices is during a liquidation. a liquidation creates one slice, so if we
 * clear one pending slice when that happens it won't grow unbounded (yes,
 * there are degenerate cases where the queue starts growing much faster that
 * the auctions are happening and in those instances it could grow unbounded,
 * but roughly speaking in most cases it should average out) *)
type ty_mark_for_liquidation = checker * burrow_id -> LigoOp.operation list * checker
let[@inline] mark_for_liquidation (state, burrow_id: checker * burrow_id) : (LigoOp.operation list * checker) =
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

  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () liquidation_reward c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in

  ( [op],
    { state with
      burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
      liquidation_auctions = updated_liquidation_auctions;
    }
  )


(* Cancel the liquidation of a slice. *)
type ty_cancel_liquidation_slice = checker * (permission * leaf_ptr) -> LigoOp.operation list * checker
let cancel_liquidation_slice ((state, (permission, leaf_ptr)): checker * (permission * leaf_ptr)) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let (cancelled, auctions) = liquidation_auctions_cancel_slice state.liquidation_auctions leaf_ptr in
  let burrow = find_burrow state.burrows cancelled.burrow in
  let r = ensure_valid_permission permission cancelled.burrow (burrow_permission_version burrow) in
  if not (does_right_allow_cancelling_liquidations r) then
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)
  else
    let burrow = burrow_return_slice_from_auction cancelled burrow in
    let state =
      { state with
        burrows = Ligo.Big_map.add cancelled.burrow burrow state.burrows;
        liquidation_auctions = auctions;
      } in
    assert_checker_invariants state;
    (([]:  LigoOp.operation list), state)

(* NOTE: It prepends the operation to the list of operations given. This means
 * that if we touch a list of liquidation slices, the order of operations is
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
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendSliceToChecker" slice.burrow : Ligo.tez LigoOp.contract option) with
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
type ty_touch_liquidation_slices = checker * leaf_ptr list -> LigoOp.operation list * checker
let[@inline] touch_liquidation_slices (state, slices: checker * leaf_ptr list): (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  (* NOTE: the order of the operations is reversed here (wrt to the order of
   * the slices), but hopefully we don't care in this instance about this. *)
  let
    { burrows = state_burrows;
      uniswap = state_uniswap;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      delegation_auction = state_delegation_auction;
      delegate = state_delegate;
      last_price = state_last_price;
    } = state in

  let new_ops, state_liquidation_auctions, state_burrows, kit_to_burn =
    touch_liquidation_slices_rec (([]: LigoOp.operation list), state_liquidation_auctions, state_burrows, kit_zero, slices) in
  let state_parameters = remove_circulating_kit state_parameters kit_to_burn in

  let new_state =
    { burrows = state_burrows;
      uniswap = state_uniswap;
      parameters = state_parameters;
      liquidation_auctions = state_liquidation_auctions;
      delegation_auction = state_delegation_auction;
      delegate = state_delegate;
      last_price = state_last_price;
    } in
  assert_checker_invariants new_state;
  (new_ops, new_state)

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

let updated_delegation_auction (state: checker) (new_auction: delegation_auction) =
  let prev_auction = state.delegation_auction in
  (* When we move to a new cycle, we accrue the amount that won delegation for
     the previous cycle to uniswap. *)
  let accrued_tez =
    if delegation_auction_cycle prev_auction <> delegation_auction_cycle new_auction then
      match delegation_auction_winning_amount prev_auction with
      | None -> Ligo.tez_from_literal "0mutez"
      | Some tez -> tez
    else
      Ligo.tez_from_literal "0mutez"
  in
  let new_delegate = delegation_auction_delegate new_auction in

  let ops = if state.delegate = new_delegate then
      ([]: LigoOp.operation list)
    else [LigoOp.Tezos.set_delegate new_delegate]
  in
  (ops,

   { state with
     delegation_auction = new_auction;
     delegate = delegation_auction_delegate new_auction;
     uniswap = uniswap_add_accrued_tez state.uniswap accrued_tez;
   })

type ty_checker_delegation_auction_place_bid = checker -> LigoOp.operation list * checker
let checker_delegation_auction_place_bid (state: checker) : (LigoOp.operation list * checker) =
  let bid, auction =
    delegation_auction_place_bid
      state.delegation_auction
      !Ligo.Tezos.sender
      !Ligo.Tezos.amount
  in
  let ticket = issue_delegation_auction_bid_ticket bid in
  let (ops, new_state) = updated_delegation_auction state auction in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferDABidTicket" !Ligo.Tezos.sender : delegation_auction_bid_content Ligo.ticket LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.da_bid_transaction ticket (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferDABidTicket : LigoOp.operation list) in
  (ops, new_state)

type ty_checker_delegation_auction_claim_win = checker * (delegation_auction_bid_ticket * Ligo.key_hash) -> LigoOp.operation list * checker
let checker_delegation_auction_claim_win (state, p: checker * (delegation_auction_bid_ticket * Ligo.key_hash)) : LigoOp.operation list * checker =
  let bid_ticket, for_delegate = p in
  let _ = ensure_no_tez_given () in
  let bid = ensure_valid_delegation_auction_bid_ticket bid_ticket in
  let auction = delegation_auction_claim_win state.delegation_auction bid for_delegate in
  updated_delegation_auction state auction

type ty_checker_delegation_auction_reclaim_bid = checker * delegation_auction_bid_ticket -> LigoOp.operation list * checker
let checker_delegation_auction_reclaim_bid (state, bid_ticket: checker * delegation_auction_bid_ticket) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in
  let bid = ensure_valid_delegation_auction_bid_ticket bid_ticket in
  let tez, auction = delegation_auction_reclaim_bid state.delegation_auction bid in
  let ops, new_auction = updated_delegation_auction state auction in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation list) in
  (ops, new_auction)

let touch_delegation_auction (state: checker) =
  updated_delegation_auction state (delegation_auction_touch state.delegation_auction)

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

type ty_buy_kit = checker * (kit * Ligo.timestamp) -> LigoOp.operation list * checker
let buy_kit (state, p: checker * (kit * Ligo.timestamp)) : LigoOp.operation list * checker =
  let min_kit_expected, deadline = p in
  let (ops, state) = touch_delegation_auction state in
  let (kit_tokens, updated_uniswap) = uniswap_buy_kit state.uniswap !Ligo.Tezos.amount min_kit_expected deadline in
  let kit_tokens = kit_issue kit_tokens in (* Issue them here!! *)
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation list) in
  (ops, {state with uniswap = updated_uniswap})

type ty_sell_kit = checker * (kit_token * Ligo.tez * Ligo.timestamp) -> LigoOp.operation list * checker
let sell_kit (state, p: checker * (kit_token * Ligo.tez * Ligo.timestamp)) : LigoOp.operation list * checker =
  let kit, min_tez_expected, deadline = p in
  let _ = ensure_no_tez_given () in
  let (ops, state) = touch_delegation_auction state in
  let kit = ensure_valid_kit_token kit in (* destroyed *)
  let (tez, updated_uniswap) = uniswap_sell_kit state.uniswap !Ligo.Tezos.amount kit min_tez_expected deadline in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation list) in
  let updated_state = {state with uniswap = updated_uniswap} in
  (ops, updated_state)

type ty_add_liquidity = checker * (kit_token * Ligo.nat * Ligo.timestamp) -> LigoOp.operation list * checker
let add_liquidity (state, p: checker * (kit_token * Ligo.nat * Ligo.timestamp)) : LigoOp.operation list * checker =
  let max_kit_deposited, min_lqt_minted, deadline = p in
  let (ops, state) = touch_delegation_auction state in
  let pending_accrual = match delegation_auction_winning_amount state.delegation_auction with
    | None -> Ligo.tez_from_literal "0mutez"
    | Some tez -> tez in
  let max_kit_deposited = ensure_valid_kit_token max_kit_deposited in (* destroyed *)
  let (lqt_tokens, kit_tokens, updated_uniswap) =
    uniswap_add_liquidity state.uniswap !Ligo.Tezos.amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  let lqt_tokens = issue_liquidity_tokens lqt_tokens in (* Issue them here!! *)
  let kit_tokens = kit_issue kit_tokens in (* Issue them here!! *)
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation list) in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferLqt" !Ligo.Tezos.sender : liquidity LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.lqt_transaction lqt_tokens (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferLqt : LigoOp.operation list) in
  (ops, {state with uniswap = updated_uniswap})

type ty_remove_liquidity = checker * (liquidity * Ligo.tez * kit * Ligo.timestamp) -> LigoOp.operation list * checker
let remove_liquidity (state, p: checker * (liquidity * Ligo.tez * kit * Ligo.timestamp)) : LigoOp.operation list * checker =
  let lqt_burned, min_tez_withdrawn, min_kit_withdrawn, deadline = p in
  let _ = ensure_no_tez_given () in
  let (ops, state) = touch_delegation_auction state in
  let lqt_burned = ensure_valid_liquidity_token lqt_burned in
  let (_, (_, lqt_burned)), _ = Ligo.Tezos.read_ticket lqt_burned in (* NOTE: consumed, right here. *)
  let (tez, kit_tokens, updated_uniswap) =
    uniswap_remove_liquidity state.uniswap !Ligo.Tezos.amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  let kit_tokens = kit_issue kit_tokens in (* Issue them here!! *)
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation list) in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation list) in
  let updated_state = {state with uniswap = updated_uniswap} in
  (ops, updated_state)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

type ty_checker_liquidation_auction_place_bid = checker * kit_token -> LigoOp.operation list * checker
let checker_liquidation_auction_place_bid (state, kit: checker * kit_token) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in
  let kit = ensure_valid_kit_token kit in (* destroyed *)

  let bid = { address=(!Ligo.Tezos.sender); kit=kit; } in
  let current_auction = liquidation_auction_get_current_auction state.liquidation_auctions in

  let (new_current_auction, bid_details) = liquidation_auction_place_bid current_auction bid in
  let bid_ticket = issue_liquidation_auction_bid_ticket bid_details in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferLABidTicket" !Ligo.Tezos.sender : liquidation_auction_bid_content Ligo.ticket LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.la_bid_transaction bid_ticket (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferLABidTicket : LigoOp.operation) in
  ( [op],
    { state with
      liquidation_auctions=
        { state.liquidation_auctions with
          current_auction = Some new_current_auction;
        };
    }
  )

type ty_checker_liquidation_auction_reclaim_bid = checker * liquidation_auction_bid_ticket -> LigoOp.operation list * checker
let checker_liquidation_auction_reclaim_bid (state, bid_ticket: checker * liquidation_auction_bid_ticket) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let bid_details = ensure_valid_liquidation_auction_bid_ticket bid_ticket in
  let kit = liquidation_auction_reclaim_bid state.liquidation_auctions bid_details in
  let kit_tokens = kit_issue kit in (* TODO: should not issue; should change the auction logic instead! *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation) in
  ([op], state) (* NOTE: unchanged state. It's a little weird that we don't keep track of how much kit has not been reclaimed. *)

type ty_checker_liquidation_auction_reclaim_winning_bid = checker * liquidation_auction_bid_ticket -> LigoOp.operation list * checker
let checker_liquidation_auction_reclaim_winning_bid (state, bid_ticket: checker * liquidation_auction_bid_ticket) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let bid_details = ensure_valid_liquidation_auction_bid_ticket bid_ticket in
  let (tez, liquidation_auctions) = liquidation_auction_reclaim_winning_bid state.liquidation_auctions bid_details in
  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () tez c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in
  ([op], {state with liquidation_auctions = liquidation_auctions })

(* TODO: Maybe we should provide an entrypoint for increasing a losing bid.
 * *)

type ty_receive_slice_from_burrow = checker -> LigoOp.operation list * checker
let[@inline] receive_slice_from_burrow (state: checker) : (LigoOp.operation list * checker) =
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
     * last observed kit_in_tez price from uniswap to update the parameters,
     * which return kit to be added to the uniswap contract. Gotta make sure we
     * do things in the right order here. *)

    (* 1: Calculate the reward that we should create out of thin air to give
     * to the contract toucher, and update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state.parameters.last_touched in
    let state = { state with parameters = add_circulating_kit state.parameters reward } in

    (* Ensure the delegation auction is up-to-date, and any proceeds accrued to the uniswap *)
    let (ops, state) = touch_delegation_auction state in

    (* 2: Update the system parameters *)
    let total_accrual_to_uniswap, updated_parameters =
      parameters_touch index (uniswap_kit_in_tez_in_prev_block state.uniswap) state.parameters
    in
    (* 3: Add accrued burrowing fees to the uniswap sub-contract *)
    let updated_uniswap = uniswap_add_accrued_kit state.uniswap total_accrual_to_uniswap in

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
        uniswap = updated_uniswap;
        liquidation_auctions = updated_liquidation_auctions;
      } in

    (* TODO: Figure out how many slices we can process per checker touch.*)
    let ops, state =
      let
        { burrows = state_burrows;
          uniswap = state_uniswap;
          parameters = state_parameters;
          liquidation_auctions = state_liquidation_auctions;
          delegation_auction = state_delegation_auction;
          delegate = state_delegate;
          last_price = state_last_price;
        } = state in
      let ops, state_liquidation_auctions, state_burrows, kit_to_burn =
        touch_oldest (ops, state_liquidation_auctions, state_burrows, kit_zero, number_of_slices_to_process) in
      let state_parameters = remove_circulating_kit state_parameters kit_to_burn in
      let new_state =
        { burrows = state_burrows;
          uniswap = state_uniswap;
          parameters = state_parameters;
          liquidation_auctions = state_liquidation_auctions;
          delegation_auction = state_delegation_auction;
          delegate = state_delegate;
          last_price = state_last_price;
        } in
      (ops, new_state) in

    assert_checker_invariants state;

    let kit_tokens = kit_issue reward in
    let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation list) in

    (* Create operations to ask the oracles to send updated values. *)
    let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receivePrice" !Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
      | Some cb -> cb
      | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : Ligo.nat LigoOp.contract) in
    let oracle = match (LigoOp.Tezos.get_entrypoint_opt oracle_entrypoint oracle_address : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
      | Some c -> c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint : (Ligo.nat LigoOp.contract) LigoOp.contract) in
    let op = LigoOp.Tezos.nat_contract_transaction cb (Ligo.tez_from_literal "0mutez") oracle in
    let ops = (op :: ops) in (* FIXME: op should be at the end, not the beginning *)

    (ops, state)

type ty_touch = checker -> LigoOp.operation list * checker
let touch (state: checker) : (LigoOp.operation list * checker) =
  let index = match state.last_price with
    | None -> state.parameters.index (* use the old one *)
    | Some i -> Ligo.mul_nat_tez i (Ligo.tez_from_literal "1mutez") in (* FIXME: Is the nat supposed to represent tez? *)
  touch_with_index state index

(* ************************************************************************* *)
(**                               ORACLE                                     *)
(* ************************************************************************* *)

type ty_receive_price = checker * Ligo.nat -> LigoOp.operation list * checker
let receive_price (state, price: checker * Ligo.nat) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in
  if !Ligo.Tezos.sender <> oracle_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * checker)
  else
    (([]: LigoOp.operation list), {state with last_price = Some price})

(* ENTRYPOINTS *)

type lazyFunctionId = int
let[@inline] lazy_fun_touch = 1
let[@inline] lazy_fun_create_burrow = 2
let[@inline] lazy_fun_touch_liquidation_slices = 3
let[@inline] lazy_fun_mark_for_liquidation = 4
let[@inline] lazy_fun_cancel_liquidation_slice = 5
let[@inline] lazy_fun_checker_liquidation_auction_reclaim_winning_bid = 6
let[@inline] lazy_fun_deposit_tez = 7
let[@inline] lazy_fun_withdraw_tez = 8
let[@inline] lazy_fun_burn_kit = 9
let[@inline] lazy_fun_activate_burrow = 10
let[@inline] lazy_fun_deactivate_burrow = 11
let[@inline] lazy_fun_touch_burrow = 12
let[@inline] lazy_fun_set_burrow_delegate = 13
let[@inline] lazy_fun_make_permission = 14
let[@inline] lazy_fun_invalidate_all_permissions = 15
let[@inline] lazy_fun_buy_kit = 16
let[@inline] lazy_fun_sell_kit = 17
let[@inline] lazy_fun_add_liquidity = 18
let[@inline] lazy_fun_remove_liquidity = 19
let[@inline] lazy_fun_checker_liquidation_auction_place_bid = 20
let[@inline] lazy_fun_checker_liquidation_auction_reclaim_bid = 21
let[@inline] lazy_fun_receive_slice_from_burrow = 22
let[@inline] lazy_fun_checker_delegation_auction_place_bid = 23
let[@inline] lazy_fun_checker_delegation_auction_claim_win = 24
let[@inline] lazy_fun_checker_delegation_auction_reclaim_bid = 25
let[@inline] lazy_fun_receive_price = 26

type params =
  | Touch
  (* Burrows *)
  | CreateBurrow of Ligo.key_hash option
  | DepositTez of (permission option * burrow_id)
  | WithdrawTez of (permission * Ligo.tez * burrow_id)
  | MintKit of (permission * burrow_id * kit)
  | BurnKit of (permission option * burrow_id * kit_token)
  | ActivateBurrow of (permission * burrow_id)
  | DeactivateBurrow of (permission * burrow_id)
  | MarkBurrowForLiquidation of burrow_id
  | TouchLiquidationSlices of leaf_ptr list
  | CancelSliceLiquidation of (permission * leaf_ptr)
  | TouchBurrow of burrow_id
  | SetBurrowDelegate of (permission * burrow_id * Ligo.key_hash option)
  | MakePermission of (permission * burrow_id * rights)
  | InvalidateAllPermissions of (permission * burrow_id)
  (* Uniswap *)
  | BuyKit of (kit * Ligo.timestamp)
  | SellKit of (kit_token * Ligo.tez * Ligo.timestamp)
  | AddLiquidity of (kit_token * Ligo.nat * Ligo.timestamp)
  | RemoveLiquidity of (liquidity * Ligo.tez * kit * Ligo.timestamp)
  (* Liquidation Auction *)
  | LiqAuctionPlaceBid of kit_token
  | LiqAuctionReclaimBid of liquidation_auction_bid_ticket
  | LiqAuctionReclaimWinningBid of liquidation_auction_bid_ticket
  | ReceiveLiquidationSlice
  (* Delegation Auction *)
  | DelegationAuctionPlaceBid
  | DelegationAuctionClaimWin of (delegation_auction_bid_ticket * Ligo.key_hash)
  | DelegationAuctionReclaimBid of delegation_auction_bid_ticket
  (* Oracles *)
  | ReceivePrice of Ligo.nat
  (* Deployment *)
  | DeployFunction of (lazyFunctionId * Ligo.bytes)
  | SealContract

type wrapper = checker * (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option
let initial_wrapper (addr: Ligo.address) =
  (initial_checker, (Ligo.Big_map.empty: (lazyFunctionId, Ligo.bytes) Ligo.big_map), Some addr)

let main (op_and_state: params * wrapper): LigoOp.operation list * wrapper =
  let op, state = op_and_state in
  let checker, lazy_functions, deployer = state in

  let ops, checker, lazy_functions, deployer = match deployer with
    | Some deployer -> begin
        let lazy_functions, deployer =
          if !Ligo.Tezos.sender = deployer
          then match op with
            | DeployFunction p ->
              let lfi, bs = p in
              let lazy_functions =
                match Ligo.Big_map.find_opt lfi lazy_functions with
                | None -> Ligo.Big_map.add lfi bs lazy_functions
                | Some prev -> Ligo.Big_map.add lfi (Ligo.Bytes.concat prev bs) lazy_functions in
              (lazy_functions, Some deployer)
            | SealContract ->
              (lazy_functions, (None: Ligo.address option))
            (* we really need wildcard patterns... *)
            | ActivateBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | AddLiquidity _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | BurnKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | BuyKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | CancelSliceLiquidation _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | CreateBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DeactivateBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DelegationAuctionClaimWin _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DelegationAuctionPlaceBid -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DelegationAuctionReclaimBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | DepositTez _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | InvalidateAllPermissions _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | LiqAuctionPlaceBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | LiqAuctionReclaimBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | LiqAuctionReclaimWinningBid _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | MakePermission _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | MarkBurrowForLiquidation _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | MintKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | ReceiveLiquidationSlice -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | ReceivePrice _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | RemoveLiquidity _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | SellKit _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | SetBurrowDelegate _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | Touch -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | TouchBurrow _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | TouchLiquidationSlices _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
            | WithdrawTez _ -> (Ligo.failwith error_ContractNotDeployed: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option)
          else (Ligo.failwith error_UnauthorisedCaller: (lazyFunctionId, Ligo.bytes) Ligo.big_map * Ligo.address option) in
        (([]: LigoOp.operation list), checker, lazy_functions, deployer)
      end
    | None -> begin
        (* BEGIN_LIGO
           let get_lazy_fun (m, k: (lazyFunctionId, Ligo.bytes) Ligo.big_map * lazyFunctionId): Ligo.bytes =
            match Ligo.Big_map.find_opt k m with
            | None -> (failwith "lazy function not found": Ligo.bytes)
            | Some f -> f in
           END_LIGO *)
        let ops, checker = match op with
          | Touch ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_touch)): ty_touch option) with
                 | None -> (failwith "lazy function not found": ty_touch)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (touch: ty_touch)
              (* END_OCAML *)
            in f checker
          (* Burrows *)
          | CreateBurrow p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_create_burrow)): ty_create_burrow option) with
                 | None -> (failwith "lazy function not found": ty_create_burrow)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (create_burrow: ty_create_burrow)
              (* END_OCAML *)
            in f (checker, p)
          | DepositTez p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_deposit_tez)): ty_deposit_tez option) with
                 | None -> (failwith "lazy function not found": ty_deposit_tez)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (deposit_tez: ty_deposit_tez)
              (* END_OCAML *)
            in f (checker, p)
          | WithdrawTez p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_withdraw_tez)): ty_withdraw_tez option) with
                 | None -> (failwith "lazy function not found": ty_withdraw_tez)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (withdraw_tez: ty_withdraw_tez)
              (* END_OCAML *)
            in f (checker, p)
          | MintKit p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_withdraw_tez)): ty_mint_kit option) with
                 | None -> (failwith "lazy function not found": ty_mint_kit)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (mint_kit: ty_mint_kit)
              (* END_OCAML *)
            in f (checker, p)
          | BurnKit p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_burn_kit)): ty_burn_kit option) with
                 | None -> (failwith "lazy function not found": ty_burn_kit)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (burn_kit: ty_burn_kit)
              (* END_OCAML *)
            in f (checker, p)
          | ActivateBurrow p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_activate_burrow)): ty_activate_burrow option) with
                 | None -> (failwith "lazy function not found": ty_activate_burrow)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (activate_burrow: ty_activate_burrow)
              (* END_OCAML *)
            in f (checker, p)
          | DeactivateBurrow p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_deactivate_burrow)): ty_deactivate_burrow option) with
                 | None -> (failwith "lazy function not found": ty_deactivate_burrow)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (deactivate_burrow: ty_deactivate_burrow)
              (* END_OCAML *)
            in f (checker, p)
          | MarkBurrowForLiquidation p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_mark_for_liquidation)): ty_mark_for_liquidation option) with
                 | None -> (failwith "lazy function not found": ty_mark_for_liquidation)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (mark_for_liquidation: ty_mark_for_liquidation)
              (* END_OCAML *)
            in f (checker, p)
          | TouchLiquidationSlices p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_touch_liquidation_slices)): ty_touch_liquidation_slices option) with
                 | None -> (failwith "lazy function not found": ty_touch_liquidation_slices)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (touch_liquidation_slices: ty_touch_liquidation_slices)
              (* END_OCAML *)
            in f (checker, p)
          | CancelSliceLiquidation p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_cancel_liquidation_slice)): ty_cancel_liquidation_slice option) with
                 | None -> (failwith "lazy function not found": ty_cancel_liquidation_slice)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (cancel_liquidation_slice: ty_cancel_liquidation_slice)
              (* END_OCAML *)
            in f (checker, p)
          | TouchBurrow p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_touch_burrow)): ty_touch_burrow option) with
                 | None -> (failwith "lazy function not found": ty_touch_burrow)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (touch_burrow: ty_touch_burrow)
              (* END_OCAML *)
            in f (checker, p)
          | SetBurrowDelegate p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_set_burrow_delegate)): ty_set_burrow_delegate option) with
                 | None -> (failwith "lazy function not found": ty_set_burrow_delegate)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (set_burrow_delegate: ty_set_burrow_delegate)
              (* END_OCAML *)
            in f (checker, p)
          | MakePermission p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_make_permission)): ty_make_permission option) with
                 | None -> (failwith "lazy function not found": ty_make_permission)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (make_permission: ty_make_permission)
              (* END_OCAML *)
            in f (checker, p)
          | InvalidateAllPermissions p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_invalidate_all_permissions)): ty_invalidate_all_permissions option) with
                 | None -> (failwith "lazy function not found": ty_invalidate_all_permissions)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (invalidate_all_permissions: ty_invalidate_all_permissions)
              (* END_OCAML *)
            in f (checker, p)
          (* Uniswap *)
          | BuyKit p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_buy_kit)): ty_buy_kit option) with
                 | None -> (failwith "lazy function not found": ty_buy_kit)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (buy_kit: ty_buy_kit)
              (* END_OCAML *)
            in f (checker, p)
          | SellKit p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_sell_kit)): ty_sell_kit option) with
                 | None -> (failwith "lazy function not found": ty_sell_kit)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (sell_kit: ty_sell_kit)
              (* END_OCAML *)
            in f (checker, p)
          | AddLiquidity p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_add_liquidity)): ty_add_liquidity option) with
                 | None -> (failwith "lazy function not found": ty_add_liquidity)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (add_liquidity: ty_add_liquidity)
              (* END_OCAML *)
            in f (checker, p)
          | RemoveLiquidity p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_remove_liquidity)): ty_remove_liquidity option) with
                 | None -> (failwith "lazy function not found": ty_remove_liquidity)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (remove_liquidity: ty_remove_liquidity)
              (* END_OCAML *)
            in f (checker, p)
          (* Liquidation Auction *)
          | LiqAuctionPlaceBid p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_checker_liquidation_auction_place_bid)): ty_checker_liquidation_auction_place_bid option) with
                 | None -> (failwith "lazy function not found": ty_checker_liquidation_auction_place_bid)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (checker_liquidation_auction_place_bid: ty_checker_liquidation_auction_place_bid)
              (* END_OCAML *)
            in f (checker, p)
          | LiqAuctionReclaimBid p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_checker_liquidation_auction_reclaim_bid)): ty_checker_liquidation_auction_reclaim_bid option) with
                 | None -> (failwith "lazy function not found": ty_checker_liquidation_auction_reclaim_bid)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (checker_liquidation_auction_reclaim_bid: ty_checker_liquidation_auction_reclaim_bid)
              (* END_OCAML *)
            in f (checker, p)
          | LiqAuctionReclaimWinningBid p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_checker_liquidation_auction_reclaim_winning_bid)): ty_checker_liquidation_auction_reclaim_winning_bid option) with
                 | None -> (failwith "lazy function not found": ty_checker_liquidation_auction_reclaim_winning_bid)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (checker_liquidation_auction_reclaim_winning_bid: ty_checker_liquidation_auction_reclaim_winning_bid)
              (* END_OCAML *)
            in f (checker, p)
          | ReceiveLiquidationSlice ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_receive_slice_from_burrow)): ty_receive_slice_from_burrow option) with
                 | None -> (failwith "lazy function not found": ty_receive_slice_from_burrow)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (receive_slice_from_burrow: ty_receive_slice_from_burrow)
              (* END_OCAML *)
            in f checker
          (* Delegation Auction *)
          | DelegationAuctionPlaceBid ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_checker_delegation_auction_place_bid)): ty_checker_delegation_auction_place_bid option) with
                 | None -> (failwith "lazy function not found": ty_checker_delegation_auction_place_bid)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (checker_delegation_auction_place_bid: ty_checker_delegation_auction_place_bid)
              (* END_OCAML *)
            in f checker
          | DelegationAuctionClaimWin p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_checker_delegation_auction_claim_win)): ty_checker_delegation_auction_claim_win option) with
                 | None -> (failwith "lazy function not found": ty_checker_delegation_auction_claim_win)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (checker_delegation_auction_claim_win: ty_checker_delegation_auction_claim_win)
              (* END_OCAML *)
            in f (checker, p)
          | DelegationAuctionReclaimBid p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_checker_delegation_auction_reclaim_bid)): ty_checker_delegation_auction_reclaim_bid option) with
                 | None -> (failwith "lazy function not found": ty_checker_delegation_auction_reclaim_bid)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (checker_delegation_auction_reclaim_bid: ty_checker_delegation_auction_reclaim_bid)
              (* END_OCAML *)
            in f (checker, p)
          (* Oracles *)
          | ReceivePrice p ->
            let f =
              (* BEGIN_LIGO
                 match (Bytes.unpack (get_lazy_fun (lazy_functions, lazy_fun_receive_price)): ty_receive_price option) with
                 | None -> (failwith "lazy function not found": ty_receive_price)
                 | Some f -> f
                 END_LIGO *)
              (* BEGIN_OCAML *)
              (receive_price: ty_receive_price)
              (* END_OCAML *)
            in f (checker, p)
          (* Deployment *)
          | SealContract -> (Ligo.failwith error_ContractAlreadyDeployed: LigoOp.operation list * checker)
          | DeployFunction _ -> (Ligo.failwith error_ContractAlreadyDeployed: LigoOp.operation list * checker)
        in
        (ops, checker, lazy_functions, (None: Ligo.address option))
      end in

  (ops, (checker, lazy_functions, deployer))
