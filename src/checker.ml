open FixedPoint
open Ratio
open Kit
open Mem
open Avl
open Permission
open Parameters
open Uniswap
open Burrow
open DelegationAuction
open DelegationAuctionTypes
open LiquidationAuction
open LiquidationAuctionPrimitiveTypes
open Common
open Constants
open Tickets
open BurrowTypes
open CheckerTypes
open Error

(* TODO: At the very end, inline all numeric operations, flatten all ratio so
 * that we mainly deal with integers directly. Hardwire the constants too,
 * where possible. *)

(* BEGIN_OCAML *)
let assert_checker_invariants (state: checker) : unit =
  (* Check if the auction pointerfest kind of make sense. *)
  assert_liquidation_auction_invariants state.liquidation_auctions;
  (* Per-burrow assertions *)
  List.iter
    (fun (burrow_address, burrow) ->
       assert_burrow_invariants burrow;

       match burrow_liquidation_slices burrow with
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
           assert (slice.burrow = burrow_address);
           assert (slice.younger = prev);
           match slice.older with
           | Some next ->
             Ligo.add_tez_tez slice.tez (go next (Some curr))
           | None ->
             assert (curr = slices.oldest);
             slice.tez in
         let actual_collateral = go slices.youngest None in
         assert (burrow_collateral_at_auction burrow = actual_collateral)
    )
    (Ligo.Big_map.bindings state.burrows)
(* END_OCAML *)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

let is_burrow_done_with_liquidations (avl_storage: mem) (burrow: burrow) =
  match burrow_oldest_liquidation_ptr burrow with
  | None -> true
  | Some ls ->
    let root = avl_find_root avl_storage ls in
    let outcome = avl_root_data avl_storage root in
    (match outcome with
     | None -> true
     | Some _ -> false)

let find_burrow (burrows: burrow_map) (burrow_id: burrow_id) : burrow =
  match Ligo.Big_map.find_opt burrow_id burrows with
  | None -> (Ligo.failwith error_NonExistentBurrow : burrow)
  | Some burrow -> burrow

(* Looks up a burrow_id from state, and checks if the resulting burrow does
 * not have any completed liquidation slices that need to be claimed before
 * any operation. *)
let ensure_burrow_has_no_unclaimed_slices (avl_storage: mem) (burrow: burrow) : unit =
  if is_burrow_done_with_liquidations avl_storage burrow
  then ()
  else Ligo.failwith error_BurrowHasCompletedLiquidation

(* Ensure that there is no tez given. To prevent accidental fund loss. *)
let ensure_no_tez_given () =
  if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez"
  then Ligo.failwith error_UnwantedTezGiven
  else ()

let[@inline] create_burrow (state: checker) (delegate_opt: Ligo.key_hash option) =
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

let touch_burrow (state: checker) (burrow_id: burrow_id) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let updated_burrow = burrow_touch state.parameters burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  let ops : LigoOp.operation list = [] in
  (ops, state)

let deposit_tez (state: checker) (permission: permission option) (burrow_id: burrow_id) : (LigoOp.operation list * checker) =
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowStoreTez" burrow_id : unit LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () !Ligo.Tezos.amount c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowStoreTez : LigoOp.operation) in
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
    ([op], {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows})
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

let mint_kit (state: checker) (permission: permission) (burrow_id: burrow_id) (kit: kit) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if does_right_allow_kit_minting r then
    (* the permission should support minting kit. *)
    let burrow = burrow_mint_kit state.parameters kit burrow in
    let kit_tokens = kit_issue kit in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation) in
    let state =
      {state with
       burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows;
       parameters =
         add_outstanding_kit
           (add_circulating_kit state.parameters kit)
           kit;
      } in
    ([op], state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

let withdraw_tez (state: checker) (permission: permission) (tez: Ligo.tez) (burrow_id: burrow_id) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
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

let burn_kit (state: checker) (permission: permission option) (burrow_id: burrow_id) (kit: kit_token) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
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

let activate_burrow (state: checker) (permission: permission) (burrow_id: burrow_id) : (LigoOp.operation list * checker) =
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
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

let deactivate_burrow (state: checker) (permission: permission) (burrow_id: burrow_id) (recipient: Ligo.address) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if is_admin_right r then
    (* only admins (and checker itself, due to liquidations) can deactivate burrows. *)
    let (updated_burrow, returned_tez) = burrow_deactivate state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendTezTo" burrow_id : (Ligo.tez * Ligo.address) LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.tez_address_transaction (returned_tez, recipient) (Ligo.tez_from_literal "0mutez") c (* NOTE: returned_tez inlcudes creation deposit! *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendTezTo : LigoOp.operation) in
    ([op], updated_state)
  else
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)

let set_burrow_delegate (state: checker) (permission: permission) (burrow_id: burrow_id) (delegate_opt: Ligo.key_hash option) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
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

let make_permission (state: checker) (permission: permission) (burrow_id: burrow_id) (right: rights) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
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

let invalidate_all_permissions (state: checker) (permission: permission) (burrow_id: burrow_id) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in
  let _ = ensure_burrow_has_no_unclaimed_slices state.liquidation_auctions.avl_storage burrow in
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
let[@inline]  mark_for_liquidation (state: checker) (burrow_id: burrow_id) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let burrow = find_burrow state.burrows burrow_id in

  let details = match burrow_request_liquidation state.parameters burrow with
    | Unnecessary -> (Ligo.failwith error_NotLiquidationCandidate : liquidation_details)
    | Partial details -> details
    | Complete details -> details
    | Close details -> details
  in
  let liquidation_slice =
    {
      burrow = burrow_id;
      tez = details.tez_to_auction;
      min_kit_for_unwarranted = details.min_kit_for_unwarranted;
      older = (
        match burrow_liquidation_slices burrow with
        | None -> (None : leaf_ptr option)
        | Some i -> Some i.youngest
      );
      younger = (None: leaf_ptr option);
    } in
  let (updated_liquidation_auctions, leaf_ptr) =
    liquidation_auction_send_to_auction state.liquidation_auctions liquidation_slice in

  (* Fixup the previous youngest pointer since the newly added slice
   * is even younger.
   *
   * This is hacky, but couldn't figure out a nicer way, please do
   * refactor if you do.
  *)
  let updated_storage = (
    match liquidation_slice.older with
    | None -> updated_liquidation_auctions.avl_storage
    | Some older_ptr ->
      mem_update
        updated_liquidation_auctions.avl_storage
        (ptr_of_leaf_ptr older_ptr)
        (fun (older: node) ->
           let older = node_leaf older in
           Leaf { older with value = { older.value with younger = Some leaf_ptr; }; }
        )
  ) in

  (* Update the burrow's liquidation slices with the pointer to the newly
   * created liquidation slice. *)
  let updated_burrow =
    burrow_set_liquidation_slices
      details.burrow_state
      (match burrow_liquidation_slices details.burrow_state with
       | None -> Some { oldest=leaf_ptr; youngest=leaf_ptr; }
       | Some s -> Some { s with youngest=leaf_ptr; })
  in

  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () details.liquidation_reward c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in

  ( [op],
    {state with
     burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
     liquidation_auctions = { updated_liquidation_auctions with avl_storage = updated_storage; };
    }
  )

(* Update the immediate neighbors of a slice (i.e. the younger and the older)
 * to point to each other instead of the slice in question, so that it can be
 * removed. *)
(* NOTE: the liquidation slice must be the one pointed to by the leaf pointer. *)
let update_immediate_neighbors (state: checker) (leaf_ptr: leaf_ptr) (leaf : liquidation_slice) =
  (* update the younger *)
  let state = (
    match leaf.younger with
    | None -> state
    | Some younger_ptr ->
      { state with
        liquidation_auctions = { state.liquidation_auctions with
                                 avl_storage =
                                   avl_update_leaf
                                     state.liquidation_auctions.avl_storage
                                     younger_ptr
                                     (fun (younger: liquidation_slice) ->
                                        assert (younger.older = Some leaf_ptr);
                                        { younger with older = leaf.older }
                                     )
                               }}
  ) in
  (* update the older *)
  let state = (
    match leaf.older with
    | None -> state
    | Some older_ptr ->
      { state with
        liquidation_auctions = { state.liquidation_auctions with
                                 avl_storage =
                                   avl_update_leaf
                                     state.liquidation_auctions.avl_storage
                                     older_ptr
                                     (fun (older: liquidation_slice) ->
                                        assert (older.younger = Some leaf_ptr);
                                        { older with younger = leaf.younger }
                                     )
                               }}
  ) in
  state

(* Cancel the liquidation of a slice. The burden is on the caller to provide
 * both the burrow_id and the leaf_ptr. *)
let cancel_liquidation_slice (state: checker) (permission: permission) (leaf_ptr: leaf_ptr) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let leaf = avl_read_leaf state.liquidation_auctions.avl_storage leaf_ptr in
  let burrow_id = leaf.burrow in
  let burrow = find_burrow state.burrows burrow_id in
  let r = ensure_valid_permission permission burrow_id (burrow_permission_version burrow) in
  if not (does_right_allow_cancelling_liquidations r) then
    (Ligo.failwith error_InsufficientPermission : LigoOp.operation list * checker)
  else
    let root = avl_find_root state.liquidation_auctions.avl_storage leaf_ptr in
    if ptr_of_avl_ptr root <> ptr_of_avl_ptr state.liquidation_auctions.queued_slices
    then (Ligo.failwith error_UnwarrantedCancellation : LigoOp.operation list * checker)
    else
      let leaf = avl_read_leaf state.liquidation_auctions.avl_storage leaf_ptr in
      if burrow_is_overburrowed state.parameters burrow then
        (Ligo.failwith error_UnwarrantedCancellation : LigoOp.operation list * checker)
      else
        let state =
          let (new_storage, _) = avl_del state.liquidation_auctions.avl_storage leaf_ptr in
          { state with
            liquidation_auctions = {
              state.liquidation_auctions with
              avl_storage = new_storage }} in

        (* Return the tez to the burrow and update its pointers to liq. slices. *)
        let burrow = burrow_return_slice_from_auction leaf_ptr leaf burrow in
        let state =
          { state with
            burrows = Ligo.Big_map.update leaf.burrow (Some burrow) state.burrows } in

        (* And we update the slices around it *)
        let state = update_immediate_neighbors state leaf_ptr leaf in
        assert_checker_invariants state;
        let ops : LigoOp.operation list = [] in
        (ops, state)

(* FIXME: Below function shouldn't be inlined, since it's huge and used in multiple places. However otherwise `touch_oldest` function
 * throws a "type too large" error that I couldn't solve.
*)
let[@inline] touch_liquidation_slice (ops, state, leaf_ptr: LigoOp.operation list * checker * leaf_ptr) : (LigoOp.operation list * checker) =
  let root = avl_find_root state.liquidation_auctions.avl_storage leaf_ptr in
  match avl_root_data state.liquidation_auctions.avl_storage root with
  (* The slice does not belong to a completed auction, so we skip it. *)
  (* NOTE: Perhaps failing would be better than silently doing nothing here?
   * Not sure if there is any danger though. *)
  | None -> (ops, state)
  (* If it belongs to a completed auction, we delete the slice *)
  | Some outcome ->
    (* TODO: Check if leaf_ptr's are valid *)
    let leaf = avl_read_leaf state.liquidation_auctions.avl_storage leaf_ptr in

    (* How much kit should be given to the burrow and how much should be burned. *)
    (* NOTE: we treat each slice in a lot separately, so Sum(kit_to_repay_i +
     * kit_to_burn_i)_{1..n} might not add up to outcome.winning_bid.kit, due
     * to truncation. That could be a problem; the extra kit, no matter how
     * small, must be dealt with (e.g. be removed from the circulating kit). *)
    let kit_to_repay, kit_to_burn =
      let corresponding_kit =
        kit_of_ratio_floor
          (mul_ratio
             (make_ratio (tez_to_mutez leaf.tez) (tez_to_mutez outcome.sold_tez))
             (kit_to_ratio outcome.winning_bid.kit)
          ) in
      let penalty =
        if corresponding_kit < leaf.min_kit_for_unwarranted then
          kit_of_ratio_ceil (mul_ratio (kit_to_ratio corresponding_kit) liquidation_penalty)
        else
          kit_zero
      in
      (kit_sub corresponding_kit penalty, penalty)
    in

    (* Burn the kit by removing it from circulation. *)
    let state =
      { state with
        parameters = remove_circulating_kit state.parameters kit_to_burn } in

    (* Now we delete the slice from the lot, so it cannot be
     * withdrawn twice, also to save storage. This might cause
     * the lot root to change, so we also update completed_auctions
     * to reflect that.
     *
     * Deletion process also returns the tree root. *)
    let (state, auction) =
      let (new_storage, auction) = avl_del state.liquidation_auctions.avl_storage leaf_ptr in
      let new_state = { state with liquidation_auctions = { state.liquidation_auctions with avl_storage = new_storage }} in
      (new_state, auction) in

    (* When the auction has no slices left, we pop it from the linked list of lots. We do not
     * delete the auction itself from the storage, since we still want the winner to be able
     * to claim its result. *)
    let state =
      if avl_is_empty state.liquidation_auctions.avl_storage auction then
        { state with
          liquidation_auctions = liquidation_auction_pop_completed_auction state.liquidation_auctions auction;
        }
      else state in

    (* When we delete the youngest or the oldest slice, we have to adjust
     * the burrow pointers accordingly.
     *
     * TODO: We might not actually need to store this information, since
     * on every operation we might expect to get the first and last
     * elements of the linked list off-chain. However, this means that
     * the client would have to do a costly search across all the auction
     * queue to find at least one slice for the burrow.
    *)
    let state =
      { state with burrows =
                     let burrow = match Ligo.Big_map.find_opt leaf.burrow state.burrows with
                       | None -> (failwith "TODO: Check if this case can happen." : burrow)
                       | Some b -> b
                     in
                     Ligo.Big_map.update
                       leaf.burrow
                       (Some (burrow_return_kit_from_auction leaf_ptr leaf kit_to_repay burrow))
                       state.burrows
      } in

    (* And we update the slices around it *)
    let state = update_immediate_neighbors state leaf_ptr leaf in
    assert_checker_invariants state;

    (* Signal the burrow to send the tez to checker. *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%burrowSendSliceToChecker" leaf.burrow : Ligo.tez LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.tez_transaction leaf.tez (Ligo.tez_from_literal "0mutez") c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureBurrowSendSliceToChecker : LigoOp.operation) in
    ((op :: ops), state)

let rec touch_liquidation_slices_rec (ops, state, slices: LigoOp.operation list * checker * leaf_ptr list) : (LigoOp.operation list * checker) =
  match slices with
  | [] -> (ops, state)
  | x::xs ->
    let new_ops, new_state = touch_liquidation_slice (ops, state, x) in
    touch_liquidation_slices_rec (new_ops, new_state, xs)

let[@inline] touch_liquidation_slices (state: checker) (slices: leaf_ptr list) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  (* NOTE: the order of the operations is reversed here (wrt to the order of
   * the slices), but hopefully we don't care in this instance about this. *)
  touch_liquidation_slices_rec (([]: LigoOp.operation list), state, slices)

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

let option_delegate_eq (o1: Ligo.key_hash option) (o2: Ligo.key_hash option) =
  match o1 with
  | None -> (match o2 with
      | None -> true
      | Some _ -> false
    )
  | Some h1 -> (match o2 with
      | None -> false
      | Some h2 -> h1 = h2
    )

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

  let ops = if option_delegate_eq state.delegate new_delegate then
      ([]: LigoOp.operation list)
    else [LigoOp.Tezos.set_delegate new_delegate]
  in
  (ops,

   { state with
     delegation_auction = new_auction;
     delegate = delegation_auction_delegate new_auction;
     uniswap = uniswap_add_accrued_tez state.uniswap accrued_tez;
   })

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

let checker_delegation_auction_claim_win (state: checker) (bid_ticket: delegation_auction_bid_ticket) (for_delegate: Ligo.key_hash) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let bid = ensure_valid_delegation_auction_bid_ticket bid_ticket in
  let auction = delegation_auction_claim_win state.delegation_auction bid for_delegate in
  updated_delegation_auction state auction

let checker_delegation_auction_reclaim_bid (state: checker) (bid_ticket: delegation_auction_bid_ticket) : LigoOp.operation list * checker =
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

let buy_kit (state: checker) (min_kit_expected: kit) (deadline: Ligo.timestamp) : (LigoOp.operation list * checker) =
  let (ops, state) = touch_delegation_auction state in
  let (kit_tokens, updated_uniswap) = uniswap_buy_kit state.uniswap !Ligo.Tezos.amount min_kit_expected deadline in
  let kit_tokens = kit_issue kit_tokens in (* Issue them here!! *)
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation list) in
  (ops, {state with uniswap = updated_uniswap})

let sell_kit (state: checker) (kit: kit_token) (min_tez_expected: Ligo.tez) (deadline: Ligo.timestamp) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let (ops, state) = touch_delegation_auction state in
  let kit = ensure_valid_kit_token kit in (* destroyed *)
  let (tez, updated_uniswap) = uniswap_sell_kit state.uniswap !Ligo.Tezos.amount kit min_tez_expected deadline in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation list) in
  let updated_state = {state with uniswap = updated_uniswap} in
  (ops, updated_state)

let add_liquidity (state: checker) (max_kit_deposited: kit_token) (min_lqt_minted: Ligo.nat) (deadline: Ligo.timestamp) : (LigoOp.operation list * checker) =
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

let remove_liquidity (state: checker) (lqt_burned: liquidity) (min_tez_withdrawn: Ligo.tez) (min_kit_withdrawn: kit) (deadline: Ligo.timestamp) : (LigoOp.operation list * checker) =
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

let[@inline] checker_liquidation_auction_place_bid (state: checker) (kit: kit_token) : LigoOp.operation list * checker =
  let _ = ensure_no_tez_given () in
  let kit = ensure_valid_kit_token kit in (* destroyed *)

  let bid = { address=(!Ligo.Tezos.sender); kit=kit; } in
  let current_auction = liquidation_auction_get_current_auction state.liquidation_auctions in

  let (new_current_auction, bid_details) = liquidation_auction_place_bid current_auction bid in
  let bid_ticket = issue_liquidation_auction_bid_ticket bid_details in
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferLABidTicket" !Ligo.Tezos.sender : liquidation_auction_bid Ligo.ticket LigoOp.contract option) with
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

let[@inline] checker_liquidation_auction_reclaim_bid (state: checker) (bid_ticket: liquidation_auction_bid_ticket) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let bid_ticket = ensure_valid_liquidation_auction_bid_ticket bid_ticket in
  let (_, (bid_details, _)), _ = Ligo.Tezos.read_ticket bid_ticket in
  let kit = liquidation_auction_reclaim_bid state.liquidation_auctions bid_details in
  let kit_tokens = kit_issue kit in (* TODO: should not issue; should change the auction logic instead! *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c
    | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation) in
  ([op], state) (* NOTE: unchanged state. It's a little weird that we don't keep track of how much kit has not been reclaimed. *)

let[@inline] checker_liquidation_auction_reclaim_winning_bid (state: checker) (bid_ticket: liquidation_auction_bid_ticket) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  let bid_ticket = ensure_valid_liquidation_auction_bid_ticket bid_ticket in
  let (_, (bid_details, _)), _ = Ligo.Tezos.read_ticket bid_ticket in
  let (tez, liquidation_auctions) = liquidation_auction_reclaim_winning_bid state.liquidation_auctions bid_details in
  let op = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.unit_transaction () tez c
    | None -> (Ligo.failwith error_GetContractOptFailure : LigoOp.operation) in
  ([op], {state with liquidation_auctions = liquidation_auctions })

(* TODO: Maybe we should provide an entrypoint for increasing a losing bid.
 * *)

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

  let touch_low_reward = fixedpoint_of_ratio_ceil touch_low_reward in
  let touch_high_reward = fixedpoint_of_ratio_ceil touch_high_reward in
  kit_scale
    kit_one
    (fixedpoint_add
       (fixedpoint_mul (fixedpoint_of_int low_duration) touch_low_reward)
       (fixedpoint_mul (fixedpoint_of_int high_duration) touch_high_reward)
    )

let rec touch_oldest (ops, state, maximum: LigoOp.operation list * checker * int) : (LigoOp.operation list * checker) =
  if maximum <= 0 then
    (ops, state)
  else
    match liquidation_auction_oldest_completed_liquidation_slice state.liquidation_auctions with
    | None -> (ops, state)
    | Some leaf ->
      let new_ops, new_state = touch_liquidation_slice (ops, state, leaf) in
      touch_oldest (new_ops, new_state, maximum - 1)

let touch_with_index (state: checker) (index:Ligo.tez) : (LigoOp.operation list * checker) =
  assert (state.parameters.last_touched <= !Ligo.Tezos.now); (* FIXME: I think this should be translated to LIGO actually. *)
  let _ = ensure_no_tez_given () in
  if state.parameters.last_touched = !Ligo.Tezos.now then
    (* Do nothing if up-to-date (idempotence) *)
    let kit_tokens = kit_issue kit_zero in (* zero reward *)
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c (* TODO: perhaps don't transfer anything at all?? *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation) in
    ([op], state)
  else
    (* TODO: What is the right order in which to do things here? We use the
     * last observed kit_in_tez price from uniswap to update the parameters,
     * which return kit to be added to the uniswap contract. Gotta make sure we
     * do things in the right order here. *)

    (* 1: Calculate the reward that we should create out of thin air to give
     * to the contract toucher, and update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state.parameters.last_touched in
    let state = { state with parameters =
                               add_circulating_kit state.parameters reward } in

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
        (* George: I use ceil, to stay on the safe side (higher-price) *)
        (fixedpoint_of_ratio_ceil (minting_price updated_parameters)) in

    (* 6: Touch oldest liquidation slices *)
    (* TODO: Touch only runs at most once per block. But it might be beneficial to run this step
     * without that restriction. *)
    let state =
      { state with
        burrows = state.burrows; (* leave as-is *)
        parameters = updated_parameters;
        uniswap = updated_uniswap;
        liquidation_auctions = updated_liquidation_auctions;
      } in

    (* TODO: Figure out how many slices we can process per checker touch.*)
    (* NOTE: the order of the operations is reversed here (wrt to the order of
     * the slices), but hopefully we don't care in this instance about this. *)
    let ops, state = touch_oldest (ops, state, number_of_slices_to_process) in
    assert_checker_invariants state;

    (* TODO: Add more tasks here *)

    let kit_tokens = kit_issue reward in
    let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transferKit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops (* NOTE: I (George) think we should concatenate to the right actually. *)
      | None -> (Ligo.failwith error_GetEntrypointOptFailureTransferKit : LigoOp.operation list) in

    (* Create operations to ask the oracles to send updated values. *)
    let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receivePrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
      | Some cb -> cb
      | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : Ligo.nat LigoOp.contract) in
    let oracle = match (LigoOp.Tezos.get_entrypoint_opt oracle_entrypoint oracle_address : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
      | Some c -> c
      | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint : (Ligo.nat LigoOp.contract) LigoOp.contract) in
    let op = LigoOp.Tezos.nat_contract_transaction cb (Ligo.tez_from_literal "0mutez") oracle in
    let ops = (op :: ops) in (* FIXME: op should be at the end, not the beginning *)

    (ops, state)

let touch (state: checker) : (LigoOp.operation list * checker) =
  let index = match state.last_price with
    | None -> state.parameters.index (* use the old one *)
    | Some i -> Ligo.mul_nat_tez i (Ligo.tez_from_literal "1mutez") in (* FIXME: Is the nat supposed to represent tez? *)
  touch_with_index state index

(* ************************************************************************* *)
(**                               ORACLE                                     *)
(* ************************************************************************* *)

let receive_price (state: checker) (price: Ligo.nat) : (LigoOp.operation list * checker) =
  let _ = ensure_no_tez_given () in
  if !Ligo.Tezos.sender <> oracle_address then
    (Ligo.failwith error_UnauthorisedCaller : LigoOp.operation list * checker)
  else
    (([]: LigoOp.operation list), {state with last_price = Some price})

(* ENTRYPOINTS *)

type params =
  | Touch
  (* Burrows *)
  | CreateBurrow of Ligo.key_hash option
  | DepositTez of (permission option * burrow_id)
  | WithdrawTez of (permission * Ligo.tez * burrow_id)
  | MintKit of (permission * burrow_id * kit)
  | BurnKit of (permission option * burrow_id * kit_token)
  | ActivateBurrow of (permission * burrow_id)
  | DeactivateBurrow of (permission * burrow_id * Ligo.address)
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

let main (op_and_state: params * checker): LigoOp.operation list * checker =
  let op, state = op_and_state in
  match op with
  | Touch ->
    touch state
  (* Burrows *)
  | CreateBurrow delegate_opt ->
    create_burrow state delegate_opt
  | DepositTez p ->
    let (permission_option, burrow_id) = p in
    deposit_tez state permission_option burrow_id
  | WithdrawTez p ->
    let (permission, tez, burrow_id) = p in
    withdraw_tez state permission tez burrow_id
  | MintKit p ->
    let (permission, burrow_id, kit) = p in
    mint_kit state permission burrow_id kit
  | BurnKit p ->
    let (permission_option, burrow_id, kit_token) = p in
    burn_kit state permission_option burrow_id kit_token
  | ActivateBurrow p ->
    let (permission, burrow_id) = p in
    activate_burrow state permission burrow_id
  | DeactivateBurrow p ->
    let (permission, burrow_id, addr) = p in
    deactivate_burrow state permission burrow_id addr
  | MarkBurrowForLiquidation burrow_id ->
    mark_for_liquidation state burrow_id
  | TouchLiquidationSlices slices ->
    touch_liquidation_slices state slices
  | CancelSliceLiquidation p ->
    let (permission, leaf_ptr) = p in
    cancel_liquidation_slice state permission leaf_ptr
  | TouchBurrow burrow_id ->
    touch_burrow state burrow_id
  | SetBurrowDelegate p ->
    let (permission, burrow_id, delegate_opt) = p in
    set_burrow_delegate state permission burrow_id delegate_opt
  | MakePermission p ->
    let (permission, burrow_id, rights) = p in
    make_permission state permission burrow_id rights
  | InvalidateAllPermissions p ->
    let (permission, burrow_id) = p in
    invalidate_all_permissions state permission burrow_id
  (* Uniswap *)
  | BuyKit p ->
    let (min_kit, deadline) = p in
    buy_kit state min_kit deadline
  | SellKit p ->
    let (kit_token, min_tez, deadline) = p in
    sell_kit state kit_token min_tez deadline
  | AddLiquidity p ->
    let (max_kit_token, min_liquidity, deadline) = p in
    add_liquidity state max_kit_token min_liquidity deadline
  | RemoveLiquidity p ->
    let (liquidity, min_tez, min_kit, deadline) = p in
    remove_liquidity state liquidity min_tez min_kit deadline
  (* Liquidation Auction *)
  | LiqAuctionPlaceBid kit_token ->
    checker_liquidation_auction_place_bid state kit_token
  | LiqAuctionReclaimBid ticket ->
    checker_liquidation_auction_reclaim_bid state ticket
  | LiqAuctionReclaimWinningBid ticket ->
    checker_liquidation_auction_reclaim_winning_bid state ticket
  | ReceiveLiquidationSlice ->
    receive_slice_from_burrow state
  (* Delegation Auction *)
  | DelegationAuctionPlaceBid ->
    checker_delegation_auction_place_bid state
  | DelegationAuctionClaimWin p ->
    let (ticket, key) = p in
    checker_delegation_auction_claim_win state ticket key
  | DelegationAuctionReclaimBid ticket ->
    checker_delegation_auction_reclaim_bid state ticket
  (* Oracles *)
  | ReceivePrice price ->
    receive_price state price
