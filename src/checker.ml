open FixedPoint
open Ptr
open Ratio
open Kit
open Mem
open Avl
open Permission
open Parameters
open Uniswap
open Burrow
open DelegationAuction
open LiquidationAuction
open LiquidationAuctionTypes
open Common
open Constants
open TokenTypes

(* TODO: At the very end, inline all numeric operations, flatten all ratio so
 * that we mainly deal with integers directly. Hardwire the constants too,
 * where possible. *)

type burrow_id = ptr

type t =
  { burrows : (ptr, burrow) Ligo.big_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
    delegate : Ligo.key_hash option;
    last_burrow_id: ptr;
  }

let initial_checker =
  { burrows = (Ligo.Big_map.empty: (ptr, burrow) Ligo.big_map);
    uniswap = uniswap_make_initial;
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    delegation_auction = delegation_auction_empty;
    delegate = (None : Ligo.key_hash option);
    last_burrow_id = ptr_null; (* FIXME: this will be unnecessary when we start to use addresses as keys *)
  }

(* Utility function to give us burrow addresses *)
let mk_burrow_id (state: t) : (burrow_id * t) =
  let n = ptr_next state.last_burrow_id in
  (n, { state with last_burrow_id = n; })

(* BEGIN_OCAML *)
let assert_invariants (state: t) : unit =
  (* Check if the auction pointerfest kind of make sense. *)
  liquidation_auction_assert_invariants state.liquidation_auctions;
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

let is_burrow_done_with_liquidations (state: t) (burrow: burrow) =
  match burrow_oldest_liquidation_ptr burrow with
  | None -> true
  | Some ls ->
    let root = avl_find_root state.liquidation_auctions.avl_storage ls in
    let outcome = avl_root_data state.liquidation_auctions.avl_storage root in
    (match outcome with
     | None -> true
     | Some _ -> false)

let find_burrow (state: t) (burrow_id: burrow_id) : burrow =
  match Ligo.Big_map.find_opt burrow_id state.burrows with
  | None -> (failwith "NonExistentBurrow": burrow)
  | Some burrow -> burrow

let assert_permission_is_present (permission: permission option) : permission =
  match permission with
  | None -> (failwith "MissingPermission": permission)
  | Some permission -> permission

(* Looks up a burrow_id from state, and checks if the resulting burrow does
 * not have any completed liquidation slices that need to be claimed before
 * any operation. *)
let assert_burrow_has_no_unclaimed_slices (state: t) (burrow: burrow) : unit =
  if is_burrow_done_with_liquidations state burrow
  then ()
  else failwith "BurrowHasCompletedLiquidation"

(* Ensure that there is no tez given. To prevent accidental fund loss. *)
let assert_no_tez_given () =
  if !Ligo.Tezos.amount <> Ligo.tez_from_literal "0mutez"
  then failwith "UnwantedTezGiven"
  else ()

(* NOTE: It totally consumes the ticket. It's the caller's responsibility to
 * replicate the permission ticket if they don't want to lose it. *)
let assert_valid_permission
    (permission: permission)
    (burrow_id: burrow_id)
    (burrow: burrow)
  : rights =
  let (issuer, ((right, id, version), amnt)), _ = Ligo.Tezos.read_ticket permission in
  let validity_condition =
    issuer = checker_address
    && amnt = Ligo.nat_from_literal "0n"
    && version = burrow_permission_version burrow
    && id = burrow_id in
  if validity_condition
  then right
  else (failwith "InvalidPermission": rights)

let create_burrow (state: t) =
  let (burrow_id, state) = mk_burrow_id state in
  let burrow = burrow_create state.parameters !Ligo.Tezos.amount in
  let admin_ticket =
    Ligo.Tezos.create_ticket
      (Admin, burrow_id, Ligo.nat_from_literal "0n")
      (Ligo.nat_from_literal "0n") in
  let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows} in
  (burrow_id, admin_ticket, updated_state) (* TODO: send the id and the ticket to sender! *)

let touch_burrow (state: t) (burrow_id: burrow_id) : (LigoOp.operation list * t) =
  let burrow = find_burrow state burrow_id in
  let updated_burrow = burrow_touch state.parameters burrow in
  let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
  let ops : LigoOp.operation list = [] in
  (ops, state)

let deposit_tez (state: t) (permission: permission option) (burrow_id: burrow_id) : (LigoOp.operation list * t) =
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let ops : LigoOp.operation list = [] in
  if burrow_allow_all_tez_deposits burrow then
    (* no need to check the permission argument at all *)
    let updated_burrow = burrow_deposit_tez state.parameters !Ligo.Tezos.amount burrow in
    (ops, {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows})
  else
    let permission = assert_permission_is_present permission in
    let r = assert_valid_permission permission burrow_id burrow in
    if does_right_allow_tez_deposits r then
      (* the permission should support depositing tez. *)
      let updated_burrow = burrow_deposit_tez state.parameters !Ligo.Tezos.amount burrow in
      (ops, {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows})
    else
      (failwith "InsufficientPermission": LigoOp.operation list * t)

let mint_kit (state: t) (permission: permission) (burrow_id: burrow_id) (kit: kit) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if does_right_allow_kit_minting r then
    (* the permission should support minting kit. *)
    let (updated_burrow, minted) = burrow_mint_kit state.parameters kit burrow in
    assert (kit = minted);

    let kit_tokens = kit_issue minted in
    let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> [LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c]
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
    ( ops,
      {state with
       burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
       parameters =
         add_outstanding_kit
           (add_circulating_kit state.parameters minted)
           minted;
      }
    )
  else
    (failwith "InsufficientPermission": LigoOp.operation list * t)

let withdraw_tez (state: t) (permission: permission) (tez: Ligo.tez) (burrow_id: burrow_id) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if does_right_allow_tez_withdrawals r then
    (* the permission should support withdrawing tez. *)
    let (updated_burrow, withdrawn) = burrow_withdraw_tez state.parameters tez burrow in
    assert (tez = withdrawn);
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
      | Some c -> [LigoOp.Tezos.unit_transaction () withdrawn c]
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
    (ops, updated_state)
  else
    (failwith "InsufficientPermission": LigoOp.operation list * t)

let burn_kit (state: t) (permission: permission option) (burrow_id: burrow_id) (kit: kit_token) : (LigoOp.operation list * t) =
  let ops : LigoOp.operation list = [] in
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let kit = assert_valid_kit_token kit in
  let kit, _ (* destroyed *) = read_kit kit in
  if burrow_allow_all_kit_burnings burrow then
    (* no need to check the permission argument at all *)
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
    (ops, state)
  else
    let permission = assert_permission_is_present permission in
    let r = assert_valid_permission permission burrow_id burrow in
    if does_right_allow_kit_burning r then
      (* the permission should support burning kit. *)
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
      (ops, state)
    else
      (failwith "InsufficientPermission": LigoOp.operation list * t)

let activate_burrow (state: t) (permission: permission) (burrow_id: burrow_id) : (LigoOp.operation list * t) =
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if is_admin_right r then
    (* only admins can activate burrows. *)
    let updated_burrow = burrow_activate state.parameters !Ligo.Tezos.amount burrow in
    let ops : LigoOp.operation list = [] in
    let state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    (ops, state)
  else
    (failwith "InsufficientPermission": LigoOp.operation list * t)

let deactivate_burrow (state: t) (permission: permission) (burrow_id: burrow_id) (recipient: Ligo.address) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if is_admin_right r then
    (* only admins (and checker itself, due to liquidations) can deactivate burrows. *)
    let (updated_burrow, returned_tez) = burrow_deactivate state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let ops = match (LigoOp.Tezos.get_contract_opt recipient : unit LigoOp.contract option) with
      | Some c -> [LigoOp.Tezos.unit_transaction () returned_tez c]
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
    (ops, updated_state)
  else
    (failwith "InsufficientPermission": LigoOp.operation list * t)

let set_burrow_delegate (state: t) (permission: permission) (burrow_id: burrow_id) (delegate: Ligo.address) : t =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if does_right_allow_setting_delegate r then
    (* the permission should support setting the delegate. *)
    let updated_burrow = burrow_set_delegate state.parameters delegate burrow in
    {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows}
  else
    (failwith "InsufficientPermission": t)

let make_permission (state: t) (permission: permission) (burrow_id: burrow_id) (right: rights) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if is_admin_right r then
    (* only admins can create permissions. *)
    let ticket =
      Ligo.Tezos.create_ticket
        (right, burrow_id, burrow_permission_version burrow)
        (Ligo.nat_from_literal "0n") in
    let op = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_permission" !Ligo.Tezos.sender : permission LigoOp.contract option) with
      | Some c -> LigoOp.Tezos.perm_transaction ticket (Ligo.tez_from_literal "0mutez") c
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation) in
    ([op], state) (* unchanged state *)
  else
    (failwith "InsufficientPermission": LigoOp.operation list * t)

let invalidate_all_permissions (state: t) (permission: permission) (burrow_id: burrow_id) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission permission burrow_id burrow in
  if is_admin_right r then
    (* only admins can invalidate all permissions. *)
    let updated_version, updated_burrow = burrow_increase_permission_version state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let admin_ticket =
      Ligo.Tezos.create_ticket
        (Admin, burrow_id, updated_version)
        (Ligo.nat_from_literal "0n") in
    let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_permission" !Ligo.Tezos.sender : permission LigoOp.contract option) with
      | Some c -> [LigoOp.Tezos.perm_transaction admin_ticket (Ligo.tez_from_literal "0mutez") c]
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
    (ops, updated_state)
  else
    (failwith "InsufficientPermission": LigoOp.operation list * t)

(* TODO: Arthur: one time we might want to trigger garbage collection of
 * slices is during a liquidation. a liquidation creates one slice, so if we
 * clear one pending slice when that happens it won't grow unbounded (yes,
 * there are degenerate cases where the queue starts growing much faster that
 * the auctions are happening and in those instances it could grow unbounded,
 * but roughly speaking in most cases it should average out) *)
let mark_for_liquidation (state: t) (burrow_id: burrow_id) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in

  let details = match burrow_request_liquidation state.parameters burrow with
    | Unnecessary -> (failwith "NotLiquidationCandidate": liquidation_details)
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

  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> [LigoOp.Tezos.unit_transaction () details.liquidation_reward c]
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in

  ( ops,
    {state with
     burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
     liquidation_auctions = { updated_liquidation_auctions with avl_storage = updated_storage; };
    }
  )

(* Update the immediate neighbors of a slice (i.e. the younger and the older)
 * to point to each other instead of the slice in question, so that it can be
 * removed. *)
(* NOTE: the liquidation slice must be the one pointed to by the leaf pointer. *)
let update_immediate_neighbors (state: t) (leaf_ptr: leaf_ptr) (leaf : liquidation_slice) =
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
let cancel_liquidation_slice (state: t) (permission: permission) (leaf_ptr: leaf_ptr) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let leaf = avl_read_leaf state.liquidation_auctions.avl_storage leaf_ptr in
  let burrow_id = leaf.burrow in
  let burrow = find_burrow state burrow_id in
  let r = assert_valid_permission permission burrow_id burrow in
  if not (does_right_allow_cancelling_liquidations r) then
    (failwith "InsufficientPermission": LigoOp.operation list * t)
  else
    let root = avl_find_root state.liquidation_auctions.avl_storage leaf_ptr in
    if ptr_of_avl_ptr root <> ptr_of_avl_ptr state.liquidation_auctions.queued_slices
    then (failwith "UnwarrantedCancellation": LigoOp.operation list * t)
    else
      let leaf = avl_read_leaf state.liquidation_auctions.avl_storage leaf_ptr in
      if burrow_is_overburrowed state.parameters burrow then
        (failwith "UnwarrantedCancellation": LigoOp.operation list * t)
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
        assert_invariants state;
        let ops : LigoOp.operation list = [] in
        (ops, state)

let touch_liquidation_slice (state: t) (leaf_ptr: leaf_ptr): t =
  let root = avl_find_root state.liquidation_auctions.avl_storage leaf_ptr in
  match avl_root_data state.liquidation_auctions.avl_storage root with
  (* The slice does not belong to a completed auction, so we skip it. *)
  (* NOTE: Perhaps failing would be better than silently doing nothing here?
   * Not sure if there is any danger though. *)
  | None -> state
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
    assert_invariants state;
    state

let rec touch_liquidation_slices (state, slices: t * leaf_ptr list) : (LigoOp.operation list * t) =
  match slices with
  | [] -> (([] : LigoOp.operation list), state)
  | x::xs -> touch_liquidation_slices (touch_liquidation_slice state x, xs)

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

let updated_delegation_auction (state: t) (new_auction: delegation_auction) =
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

let delegation_auction_place_bid (state: t) : (LigoOp.operation list * t) =
  let ticket, auction =
    delegation_auction_place_bid
      state.delegation_auction
      !Ligo.Tezos.sender
      !Ligo.Tezos.amount
  in
  let (ops, new_state) = updated_delegation_auction state auction in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_da_bid_ticket" !Ligo.Tezos.sender : delegation_auction_bid Ligo.ticket LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.da_bid_transaction ticket (Ligo.tez_from_literal "0mutez") c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  (ops, new_state)

let delegation_auction_claim_win (state: t) (bid_ticket: delegation_auction_bid Ligo.ticket) (for_delegate: Ligo.key_hash) : (LigoOp.operation list * t) =
  let auction = delegation_auction_claim_win state.delegation_auction bid_ticket for_delegate in
  updated_delegation_auction state auction

let delegation_auction_reclaim_bid (state: t) (bid_ticket: delegation_auction_bid Ligo.ticket) : LigoOp.operation list * t =
  assert_no_tez_given ();
  let tez, auction = delegation_auction_reclaim_bid state.delegation_auction bid_ticket in
  let ops, new_auction = updated_delegation_auction state auction in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  (ops, new_auction)

let touch_delegation_auction (state: t) =
  updated_delegation_auction state (delegation_auction_touch state.delegation_auction)

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

let buy_kit (state: t) (min_kit_expected: kit) (deadline: Ligo.timestamp) : (LigoOp.operation list * t) =
  let (ops, state) = touch_delegation_auction state in
  let (kit_tokens, updated_uniswap) = uniswap_buy_kit state.uniswap !Ligo.Tezos.amount min_kit_expected deadline in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  (ops, {state with uniswap = updated_uniswap})

let sell_kit (state: t) (kit: kit_token) (min_tez_expected: Ligo.tez) (deadline: Ligo.timestamp) : (LigoOp.operation list * t) =
  let (ops, state) = touch_delegation_auction state in
  let kit = assert_valid_kit_token kit in
  let (tez, updated_uniswap) = uniswap_sell_kit state.uniswap !Ligo.Tezos.amount kit min_tez_expected deadline in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  let updated_state = {state with uniswap = updated_uniswap} in
  (ops, updated_state)

let add_liquidity (state: t) (max_kit_deposited: kit_token) (min_lqt_minted: Ligo.nat) (deadline: Ligo.timestamp) : (LigoOp.operation list * t) =
  let (ops, state) = touch_delegation_auction state in
  let pending_accrual = match delegation_auction_winning_amount state.delegation_auction with
    | None -> Ligo.tez_from_literal "0mutez"
    | Some tez -> tez in
  let max_kit_deposited = assert_valid_kit_token max_kit_deposited in
  let (lqt_tokens, kit_tokens, updated_uniswap) =
    uniswap_add_liquidity state.uniswap !Ligo.Tezos.amount pending_accrual max_kit_deposited min_lqt_minted deadline in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_lqt" !Ligo.Tezos.sender : liquidity LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.lqt_transaction lqt_tokens (Ligo.tez_from_literal "0mutez") c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  (ops, {state with uniswap = updated_uniswap})

let remove_liquidity (state: t) (lqt_burned: liquidity) (min_tez_withdrawn: Ligo.tez) (min_kit_withdrawn: kit) (deadline: Ligo.timestamp) : (LigoOp.operation list * t) =
  let (ops, state) = touch_delegation_auction state in
  let (tez, kit_tokens, updated_uniswap) =
    uniswap_remove_liquidity state.uniswap !Ligo.Tezos.amount lqt_burned min_tez_withdrawn min_kit_withdrawn deadline in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.unit_transaction () tez c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  let updated_state = {state with uniswap = updated_uniswap} in
  (ops, updated_state)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

let liquidation_auction_place_bid (state: t) (kit: kit_token) : LigoOp.operation list * t =
  assert_no_tez_given ();
  let kit = assert_valid_kit_token kit in
  let kit, _ = read_kit kit in (* TODO: should not destroy; should change the auction logic instead! *)

  let bid = { address=(!Ligo.Tezos.sender); kit=kit; } in
  let current_auction = liquidation_auction_get_current_auction state.liquidation_auctions in

  let (new_current_auction, bid_ticket) = liquidation_auction_place_bid current_auction bid in
  let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_la_bid_ticket" !Ligo.Tezos.sender : liquidation_auction_bid_details Ligo.ticket LigoOp.contract option) with
    | Some c -> [LigoOp.Tezos.la_bid_transaction bid_ticket (Ligo.tez_from_literal "0mutez") c]
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  ( ops,
    { state with
      liquidation_auctions=
        { state.liquidation_auctions with
          current_auction = Some new_current_auction;
        };
    }
  )

let liquidation_auction_reclaim_bid (state: t) (bid_ticket: liquidation_auction_bid_ticket) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let bid_ticket = liquidation_auction_assert_valid_bid_ticket bid_ticket in
  let kit = liquidation_auction_reclaim_bid state.liquidation_auctions bid_ticket in
  let kit_tokens = kit_issue kit in (* TODO: should not issue; should change the auction logic instead! *)
  let op = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
    | Some c -> LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation) in
  ([op], state) (* NOTE: unchanged state. It's a little weird that we don't keep track of how much kit has not been reclaimed. *)

let liquidation_auction_reclaim_winning_bid (state: t) (bid_ticket: liquidation_auction_bid_ticket) : (LigoOp.operation list * t) =
  assert_no_tez_given ();
  let bid_ticket = liquidation_auction_assert_valid_bid_ticket bid_ticket in
  let (tez, liquidation_auctions) = liquidation_auction_reclaim_winning_bid state.liquidation_auctions bid_ticket in
  let ops = match (LigoOp.Tezos.get_contract_opt !Ligo.Tezos.sender : unit LigoOp.contract option) with
    | Some c -> [LigoOp.Tezos.unit_transaction () tez c]
    | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
  (ops, {state with liquidation_auctions = liquidation_auctions })

(* TODO: Maybe we should provide an entrypoint for increasing a losing bid.
 * *)

(* ************************************************************************* *)
(**                              TOUCHING                                    *)
(* ************************************************************************* *)

(** Calculate how much is right now the reward for touching the main checker
  * contract. We use a bracketed calculation, where for the first
  * touch_reward_low_bracket seconds the reward increases by touch_low_reward
  * per second, and after that by touch_high_reward per second. *)
let calculate_touch_reward (state: t) : kit =
  assert (state.parameters.last_touched <= !Ligo.Tezos.now);
  let duration_in_seconds = Ligo.sub_timestamp_timestamp !Ligo.Tezos.now state.parameters.last_touched in
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

let touch (state: t) (index:Ligo.tez) : (LigoOp.operation list * t) =
  if state.parameters.last_touched = !Ligo.Tezos.now then
    (* Do nothing if up-to-date (idempotence) *)
    let kit_tokens = kit_issue kit_zero in (* zero reward *)
    let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> [LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c]
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in
    (ops, state)
  else
    (* TODO: What is the right order in which to do things here? We use the
     * last observed kit_in_tez price from uniswap to update the parameters,
     * which return kit to be added to the uniswap contract. Gotta make sure we
     * do things in the right order here. *)

    (* 1: Calculate the reward that we should create out of thin air to give
     * to the contract toucher, and update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state in
    let state = { state with parameters =
                               add_circulating_kit state.parameters reward } in

    (* Ensure the delegation auction is up-to-date, and any proceeds accrued to the uniswap *)
    let (ops, state) = touch_delegation_auction state in

    (* 2: Update the system parameters *)
    let total_accrual_to_uniswap, updated_parameters =
      parameters_touch index (uniswap_kit_in_tez_in_prev_block state.uniswap) state.parameters
    in
    (* 3: Add accrued burrowing fees to the uniswap sub-contract *)
    let total_accrual_to_uniswap = kit_issue total_accrual_to_uniswap in
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

    let rec touch_oldest (maximum, st: int * t) : t =
      if maximum <= 0 then st
      else
        match liquidation_auction_oldest_completed_liquidation_slice st.liquidation_auctions with
        | None -> st
        | Some leaf -> touch_oldest (maximum - 1, touch_liquidation_slice st leaf) in

    (* TODO: Figure out how many slices we can process per checker touch.*)
    let state = touch_oldest (number_of_slices_to_process, state) in
    assert_invariants state;

    (* TODO: Add more tasks here *)

    let kit_tokens = kit_issue reward in
    let ops = match (LigoOp.Tezos.get_entrypoint_opt "%transfer_kit" !Ligo.Tezos.sender : kit_token LigoOp.contract option) with
      | Some c -> (LigoOp.Tezos.kit_transaction kit_tokens (Ligo.tez_from_literal "0mutez") c) :: ops
      | None -> (failwith "unsupported operation, looks like" : LigoOp.operation list) in

    (ops, state)

(* ENTRYPOINTS *)

type params =
  | Touch
  (* Uniswap *)
  | BuyKit of (kit * Ligo.timestamp)
  | SellKit of (kit_token * Ligo.tez * Ligo.timestamp)
  | AddLiquidity of (kit_token * Ligo.nat * Ligo.timestamp)
  | RemoveLiquidity of (liquidity * Ligo.tez * kit * Ligo.timestamp)
  (* Delegation Auction *)
  | DelegationAuctionPlaceBid
  | DelegationAuctionClaimWin of (delegation_auction_bid Ligo.ticket * Ligo.key_hash)
  | DelegationAuctionReclaimBid of delegation_auction_bid Ligo.ticket

let main (op, state: params * t): LigoOp.operation list * t =
  match op with
  | Touch ->
    let ops, state = touch state (Ligo.tez_from_literal "0mutez") in
    (ops, state)
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
  (* Delegation Auction *)
  | DelegationAuctionPlaceBid ->
    delegation_auction_place_bid state
  | DelegationAuctionClaimWin p ->
    let (ticket, key) = p in
    delegation_auction_claim_win state ticket key
  | DelegationAuctionReclaimBid ticket ->
    delegation_auction_reclaim_bid state ticket
