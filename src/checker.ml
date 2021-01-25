open FixedPoint
open Ptr
open Ratio
open Kit
open Avl

(* TODO: At the very end, inline all numeric operations, flatten all ratio so
 * that we mainly deal with integers directly. Hardwire the constants too,
 * where possible. *)

type burrow_id = Ptr.t

type t =
  { burrows : (ptr, Burrow.t) Ligo.big_map;
    uniswap : Uniswap.t;
    parameters : Parameters.t;
    liquidation_auctions : LiquidationAuction.auctions;
    delegation_auction : DelegationAuction.t;
    delegate : Ligo.address option;
  }

let initial_checker =
  { burrows = Ligo.Big_map.empty;
    uniswap = Uniswap.make_initial;
    parameters = Parameters.initial_parameters;
    liquidation_auctions = LiquidationAuction.empty;
    delegation_auction = DelegationAuction.empty;
    delegate = None;
  }

(* Utility function to give us burrow addresses *)
let mk_burrow_id () : burrow_id =
  Ptr.random_ptr ()

let assert_invariants (state: 't) : unit =
  (* Check if the auction pointerfest kind of make sense. *)
  LiquidationAuction.assert_invariants state.liquidation_auctions;
  (* Per-burrow assertions *)
  List.iter
    (fun (burrow_address, burrow) ->
       Burrow.assert_invariants burrow;

       match Burrow.liquidation_slices burrow with
       | None ->
         assert (Burrow.collateral_at_auction burrow = Ligo.tez_from_literal "0mutez");
       | Some slices ->
         (* Check if the linked list of slices are correct, and the amount of
          * tez inside is consistent with collateral_at_auction.
         *)
         let rec go
             (curr: LiquidationAuctionTypes.leaf_ptr)
             (prev: LiquidationAuctionTypes.leaf_ptr option) : Ligo.tez =
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
         assert (Burrow.collateral_at_auction burrow = actual_collateral)
    )
    (Ligo.Big_map.bindings state.burrows)

(* ************************************************************************* *)
(**                               BURROWS                                    *)
(* ************************************************************************* *)

let is_burrow_done_with_liquidations (state: t) (burrow: Burrow.t) =
  match Burrow.oldest_liquidation_ptr burrow with
  | None -> true
  | Some ls ->
    let root = avl_find_root state.liquidation_auctions.avl_storage ls in
    let outcome = avl_root_data state.liquidation_auctions.avl_storage root in
    Option.is_none outcome

let find_burrow (state: t) (burrow_id: burrow_id) : Burrow.t =
  match Ligo.Big_map.find_opt burrow_id state.burrows with
  | None -> failwith "NonExistentBurrow"
  | Some burrow -> burrow

let assert_permission_is_present (permission: Permission.t option) : Permission.t =
  match permission with
  | None -> failwith "MissingPermission"
  | Some permission -> permission

(* Looks up a burrow_id from state, and checks if the resulting burrow does
 * not have any completed liquidation slices that need to be claimed before
 * any operation. *)
let assert_burrow_has_no_unclaimed_slices (state: t) (burrow: Burrow.t) : unit =
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
    ~(permission: Permission.t)
    ~(burrow_id:burrow_id)
    ~(burrow: Burrow.t)
  : Permission.rights =
  let (issuer, ((rights, id, version), amount)), _ = Ligo.Tezos.read_ticket permission in
  let validity_condition =
    issuer = Ligo.Tezos.self_address
    && amount = Ligo.nat_from_literal "0n"
    && version = Burrow.permission_version burrow
    && id = burrow_id in
  if validity_condition
  then rights
  else failwith "InvalidPermission"

let create_burrow (state:t) =
  let burrow_id = mk_burrow_id () in
  let burrow = Burrow.create state.parameters !Ligo.Tezos.amount in
  let admin_ticket =
    Ligo.Tezos.create_ticket
      (Permission.Admin, burrow_id, 0)
      (Ligo.nat_from_literal "0n") in
  let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some burrow) state.burrows} in
  (burrow_id, admin_ticket, updated_state) (* TODO: send the id and the ticket to sender! *)

let touch_burrow (state: t) (burrow_id: burrow_id) =
  let burrow = find_burrow state burrow_id in
  let updated_burrow = Burrow.touch state.parameters burrow in
  {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows}

let deposit_tez (state:t) ~permission ~burrow_id =
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  if Burrow.allow_all_tez_deposits burrow then
    (* no need to check the permission argument at all *)
    let updated_burrow = Burrow.deposit_tez state.parameters !Ligo.Tezos.amount burrow in
    {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows}
  else
    let permission = assert_permission_is_present permission in
    let r = assert_valid_permission ~permission ~burrow_id ~burrow in
    if Permission.does_right_allow_tez_deposits r then
      (* the permission should support depositing tez. *)
      let updated_burrow = Burrow.deposit_tez state.parameters !Ligo.Tezos.amount burrow in
      {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows}
    else
      failwith "InsufficientPermission"

let mint_kit (state:t) ~permission ~burrow_id ~kit =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.does_right_allow_kit_minting r then
    (* the permission should support minting kit. *)
    let (updated_burrow, minted) = Burrow.mint_kit state.parameters kit burrow in
    assert (kit = minted);
    ( kit_issue minted,
      {state with
       burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
       parameters =
         Parameters.add_outstanding_kit
           (Parameters.add_circulating_kit state.parameters minted)
           minted;
      }
    )
  else
    failwith "InsufficientPermission"

let withdraw_tez (state:t) ~permission ~tez ~burrow_id =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.does_right_allow_tez_withdrawals r then
    (* the permission should support withdrawing tez. *)
    let (updated_burrow, withdrawn) = Burrow.withdraw_tez state.parameters tez burrow in
    assert (tez = withdrawn);
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let tez_payment = Tez.{destination = !Ligo.Tezos.sender; amount = withdrawn} in
    (tez_payment, updated_state)
  else
    failwith "InsufficientPermission"

let burn_kit (state:t) ~permission ~burrow_id ~kit =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let kit = assert_valid_kit_token kit in
  let kit, _ (* destroyed *) = read_kit kit in
  if Burrow.allow_all_kit_burnings burrow then
    (* no need to check the permission argument at all *)
    let updated_burrow = Burrow.burn_kit state.parameters kit burrow in
    (* TODO: What should happen if the following is violated? *)
    assert (state.parameters.circulating_kit >= kit);
    {state with
     burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
     parameters =
       Parameters.remove_outstanding_kit
         (Parameters.remove_circulating_kit state.parameters kit)
         kit;
    }
  else
    let permission = assert_permission_is_present permission in
    let r = assert_valid_permission ~permission ~burrow_id ~burrow in
    if Permission.does_right_allow_kit_burning r then
      (* the permission should support burning kit. *)
      let updated_burrow = Burrow.burn_kit state.parameters kit burrow in
      (* TODO: What should happen if the following is violated? *)
      assert (state.parameters.circulating_kit >= kit);
      {state with
       burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
       parameters =
         Parameters.remove_outstanding_kit
           (Parameters.remove_circulating_kit state.parameters kit)
           kit;
      }
    else
      failwith "InsufficientPermission"

let activate_burrow (state:t) ~permission ~burrow_id =
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.is_admin_right r then
    (* only admins can activate burrows. *)
    let updated_burrow = Burrow.activate state.parameters !Ligo.Tezos.amount burrow in
    {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows}
  else
    failwith "InsufficientPermission"

let deactivate_burrow (state:t) ~permission ~burrow_id ~recipient =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.is_admin_right r then
    (* only admins (and checker itself, due to liquidations) can deactivate burrows. *)
    let (updated_burrow, returned_tez) = Burrow.deactivate state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let tez_payment = Tez.{destination = recipient; amount = returned_tez} in
    (tez_payment, updated_state)
  else
    failwith "InsufficientPermission"

let set_burrow_delegate (state:t) ~permission ~burrow_id ~delegate =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.does_right_allow_setting_delegate r then
    (* the permission should support setting the delegate. *)
    let updated_burrow = Burrow.set_delegate state.parameters delegate burrow in
    {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows}
  else
    failwith "InsufficientPermission"

let make_permission (state:t) ~permission ~burrow_id ~rights =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.is_admin_right r then
    (* only admins can create permissions. *)
    Ligo.Tezos.create_ticket
      (rights, burrow_id, 0)
      (Ligo.nat_from_literal "0n")
  else
    failwith "InsufficientPermission"

let invalidate_all_permissions (state:t) ~permission ~burrow_id =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  assert_burrow_has_no_unclaimed_slices state burrow;
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if Permission.is_admin_right r then
    (* only admins can invalidate all permissions. *)
    let updated_version, updated_burrow = Burrow.increase_permission_version state.parameters burrow in
    let updated_state = {state with burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows} in
    let admin_ticket =
      Ligo.Tezos.create_ticket
        (Permission.Admin, burrow_id, updated_version)
        (Ligo.nat_from_literal "0n") in
    (admin_ticket, updated_state)
  else
    failwith "InsufficientPermission"

(* TODO: Arthur: one time we might want to trigger garbage collection of
 * slices is during a liquidation. a liquidation creates one slice, so if we
 * clear one pending slice when that happens it won't grow unbounded (yes,
 * there are degenerate cases where the queue starts growing much faster that
 * the auctions are happening and in those instances it could grow unbounded,
 * but roughly speaking in most cases it should average out) *)
let mark_for_liquidation (state:t) ~burrow_id =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  match Burrow.request_liquidation state.parameters burrow with
  | Unnecessary -> failwith "NotLiquidationCandidate"
  | Partial details | Complete details | Close details ->
    let liquidation_slice =
      LiquidationAuctionTypes.{
        burrow = burrow_id;
        tez = details.tez_to_auction;
        min_kit_for_unwarranted = details.min_kit_for_unwarranted;
        older = (
          match Burrow.liquidation_slices burrow with
          | None -> None
          | Some i -> Some i.youngest
        );
        younger = None;
      } in
    let (updated_liquidation_auctions, leaf_ptr) =
      LiquidationAuction.send_to_auction state.liquidation_auctions liquidation_slice in

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
        Mem.mem_update
          updated_liquidation_auctions.avl_storage
          (ptr_of_leaf_ptr older_ptr) @@ fun older ->
        match older with
        | Leaf l -> Leaf
                      { l with value = { l.value with younger = Some leaf_ptr; }; }
        | _ -> (failwith "impossible" : LiquidationAuctionTypes.node)
    ) in

    (* Update the burrow's liquidation slices with the pointer to the newly
     * created liquidation slice. *)
    let updated_burrow =
      Burrow.set_liquidation_slices
        details.burrow_state
        (match Burrow.liquidation_slices details.burrow_state with
         | None -> Some Burrow.{ oldest=leaf_ptr; youngest=leaf_ptr; }
         | Some s -> Some { s with youngest=leaf_ptr; })
    in
    ( Tez.{destination = !Ligo.Tezos.sender; amount = details.liquidation_reward},
      {state with
       burrows = Ligo.Big_map.update burrow_id (Some updated_burrow) state.burrows;
       liquidation_auctions = { updated_liquidation_auctions with avl_storage = updated_storage; };
      }
    )

(* Update the immediate neighbors of a slice (i.e. the younger and the older)
 * to point to each other instead of the slice in question, so that it can be
 * removed. *)
(* NOTE: the liquidation slice must be the one pointed to by the leaf pointer. *)
let update_immediate_neighbors state (leaf_ptr: LiquidationAuctionTypes.leaf_ptr) (leaf : LiquidationAuctionTypes.liquidation_slice) =
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
                                     (fun younger ->
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
                                     (fun older ->
                                        assert (older.younger = Some leaf_ptr);
                                        { older with younger = leaf.younger }
                                     )
                               }}
  ) in
  state

(* Cancel the liquidation of a slice. The burden is on the caller to provide
 * both the burrow_id and the leaf_ptr. *)
let cancel_liquidation_slice (state: t) ~permission ~burrow_id (leaf_ptr: LiquidationAuctionTypes.leaf_ptr): t =
  assert_no_tez_given ();
  let burrow = find_burrow state burrow_id in
  let r = assert_valid_permission ~permission ~burrow_id ~burrow in
  if not (Permission.does_right_allow_cancelling_liquidations r) then
    failwith "InsufficientPermission"
  else
    let root = avl_find_root state.liquidation_auctions.avl_storage leaf_ptr in
    if root <> state.liquidation_auctions.queued_slices
    then failwith "UnwarrantedCancellation"
    else
      let leaf = avl_read_leaf state.liquidation_auctions.avl_storage leaf_ptr in

      match Ligo.Big_map.find_opt leaf.burrow state.burrows with
      | None -> (failwith "invariant violation" : t)
      | Some b when b <> burrow ->
        failwith "SlicePointsToDifferentBurrow"
      | Some _ when Burrow.is_overburrowed state.parameters burrow ->
        failwith "UnwarrantedCancellation"
      | Some _ ->
        let state =
          let (new_storage, _) = avl_del state.liquidation_auctions.avl_storage leaf_ptr in
          { state with
            liquidation_auctions = {
              state.liquidation_auctions with
              avl_storage = new_storage }} in

        (* Return the tez to the burrow and update its pointers to liq. slices. *)
        let burrow = Burrow.return_slice_from_auction leaf_ptr leaf burrow in
        let state =
          { state with
            burrows = Ligo.Big_map.update leaf.burrow (Some burrow) state.burrows } in

        (* And we update the slices around it *)
        let state = update_immediate_neighbors state leaf_ptr leaf in
        assert_invariants state;
        state

let touch_liquidation_slice (state: t) (leaf_ptr: LiquidationAuctionTypes.leaf_ptr): t =
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
             (make_ratio (Common.tez_to_mutez leaf.tez) (Common.tez_to_mutez outcome.sold_tez))
             (kit_to_ratio outcome.winning_bid.kit)
          ) in
      let penalty =
        if corresponding_kit < leaf.min_kit_for_unwarranted then
          kit_of_ratio_ceil (mul_ratio (kit_to_ratio corresponding_kit) Constants.liquidation_penalty)
        else
          kit_zero
      in
      (kit_sub corresponding_kit penalty, penalty)
    in

    (* Burn the kit by removing it from circulation. *)
    let state =
      { state with
        parameters = Parameters.remove_circulating_kit state.parameters kit_to_burn } in

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
          liquidation_auctions = LiquidationAuction.pop_completed_auction state.liquidation_auctions auction;
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
                       | None -> (failwith "TODO: Check if this case can happen." : Burrow.t)
                       | Some b -> b;
                     in
                     Ligo.Big_map.update
                       leaf.burrow
                       (Some (Burrow.return_kit_from_auction leaf_ptr leaf kit_to_repay burrow))
                       state.burrows
      } in

    (* And we update the slices around it *)
    let state = update_immediate_neighbors state leaf_ptr leaf in
    assert_invariants state;
    state

let touch_liquidation_slices (state: t) (slices: LiquidationAuctionTypes.leaf_ptr list) : t =
  List.fold_left touch_liquidation_slice state slices

(* ************************************************************************* *)
(**                          DELEGATION AUCTIONS                             *)
(* ************************************************************************* *)

let updated_delegation_auction state new_auction =
  let prev_auction = state.delegation_auction in
  (* When we move to a new cycle, we accrue the amount that won delegation for
     the previous cycle to uniswap. *)
  let accrued_tez =
    Option.value
      (if DelegationAuction.cycle prev_auction != DelegationAuction.cycle new_auction
       then DelegationAuction.winning_amount prev_auction else None)
      ~default:(Ligo.tez_from_literal "0mutez") in
  { state with
    delegation_auction = new_auction;
    delegate = DelegationAuction.delegate new_auction;
    uniswap = Uniswap.add_accrued_tez state.uniswap accrued_tez;
  }

let delegation_auction_place_bid (state: t) =
  let ticket, auction =
    DelegationAuction.place_bid
      state.delegation_auction
      !Ligo.Tezos.sender
      !Ligo.Tezos.amount in
  (ticket, updated_delegation_auction state auction)

let delegation_auction_claim_win state ~bid_ticket =
  let auction = DelegationAuction.claim_win state.delegation_auction bid_ticket in
  updated_delegation_auction state auction

let delegation_auction_reclaim_bid state ~bid_ticket =
  assert_no_tez_given ();
  let tez, auction = DelegationAuction.reclaim_bid state.delegation_auction bid_ticket in
  let tez_payment = Tez.{destination = !Ligo.Tezos.sender; amount = tez} in
  (tez_payment, updated_delegation_auction state auction)

let touch_delegation_auction state =
  updated_delegation_auction state (DelegationAuction.touch state.delegation_auction)

(* ************************************************************************* *)
(**                                UNISWAP                                   *)
(* ************************************************************************* *)

let buy_kit (state:t) ~min_kit_expected ~deadline =
  let state = touch_delegation_auction state in
  let (kit, updated_uniswap) = Uniswap.buy_kit state.uniswap ~amount:!Ligo.Tezos.amount ~min_kit_expected ~deadline in
  (kit, {state with uniswap = updated_uniswap}) (* TODO: kit must be given to !Ligo.Tezos.sender *)

let sell_kit (state:t) ~kit ~min_tez_expected ~deadline =
  let state = touch_delegation_auction state in
  let kit = assert_valid_kit_token kit in
  let (tez, updated_uniswap) = Uniswap.sell_kit state.uniswap ~amount:!Ligo.Tezos.amount kit ~min_tez_expected ~deadline in
  let tez_payment = Tez.{destination = !Ligo.Tezos.sender; amount = tez;} in
  let updated_state = {state with uniswap = updated_uniswap} in
  (tez_payment, updated_state)

let add_liquidity (state:t) ~max_kit_deposited ~min_lqt_minted ~deadline =
  let state = touch_delegation_auction state in
  let pending_accrual = Option.value (DelegationAuction.winning_amount state.delegation_auction) ~default:(Ligo.tez_from_literal "0mutez") in
  let max_kit_deposited = assert_valid_kit_token max_kit_deposited in
  let (tokens, leftover_kit, updated_uniswap) =
    Uniswap.add_liquidity state.uniswap ~amount:!Ligo.Tezos.amount ~pending_accrual ~max_kit_deposited ~min_lqt_minted ~deadline in
  (tokens, leftover_kit, {state with uniswap = updated_uniswap}) (* TODO: tokens must be given to sender *)

let remove_liquidity (state:t) ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline =
  let state = touch_delegation_auction state in
  let (tez, kit, updated_uniswap) =
    Uniswap.remove_liquidity state.uniswap ~amount:!Ligo.Tezos.amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline in
  let tez_payment = Tez.{destination = !Ligo.Tezos.sender; amount = tez;} in
  let updated_state = {state with uniswap = updated_uniswap} in
  (tez_payment, kit, updated_state) (* TODO: kit must be given to !Ligo.Tezos.sender *)

(* ************************************************************************* *)
(**                          LIQUIDATION AUCTIONS                            *)
(* ************************************************************************* *)

let liquidation_auction_place_bid state ~kit =
  assert_no_tez_given ();
  let kit = assert_valid_kit_token kit in
  let kit, _ = read_kit kit in (* TODO: should not destroy; should change the auction logic instead! *)

  let bid = LiquidationAuctionTypes.{ address=(!Ligo.Tezos.sender); kit=kit; } in
  let current_auction = LiquidationAuction.get_current_auction state.liquidation_auctions in

  let (new_current_auction, bid_ticket) = LiquidationAuction.place_bid current_auction bid in

  ( bid_ticket,
    { state with
      liquidation_auctions=
        { state.liquidation_auctions with
          current_auction = Some new_current_auction;
        };
    }
  )

let liquidation_auction_reclaim_bid state ~bid_ticket =
  assert_no_tez_given ();
  let bid_ticket = LiquidationAuction.assert_valid_bid_ticket bid_ticket in
  let kit = LiquidationAuction.reclaim_bid state.liquidation_auctions bid_ticket in
  kit_issue kit (* TODO: should not issue; should change the auction logic instead! *)

let liquidation_auction_reclaim_winning_bid state ~bid_ticket =
  assert_no_tez_given ();
  let bid_ticket = LiquidationAuction.assert_valid_bid_ticket bid_ticket in
  let (tez, liquidation_auctions) = LiquidationAuction.reclaim_winning_bid state.liquidation_auctions bid_ticket in
  let tez_payment = Tez.{destination = !Ligo.Tezos.sender; amount = tez;} in
  (tez_payment, {state with liquidation_auctions })

(* TODO: Maybe we should provide an entrypoint for increasing a losing bid.
 * *)

(* ************************************************************************* *)
(**                              TOUCHING                                    *)
(* ************************************************************************* *)

(** Calculate how much is right now the reward for touching the main checker
  * contract. We use a bracketed calculation, where for the first
  * touch_reward_low_bracket seconds the reward increases by touch_low_reward
  * per second, and after that by touch_high_reward per second. *)
let calculate_touch_reward (state:t) : kit =
  assert (state.parameters.last_touched <= !Ligo.Tezos.now);
  let duration_in_seconds = Ligo.sub_timestamp_timestamp !Ligo.Tezos.now state.parameters.last_touched in
  let low_duration = Common.min_int duration_in_seconds Constants.touch_reward_low_bracket in
  let high_duration =
    Common.max_int
      (Ligo.int_from_literal "0")
      (Ligo.sub_int_int duration_in_seconds Constants.touch_reward_low_bracket) in

  let touch_low_reward = fixedpoint_of_ratio_ceil Constants.touch_low_reward in
  let touch_high_reward = fixedpoint_of_ratio_ceil Constants.touch_high_reward in
  kit_scale
    kit_one
    (fixedpoint_add
       (fixedpoint_mul (fixedpoint_of_int low_duration) touch_low_reward)
       (fixedpoint_mul (fixedpoint_of_int high_duration) touch_high_reward)
    )

let touch (state:t) ~(index:Ligo.tez) : (kit_token * t) =
  if state.parameters.last_touched = !Ligo.Tezos.now then
    (* Do nothing if up-to-date (idempotence) *)
    (kit_issue kit_zero, state)
  else
    (* TODO: What is the right order in which to do things here? We use the
     * last observed kit_in_tez price from uniswap to update the parameters,
     * which return kit to be added to the uniswap contract. Gotta make sure we
     * do things in the right order here. *)

    (* 1: Calculate the reward that we should create out of thin air to give
     * to the contract toucher, and update the circulating kit accordingly.*)
    let reward = calculate_touch_reward state in
    let state = { state with parameters =
                               Parameters.add_circulating_kit state.parameters reward } in

    (* Ensure the delegation auction is up-to-date, and any proceeds accrued to the uniswap *)
    let state = touch_delegation_auction state in

    (* 2: Update the system parameters *)
    let total_accrual_to_uniswap, updated_parameters =
      Parameters.touch index (Uniswap.kit_in_tez_in_prev_block state.uniswap) state.parameters
    in
    (* 3: Add accrued burrowing fees to the uniswap sub-contract *)
    let total_accrual_to_uniswap = kit_issue total_accrual_to_uniswap in
    let updated_uniswap = Uniswap.add_accrued_kit state.uniswap total_accrual_to_uniswap in

    (* 5: Update auction-related info (e.g. start a new auction) *)
    let updated_liquidation_auctions =
      LiquidationAuction.touch
        state.liquidation_auctions
        (* Start the auction using the current liquidation price. We could
         * also have calculated the price right now directly using the oracle
         * feed as (tz_t * q_t), or use the current minting price, but using
         * the liquidation price is the safest option. *)
        (* George: I use ceil, to stay on the safe side (higher-price) *)
        (fixedpoint_of_ratio_ceil (Parameters.minting_price updated_parameters)) in

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

    let rec touch_oldest (maximum: int) (st: t) : t =
      if maximum <= 0 then st
      else
        match LiquidationAuction.oldest_completed_liquidation_slice st.liquidation_auctions with
        | None -> st
        | Some leaf -> touch_oldest (maximum - 1) (touch_liquidation_slice st leaf) in

    (* TODO: Figure out how many slices we can process per checker touch.*)
    let state = touch_oldest Constants.number_of_slices_to_process state in
    assert_invariants state;

    (* TODO: Add more tasks here *)
    (kit_issue reward, state)
