module PtrMap = Map.Make(Ptr)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

(* TODO: At the very end, inline all numeric operations, flatten all Q.t so
 * that we mainly deal with integers directly. Hardwire the constants too,
 * where possible. *)

type burrow_id = Ptr.t

module Checker : sig
  type t =
    { burrows : Burrow.t PtrMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      liquidation_auctions : LiquidationAuction.auctions;
    }

  type Error.error +=
    | OwnershipMismatch of Address.t * Burrow.t
    | NonExistentBurrow of burrow_id
    | NotLiquidationCandidate of burrow_id
    | BurrowHasCompletedLiquidation

  (** Make a fresh state, initialized at the given time. *)
  val initialize : Timestamp.t -> Level.t -> t

  (** Perform housekeeping tasks on the contract state. This includes:
    * - Updating the system parameters
    * - Updating uniswap parameters (e.g. adding accrued burrowing fees to it)
    * - Update auction-related info (e.g. start a new auction)
    * - NOTE: Are there any other tasks to put in this list?
  *)
  val touch : t -> tezos:Tezos.t -> index:Tez.t -> (Kit.t * t)

  (* ************************************************************************* *)
  (**                               BURROWS                                    *)
  (* ************************************************************************* *)

  (** Create and return a new burrow owned by the given owner, containing the
    * given tez as collateral, minus the creation deposit. Fail if the tez is
    * not enough to cover the creation deposit. *)
  val create_burrow : t -> owner:Address.t -> amount:Tez.t -> (burrow_id * t, Error.error) result

  (** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
    * someone else owns the burrow, or if the burrow does not exist. *)
  val deposit_tez : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Tez.t -> (t, Error.error) result

  (** Withdraw a non-negative amount of tez from a burrow. Fail if someone else
    * owns this burrow, if this action would overburrow it, or if the burrow
    * does not exist. *)
  val withdraw_tez : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Tez.t -> (Tez.t * t, Error.error) result

  (** Mint kits from a specific burrow. Fail if there is not enough collateral,
    * if the burrow owner does not match, or if the burrow does not exist. *)
  val mint_kit : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Kit.t -> (Kit.t * t, Error.error) result

  (** Deposit/burn a non-negative amount of kit to a burrow. If there is
    * excess kit, simply store it into the burrow. Fail if the burrow owner
    * does not match, or if the burrow does not exist. *)
  val burn_kit : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Kit.t -> (t, Error.error) result

  (** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
    * liquidation or if the burrow does not exist. If successful, return the
    * reward, to be credited to the liquidator. *)
  val mark_for_liquidation : t -> liquidator:Address.t -> burrow_id:burrow_id -> (Tez.t * t, Error.error) result

  (** Process the liquidation slices on completed liquidation auctions. Invalid leaf_ptr's
    * fail, and slices that correspond to incomplete liquidations are ignored.
  *)
  val touch_liquidation_slices : t -> Avl.leaf_ptr list -> t

  (** Perform maintainance tasks for the burrow.
  *)
  val touch_burrow : t -> burrow_id -> (t, Error.error) result

  (* ************************************************************************* *)
  (**                                UNISWAP                                   *)
  (* ************************************************************************* *)

  (** Buy some kit from the uniswap contract. Fail if the desired amount of kit
    * cannot be bought or if the deadline has passed. *)
  (* NOTE: an address is needed too, eventually. *)
  val buy_kit : t -> tezos:Tezos.t -> amount:Tez.t -> min_kit_expected:Kit.t -> deadline:Timestamp.t -> (Kit.t * t, Error.error) result

  (** Sell some kit to the uniswap contract. Fail if the desired amount of tez
    * cannot be bought or if the deadline has passed. *)
  val sell_kit : t -> tezos:Tezos.t -> amount:Tez.t -> Kit.t -> min_tez_expected:Tez.t -> deadline:Timestamp.t -> (Tez.t * t, Error.error) result

  (** Buy some liquidity (liquidity tokens) from the uniswap contract, by
    * giving it some tez and some kit. If the given amounts do not have the
    * right ratio, the uniswap contract keeps as much of the given tez and kit
    * as possible with the right ratio, and returns the leftovers, along with
    * the liquidity tokens. *)
  val add_liquidity : t -> tezos:Tezos.t -> amount:Tez.t -> max_kit_deposited:Kit.t -> min_lqt_minted:Uniswap.liquidity -> deadline:Timestamp.t -> (Uniswap.liquidity * Tez.t * Kit.t * t, Error.error) result

  (** Sell some liquidity (liquidity tokens) to the uniswap contract in
    * exchange for the corresponding tez and kit of the right ratio. *)
  val remove_liquidity : t -> tezos:Tezos.t -> amount:Tez.t -> lqt_burned:Uniswap.liquidity -> min_tez_withdrawn:Tez.t -> min_kit_withdrawn:Kit.t -> deadline:Timestamp.t -> (Tez.t * Kit.t * t, Error.error) result

  (* ************************************************************************* *)
  (**                          LIQUIDATION AUCTIONS                            *)
  (* ************************************************************************* *)

  (** Bid in current auction. Fail if the auction is closed, or if the bid is
    * too low. If successful, return a token which can be used to either
    * reclaim the kit when overbid, or claim the auction result. *)
  val liquidation_auction_place_bid : t -> tezos:Tezos.t -> sender:Address.t -> amount:Kit.t -> (LiquidationAuction.bid_ticket * t, Error.error) result

  (** Reclaim a failed bid for the current or a completed auction. *)
  val liquidation_auction_reclaim_bid : t -> address:Address.t -> bid_ticket:LiquidationAuction.bid_ticket
    -> (Kit.t, Error.error) result

  (** Reclaim a winning bid for the current or a completed auction. *)
  val liquidation_auction_reclaim_winning_bid : t -> address:Address.t -> bid_ticket:LiquidationAuction.bid_ticket
    -> (Tez.t * t, Error.error) result

  (* (\** Increase a failed bid for the current auction. *\)
   * val increase_bid : t -> address:Address.t -> increase:Kit.t -> bid_ticket:LiquidationAuction.bid_ticket
   *   -> (LiquidationAuction.bid_ticket, Error.error) result *)
end =
struct
  type t =
    { burrows : Burrow.t PtrMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      liquidation_auctions : LiquidationAuction.auctions;
    }

  let initialize ts level =
    { burrows = PtrMap.empty;
      uniswap = Uniswap.make_initial level;
      parameters = Parameters.make_initial ts;
      liquidation_auctions = LiquidationAuction.empty;
    }

  type Error.error +=
    | OwnershipMismatch of Address.t * Burrow.t
    | NonExistentBurrow of burrow_id
    | NotLiquidationCandidate of burrow_id
    | BurrowHasCompletedLiquidation

  (* Utility function to give us burrow addresses *)
  let mk_next_burrow_id (burrows: Burrow.t PtrMap.t) : burrow_id =
    match PtrMap.max_binding_opt burrows with
    | None -> Ptr.init
    | Some (id, _) -> Ptr.next id

  (* ************************************************************************* *)
  (**                               BURROWS                                    *)
  (* ************************************************************************* *)

  (* Looks up a burrow_id from state, and checks the resulting burrow if:
     * It has the given owner
     * It does not have any completed liquidation slices that needs to be claimed
       before any operation.
  *)
  let with_owned_burrow
      (state: t)
      (burrow_id: burrow_id)
      ~(owner: Address.t)
      (f: Burrow.t -> ('a, Error.error) result)
    : ('a, Error.error) result =
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow ->
      if Burrow.is_owned_by burrow owner
      then
        let is_ready =
          match Burrow.oldest_liquidation_ptr burrow with
          | None -> true
          | Some ls ->
            let root = Avl.find_root state.liquidation_auctions.avl_storage ls in
            let outcome = Avl.root_data state.liquidation_auctions.avl_storage root in
            Option.is_none outcome in
        if is_ready
        then f burrow
        else Error BurrowHasCompletedLiquidation
      else Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow burrow_id)

  let create_burrow (state:t) ~(owner:Address.t) ~(amount:Tez.t) =
    let address = mk_next_burrow_id state.burrows in
    match Burrow.create state.parameters owner amount with
    | Ok burrow -> Ok (address, {state with burrows = PtrMap.add address burrow state.burrows})
    | Error err -> Error err

  let touch_burrow (state: t) (burrow_id: burrow_id) =
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow ->
      let updated_burrow = Burrow.touch state.parameters burrow in
      Ok {state with burrows = PtrMap.add burrow_id updated_burrow state.burrows}
    | None -> Error (NonExistentBurrow burrow_id)

  let deposit_tez (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Tez.t) =
    with_owned_burrow state burrow_id ~owner @@ fun burrow ->
    let updated_burrow = Burrow.deposit_tez state.parameters amount burrow in
    Ok {state with burrows = PtrMap.add burrow_id updated_burrow state.burrows}

  let mint_kit (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Kit.t) =
    with_owned_burrow state burrow_id ~owner @@ fun burrow ->
    match Burrow.mint_kit state.parameters amount burrow with
    | Ok (updated_burrow, minted) ->
      assert (amount = minted);
      Ok ( minted,
           {state with
            burrows = PtrMap.add burrow_id updated_burrow state.burrows;
            parameters =
              Parameters.add_outstanding_kit
                (Parameters.add_circulating_kit state.parameters minted)
                minted;
           }
         )
    | Error err -> Error err

  let withdraw_tez (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Tez.t) =
    with_owned_burrow state burrow_id ~owner @@ fun burrow ->
    match Burrow.withdraw_tez state.parameters amount burrow with
    | Ok (updated_burrow, withdrawn) ->
      assert (amount = withdrawn);
      Ok (withdrawn, {state with burrows = PtrMap.add burrow_id updated_burrow state.burrows})
    | Error err -> Error err

  let burn_kit (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Kit.t) =
    with_owned_burrow state burrow_id ~owner @@ fun burrow ->
    let updated_burrow = Burrow.burn_kit state.parameters amount burrow in
    (* TODO: What should happen if the following is violated? *)
    assert (state.parameters.circulating_kit >= amount);
    Ok {state with
        burrows = PtrMap.add burrow_id updated_burrow state.burrows;
        parameters =
          Parameters.remove_outstanding_kit
            (Parameters.remove_circulating_kit state.parameters amount)
            amount;
       }

  (* TODO: Arthur: one time we might want to trigger garbage collection of
   * slices is during a liquidation. a liquidation creates one slice, so if we
   * clear one pending slice when that happens it won't grow unbounded (yes,
   * there are degenerate cases where the queue starts growing much faster that
   * the auctions are happening and in those instances it could grow unbounded,
   * but roughly speaking in most cases it should average out) *)
  (* TODO: the liquidator's address must be used, eventually. *)
  let mark_for_liquidation (state:t) ~liquidator:_ ~burrow_id =
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow -> (
        match Burrow.request_liquidation state.parameters burrow with
        | Unnecessary -> Error (NotLiquidationCandidate burrow_id)
        | Partial details | Complete details | Close details ->
          let liquidation_slice =
            LiquidationAuction.{
              burrow = burrow_id;
              tez = details.tez_to_auction;
              min_kit_for_unwarranted = details.min_kit_for_unwarranted;
              older = Option.map
                  Burrow.(fun i -> i.youngest)
                  (Burrow.liquidation_slices burrow);
              younger = None;
            } in
          let updated_auctions, leaf_ptr =
            LiquidationAuction.send_to_auction state.liquidation_auctions liquidation_slice in

          (* Fixup the previous youngest pointer since the newly added slice
           * is even younger.
           *
           * This is hacky, but couldn't figure out a nicer way, please do
           * refactor if you do.
          *)
          let updated_storage = (
            match liquidation_slice.older with
            | None -> updated_auctions.avl_storage
            | Some older_ptr ->
              BigMap.mem_update
                updated_auctions.avl_storage
                (Avl.ptr_of_leaf_ptr older_ptr) @@ fun older ->
              match older with
              | Leaf l -> Leaf
                            { l with value = { l.value with younger = Some leaf_ptr; }; }
              | _ -> failwith "impossible"
          ) in

          (* TODO: updated_burrow needs to keep track of (leaf_ptr, tez_to_auction) here! *)
          let updated_burrow =
            Burrow.set_liquidation_slices
              details.burrow_state
              (match Burrow.liquidation_slices details.burrow_state with
               | None -> Some Burrow.{ oldest=leaf_ptr; youngest=leaf_ptr; }
               | Some s -> Some { s with youngest=leaf_ptr; })
          in
          Ok ( details.liquidation_reward,
               {state with
                burrows = PtrMap.add burrow_id updated_burrow state.burrows;
                liquidation_auctions = { updated_auctions with avl_storage = updated_storage; };
               }
             )
      )
    | None -> Error (NonExistentBurrow burrow_id)

  let touch_liquidation_slice (state: t) (leaf_ptr: Avl.leaf_ptr): t =
    let root = Avl.find_root state.liquidation_auctions.avl_storage leaf_ptr in
    match Avl.root_data state.liquidation_auctions.avl_storage root with
    (* The slice does not belong to a completed auction, so we skip it. *)
    | None -> state
    (* If it belongs to a completed auction, we delete the slice *)
    | Some outcome ->
      (* TODO: Check if leaf_ptr's are valid *)
      let (leaf, _) = Avl.read_leaf state.liquidation_auctions.avl_storage leaf_ptr in

      (* How much kit should be given to the burrow and how much should be burned. *)
      let kit_to_repay, kit_to_burn =
        let corresponding_kit = Kit.of_q_floor Q.(
            (Tez.to_q leaf.tez / Tez.to_q outcome.sold_tez) * Kit.to_q outcome.winning_bid.kit
          ) in
        let penalty =
          if corresponding_kit < leaf.min_kit_for_unwarranted then
            Kit.of_q_ceil Q.(Kit.to_q corresponding_kit * Constants.liquidation_penalty)
          else
            Kit.zero
        in
        (Kit.(corresponding_kit - penalty), penalty)
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
        let (new_storage, auction) = Avl.del state.liquidation_auctions.avl_storage leaf_ptr in
        let new_state = { state with liquidation_auctions = { state.liquidation_auctions with avl_storage = new_storage }} in
        (new_state, auction) in

      (* When the auction has no slices left, we pop it from the linked list of lots. We do not
       * delete the auction itself from the storage, since we still want the winner to be able
       * to claim its result. *)
      let state =
        if Avl.is_empty state.liquidation_auctions.avl_storage auction then
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
                       PtrMap.update
                         leaf.burrow
                         (fun b ->
                            match b with
                            | None -> failwith "TODO: Check if this case can happen."
                            | Some burrow ->
                              (* NOTE: We should touch the burrow here I think, before we
                               * do anything else. *)
                              let burrow =
                                Burrow.return_kit_from_auction
                                  leaf.tez kit_to_repay burrow in
                              let slices = Option.get (Burrow.liquidation_slices burrow) in
                              match (leaf.younger, leaf.older) with
                              | (None, None) ->
                                assert (slices.youngest = leaf_ptr);
                                assert (slices.oldest = leaf_ptr);
                                Some Burrow.(set_liquidation_slices burrow None)
                              | (None, Some older) ->
                                assert (slices.youngest = leaf_ptr);
                                Some Burrow.(set_liquidation_slices burrow (Some {slices with youngest = older}))
                              | (Some younger, None) ->
                                assert (slices.oldest = leaf_ptr);
                                Some Burrow.(set_liquidation_slices burrow (Some {slices with oldest = younger}))
                              | (Some _, Some _) ->
                                assert (slices.oldest <> leaf_ptr);
                                assert (slices.youngest <> leaf_ptr);
                                Some burrow
                         )
                         state.burrows
        } in

      (* And we update the slices around it *)
      let state = (
        match leaf.younger with
        | None -> state
        | Some younger_ptr ->
          { state with liquidation_auctions = { state.liquidation_auctions with
                                                avl_storage =
                                                  Avl.update_leaf
                                                    state.liquidation_auctions.avl_storage
                                                    younger_ptr
                                                    (fun younger ->
                                                       assert (younger.older = Some leaf_ptr);
                                                       { younger with older = leaf.older }
                                                    )
                                              }}
      ) in
      let state = (
        match leaf.older with
        | None -> state
        | Some older_ptr ->
          { state with liquidation_auctions = { state.liquidation_auctions with
                                                avl_storage =
                                                  Avl.update_leaf
                                                    state.liquidation_auctions.avl_storage
                                                    older_ptr
                                                    (fun older ->
                                                       assert (older.younger = Some leaf_ptr);
                                                       { older with younger = leaf.younger }
                                                    )
                                              }}
      ) in
      LiquidationAuction.assert_invariants state.liquidation_auctions;
      state

  let touch_liquidation_slices (state: t) (slices: Avl.leaf_ptr list) : t =
    List.fold_left touch_liquidation_slice state slices

  (* ************************************************************************* *)
  (**                                UNISWAP                                   *)
  (* ************************************************************************* *)

  (* NOTE: an address is needed too, eventually. *)
  let buy_kit (state:t) ~tezos ~amount ~min_kit_expected ~deadline =
    match Uniswap.buy_kit state.uniswap ~amount ~min_kit_expected ~tezos ~deadline with
    | Ok (kit, updated_uniswap) -> Ok (kit, {state with uniswap = updated_uniswap})
    | Error err -> Error err

  (* NOTE: an address is needed too, eventually. *)
  let sell_kit (state:t) ~tezos ~amount kit ~min_tez_expected ~deadline =
    match Uniswap.sell_kit state.uniswap ~amount kit ~min_tez_expected ~tezos ~deadline with
    | Ok (tez, updated_uniswap) -> Ok (tez, {state with uniswap = updated_uniswap})
    | Error err -> Error err

  (* NOTE: an address is needed too, eventually. *)
  let add_liquidity (state:t) ~tezos ~amount ~max_kit_deposited ~min_lqt_minted ~deadline =
    match Uniswap.add_liquidity state.uniswap ~amount ~max_kit_deposited ~min_lqt_minted ~tezos ~deadline with
    | Error err -> Error err
    | Ok (tokens, leftover_tez, leftover_kit, updated_uniswap) ->
      Ok (tokens, leftover_tez, leftover_kit, {state with uniswap = updated_uniswap})

  (* NOTE: an address is needed too, eventually. *)
  let remove_liquidity (state:t) ~tezos ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~deadline =
    match Uniswap.remove_liquidity state.uniswap ~amount ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~tezos ~deadline with
    | Error err -> Error err
    | Ok (tez, kit, updated_uniswap) ->
      Ok (tez, kit, {state with uniswap = updated_uniswap})

  (* ************************************************************************* *)
  (**                               AUCTIONS                                   *)
  (* ************************************************************************* *)

  let liquidation_auction_place_bid state ~tezos ~sender ~amount =
    let bid = Bid.{ address=sender; kit=amount; } in
    match
      LiquidationAuction.with_current_auction state.liquidation_auctions @@
      fun auction -> LiquidationAuction.place_bid tezos auction bid with
    | Error err -> Error err
    | Ok (new_auctions, bid_ticket) ->
      Ok (
        bid_ticket,
        {state with liquidation_auctions=new_auctions;}
      )

  let liquidation_auction_reclaim_bid state ~address:_ ~bid_ticket =
    LiquidationAuction.reclaim_bid state.liquidation_auctions bid_ticket

  let liquidation_auction_reclaim_winning_bid state ~address:_ ~bid_ticket =
    match LiquidationAuction.reclaim_winning_bid state.liquidation_auctions bid_ticket with
    | Error err -> Error err
    | Ok (ret, liquidation_auctions) -> Ok (ret, { state with liquidation_auctions })

  (* ************************************************************************* *)
  (**                              TOUCHING                                    *)
  (* ************************************************************************* *)

  (** Calculate how much is right now the reward for touching the main checker
    * contract. We use a bracketed calculation, where for the first
    * touch_reward_low_bracket seconds the reward increases by touch_low_reward
    * per second, and after that by touch_high_reward per second. *)
  let calculate_touch_reward (state:t) ~tezos : Kit.t =
    assert (state.parameters.last_touched <= Tezos.(tezos.now));
    let duration_in_seconds =
      Timestamp.seconds_elapsed
        ~start:state.parameters.last_touched
        ~finish:tezos.now in
    let low_duration = min duration_in_seconds Constants.touch_reward_low_bracket in
    let high_duration = max 0 (duration_in_seconds - Constants.touch_reward_low_bracket) in

    let touch_low_reward = FixedPoint.of_q_ceil Constants.touch_low_reward in (* FLOOR-or-CEIL *)
    let touch_high_reward = FixedPoint.of_q_ceil Constants.touch_high_reward in (* FLOOR-or-CEIL *)
    Kit.scale
      Kit.one
      FixedPoint.(
        of_int low_duration * touch_low_reward
        + of_int high_duration * touch_high_reward
      )

  let touch (state:t) ~tezos ~(index:Tez.t) : (Kit.t * t) =
    if state.parameters.last_touched = Tezos.(tezos.now) then
      (* Do nothing if up-to-date (idempotence) *)
      (Kit.zero, state)
    else
      (* TODO: What is the right order in which to do things here? We use the
       * last observed kit_in_tez price from uniswap to update the parameters,
       * which return kit to be added to the uniswap contract. Gotta make sure we
       * do things in the right order here. *)

      (* 1: Calculate the reward that we should create out of thin air to give
       * to the contract toucher, and update the circulating kit accordingly.*)
      let reward = calculate_touch_reward state ~tezos in
      let state = { state with parameters =
                                 Parameters.add_circulating_kit state.parameters reward } in

      (* 2: Update the system parameters *)
      let total_accrual_to_uniswap, updated_parameters =
        Parameters.touch tezos index (Uniswap.kit_in_tez_in_prev_block state.uniswap) state.parameters
      in
      (* 3: Add accrued burrowing fees to the uniswap sub-contract *)
      let updated_uniswap = Uniswap.add_accrued_kit state.uniswap tezos total_accrual_to_uniswap in
      (* 4: Update auction-related info (e.g. start a new auction) *)
      let updated_auctions =
        LiquidationAuction.touch
          state.liquidation_auctions
          tezos
          (* Start the auction using the current liquidation price. We could
           * also have calculated the price right now directly using the oracle
           * feed as (tz_t * q_t), or use the current minting price, but using
           * the liquidation price is the safest option. *)
          (* George: I use ceil, to stay on the safe side (higher-price) *)
          (FixedPoint.of_q_ceil (Parameters.minting_price updated_parameters)) in

      (* 5: Touch oldest liquidation slices *)
      (* TODO: Touch only runs at most once per block. But it might be benefical to run this step
       * without that restriction. *)
      let new_state =
        { burrows = state.burrows; (* leave as-is *)
          parameters = updated_parameters;
          uniswap = updated_uniswap;
          liquidation_auctions = updated_auctions;
        } in

      let rec touch_oldest (maximum: int) (st: t) : t =
        if maximum <= 0 then st
        else
          match LiquidationAuction.oldest_completed_liquidation_slice st.liquidation_auctions with
          | None -> st
          | Some leaf -> touch_liquidation_slice st leaf |> touch_oldest (maximum - 1) in

      (* TODO: Figure out how many slices we can process per checker touch.*)
      let new_state = touch_oldest Constants.number_of_slices_to_process new_state in

      (* TODO: Add more tasks here *)
      (reward, new_state)
end
