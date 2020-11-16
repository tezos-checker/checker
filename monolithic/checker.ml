open Burrow

(* TODO: Things to consider / action items:
 *
 * * What if compute_tez_to_auction returns something positive?
 *   => Create a kit UTXO for the burrow owner.
 *
 * * Implement auctioning logic.
 *
 * * George: Do we need >>= for type result?
 *
 * * TODO: George: I think that all operations should return something like (X
 *   result * Checker.t) and not ((X * Checker.t) result); even if the operation
 *   fails for some reason, I'd assume that we wish to keep the updated state of
 *   the contract, right?
*)

module PtrMap = Map.Make(Ptr)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

type burrow_id = Ptr.t

module Checker : sig
  type t =
    { burrows : Burrow.t PtrMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      auctions : Auction.auctions;
    }

  type Error.error +=
    | OwnershipMismatch of Address.t * Burrow.t
    | NonExistentBurrow of burrow_id
    | NotLiquidationCandidate of burrow_id

  val empty_checker_state : Timestamp.t -> t

  (** Perform housekeeping tasks on the contract state. This includes:
    * - Updating the parameters. TODO: We have to find a way to represent
    *   external inputs here; the inputs that Parameters.step requires.
    * - NOTE: Are there any other tasks to put in this list?
  *)
  val touch : t -> now:Timestamp.t -> index:FixedPoint.t -> t

  (* ************************************************************************* *)
  (**                               BURROWS                                    *)
  (* ************************************************************************* *)

  (** Create and return a new burrow owned by the given owner, containing the
    * given tez as collateral, minus the creation deposit. Fail if the tez is
    * not enough to cover the creation deposit.
    * NOTE: Call Checker.touch too. *)
  val create_burrow : t -> owner:Address.t -> amount:Tez.t -> (burrow_id * t, Error.error) result

  (** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
    * someone else owns the burrow, or if the burrow does not exist.
    * NOTE: Call Checker.touch too. *)
  val deposit_tez : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Tez.t -> (t, Error.error) result

  (** Withdraw a non-negative amount of tez from a burrow. Fail if someone else
    * owns this burrow, if this action would overburrow it, or if the burrow
    * does not exist.
    * NOTE: Call Checker.touch too. *)
  val withdraw_tez : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Tez.t -> (Tez.t * t, Error.error) result

  (** Mint kits from a specific burrow. Fail if there is not enough collateral,
    * if the burrow owner does not match, or if the burrow does not exist.
    * NOTE: Call Checker.touch too. *)
  val mint_kit : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Kit.t -> (Kit.t * t, Error.error) result

  (** Deposit/burn a non-negative amount of kit to a burrow. If there is
    * excess kit, simply store it into the burrow. Fail if the burrow owner
    * does not match, or if the burrow does not exist.
    * NOTE: Call Checker.touch too. *)
  val burn_kit : t -> owner:Address.t -> burrow_id:burrow_id -> amount:Kit.t -> (t, Error.error) result

  (** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
    * liquidation or if the burrow does not exist. If successful, return the
    * reward, to be credited to the liquidator.
    * NOTE: Call Checker.touch too. *)
  val mark_for_liquidation : t -> liquidator:Address.t -> burrow_id:burrow_id -> (Tez.t * t, Error.error) result

  (** Process the liquidation slices on completed auctions. Invalid leaf_ptr's
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
  val buy_kit : t -> now:Timestamp.t -> amount:Tez.t -> min_kit_expected:Kit.t -> deadline:Timestamp.t -> (Kit.t * t, Error.error) result

  (** Sell some kit to the uniswap contract. Fail if the desired amount of tez
    * cannot be bought or if the deadline has passed. *)
  val sell_kit : t -> now:Timestamp.t -> Kit.t -> min_tez_expected:Tez.t -> deadline:Timestamp.t -> (Tez.t * t, Error.error) result

  (** Buy some liquidity (liquidity tokens) from the uniswap contract, by
    * giving it some tez and some kit. If the given amounts do not have the
    * right ratio, the uniswap contract keeps as much of the given tez and kit
    * as possible with the right ratio, and returns the leftovers, along with
    * the liquidity tokens. *)
  val add_liquidity : t -> amount:Tez.t -> max_kit_deposited:Kit.t -> min_lqt_minted:Uniswap.liquidity -> now:Timestamp.t -> deadline:Timestamp.t -> (Uniswap.liquidity * Tez.t * Kit.t * t, Error.error) result

  (** Sell some liquidity (liquidity tokens) to the uniswap contract in
    * exchange for the corresponding tez and kit of the right ratio. *)
  val remove_liquidity : t -> lqt_burned:Uniswap.liquidity -> min_tez_withdrawn:Tez.t -> min_kit_withdrawn:Kit.t -> now:Timestamp.t -> deadline:Timestamp.t -> (Tez.t * Kit.t * t, Error.error) result

  (* ************************************************************************* *)
  (**                               AUCTIONS                                   *)
  (* ************************************************************************* *)

  (** Bid in current auction. Fail if the auction is closed, or if the bid is
    * too low. If successful, return a token which can be used to either
    * reclaim the kit when overbid, or claim the auction result. *)
  val place_bid : t -> now:Timestamp.t -> sender:Address.t -> amount:Kit.t -> (Auction.bid_ticket * t, Error.error) result

  (** Reclaim a failed bid for the current or a completed auction. *)
  val reclaim_bid : t -> address:Address.t -> bid_ticket:Auction.bid_ticket
    -> (Kit.t, Error.error) result

  (** Reclaim a winning bid for the current or a completed auction. *)
  val reclaim_winning_bid : t -> address:Address.t -> bid_ticket:Auction.bid_ticket
    -> (Tez.t, Error.error) result

  (* (\** Increase a failed bid for the current auction. *\)
   * val increase_bid : t -> address:Address.t -> increase:Kit.t -> bid_ticket:Auction.bid_ticket
   *   -> (Auction.bid_ticket, Error.error) result *)
end =
struct
  type t =
    { burrows : Burrow.t PtrMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      auctions : Auction.auctions;
    }

  let empty_checker_state ts =
    { burrows = PtrMap.empty;
      uniswap =
        Uniswap.{ tez = Tez.one; kit = Kit.one;
                  total_liquidity_tokens = Uniswap.liquidity_of_int 1; };
      parameters =
        Parameters.{ q = FixedPoint.one;
          index = Tez.one;
          protected_index = Tez.one;
          target = FixedPoint.one;
          drift = FixedPoint.zero;
          drift' = FixedPoint.zero;
          burrow_fee_index = FixedPoint.one;
          imbalance_index = FixedPoint.one;
          outstanding_kit = Kit.of_mukit 1_000_000;
          circulating_kit = Kit.of_mukit 1_000_000;
          last_touched = ts;
        };

      auctions =
        Auction.empty;
    }

  type Error.error +=
    | OwnershipMismatch of Address.t * Burrow.t
    | NonExistentBurrow of burrow_id
    | NotLiquidationCandidate of burrow_id

  (* Utility function to give us burrow addresses *)
  let mk_next_burrow_id (burrows: Burrow.t PtrMap.t) : burrow_id =
    match PtrMap.max_binding_opt burrows with
    | None -> Ptr.init
    | Some (id, _) -> Ptr.next id

  let touch (state:t) ~(now:Timestamp.t) ~(index:FixedPoint.t) : t =
    if state.parameters.last_touched = now then
      (* Do nothing if up-to-date (idempotence) *)
      state
    else
      (* TODO: What is the right order in which to do things here? We use the
       * last observed kit_in_tez price from uniswap to update the parameters,
       * which return kit to be added to the uniswap contract. Gotta make sure we
       * do things in the right order here. *)
      (* 1: Update the system parameters *)
      let total_accrual_to_uniswap, updated_parameters =
        Parameters.step now index (FixedPoint.of_q_floor (Uniswap.kit_in_tez state.uniswap)) state.parameters (* TODO: Should stick with Q.t here I think *)
      in
      (* 2: Add accrued burrowing fees to the uniswap sub-contract *)
      let updated_uniswap = Uniswap.add_accrued_kit state.uniswap total_accrual_to_uniswap in
      let updated_auctions =
        Auction.touch
          state.auctions
          now
          (* TODO: This has unit kit / tez, but the docs only pass tz, so we
             multiply it with q here. Check if this is correct. *)
          FixedPoint.(Tez.to_fp updated_parameters.index * updated_parameters.q) in
      (* TODO: Add more tasks here *)
      { burrows = state.burrows; (* leave as-is *)
        parameters = updated_parameters;
        uniswap = updated_uniswap;
        auctions = updated_auctions;
      }

  (* ************************************************************************* *)
  (**                               BURROWS                                    *)
  (* ************************************************************************* *)

  let create_burrow (state:t) ~(owner:Address.t) ~(amount:Tez.t) =
    (* TODO: Call Checker.touch. *)
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
    (* TODO: Call Checker.touch. *)
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner ->
      assert (state.parameters.last_touched = burrow.last_touched);
      let updated_burrow = Burrow.deposit_tez state.parameters amount burrow in
      Ok {state with burrows = PtrMap.add burrow_id updated_burrow state.burrows}
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow burrow_id)

  let mint_kit (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Kit.t) =
    (* TODO: Call Checker.touch. *)
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner -> (
        assert (state.parameters.last_touched = burrow.last_touched);
        match Burrow.mint_kit state.parameters amount burrow with
        | Ok (updated_burrow, minted) ->
          assert (amount = minted);
          Ok ( minted,
               {state with
                burrows = PtrMap.add burrow_id updated_burrow state.burrows;
                parameters = Parameters.add_circulating_kit state.parameters minted;
               }
             )
        | Error err -> Error err
      )
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow burrow_id)

  let withdraw_tez (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Tez.t) =
    (* TODO: Call Checker.touch. *)
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner -> (
        assert (state.parameters.last_touched = burrow.last_touched);
        match Burrow.withdraw_tez state.parameters amount burrow with
        | Ok (updated_burrow, withdrawn) ->
          assert (amount = withdrawn);
          Ok (withdrawn, {state with burrows = PtrMap.add burrow_id updated_burrow state.burrows})
        | Error err -> Error err
      )
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow burrow_id)

  let burn_kit (state:t) ~(owner:Address.t) ~burrow_id ~(amount:Kit.t) =
    (* TODO: Call Checker.touch. *)
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner ->
      assert (state.parameters.last_touched = burrow.last_touched);
      let updated_burrow = Burrow.burn_kit state.parameters amount burrow in
      (* TODO: What should happen if the following is violated? *)
      assert (state.parameters.circulating_kit >= amount);
      Ok {state with
          burrows = PtrMap.add burrow_id updated_burrow state.burrows;
          parameters = Parameters.remove_circulating_kit state.parameters amount;
         }
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow burrow_id)

  (* TODO: the liquidator's address must be used, eventually. *)
  let mark_for_liquidation (state:t) ~liquidator:_ ~burrow_id =
    (* TODO: Call Checker.touch. *)
    match PtrMap.find_opt burrow_id state.burrows with
    | Some burrow -> (
        assert (state.parameters.last_touched = burrow.last_touched);
        match Burrow.request_liquidation state.parameters burrow with
        | Unnecessary -> Error (NotLiquidationCandidate burrow_id)
        | Partial details | Complete details | Close details ->
          (* TODO: At the moment the auction machinery and representation do
           * not have support for the min_received_kit_for_unwarranted field
           * (used to determine what to return to the burrow, once an auction
           * is over), but they should.  *)
          let liquidation_slice =
            Auction.{
              burrow = burrow_id;
              tez = details.tez_to_auction;
              min_kit_for_unwarranted = details.min_kit_for_unwarranted;
              older = Option.map
                  Burrow.(fun i -> i.youngest)
                  burrow.liquidation_slices;
              younger = None;
            } in
          let updated_auctions, leaf_ptr =
            Auction.send_to_auction state.auctions liquidation_slice in

          (* Fixup the previous youngest pointer since the newly added slice
           * is even younger.
           *
           * This is hacky, but couldn't figure out a nicer way, please do
           * refactor if you do.
          *)
          let updated_storage = (
            match liquidation_slice.older with
            | None -> updated_auctions.storage
            | Some older_ptr ->
                BigMap.mem_update
                  updated_auctions.storage
                  (Avl.ptr_of_leaf_ptr older_ptr) @@ fun older ->
                match older with
                | Leaf l -> Leaf
                              { l with value = { l.value with younger = Some leaf_ptr; }; }
                | _ -> failwith "impossible"
          ) in

          (* TODO: updated_burrow needs to keep track of (leaf_ptr, tez_to_auction) here! *)
          let updated_burrow =
            { details.burrow_state with
              liquidation_slices =
                match details.burrow_state.liquidation_slices with
                | None -> Some Burrow.{ oldest=leaf_ptr; youngest=leaf_ptr; }
                | Some s -> Some { s with youngest=leaf_ptr; };
            } in
          Ok ( details.liquidation_reward,
               {state with
                burrows = PtrMap.add burrow_id updated_burrow state.burrows;
                auctions = { updated_auctions with storage = updated_storage; };
               }
             )
      )
    | None -> Error (NonExistentBurrow burrow_id)

  let touch_liquidation_slice (state: t) (leaf_ptr: Avl.leaf_ptr): t =
    let root = Avl.find_root state.auctions.storage leaf_ptr in
    match Auction.AvlPtrMap.find_opt root state.auctions.completed_auctions with
      (* The slice does not belong to a completed auction, so we skip it. *)
      | None -> state
      (* If it belongs to a completed auction, we delete the slice *)
      | Some _outcome ->
        (* TODO: Check if leaf_ptr's are valid *)
        let (leaf, _) = Avl.read_leaf state.auctions.storage leaf_ptr in
        let state =
          { state with auctions = { state.auctions with
            storage = Avl.del state.auctions.storage leaf_ptr
          }} in

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
            (* TODO: Also update burrow using the auction_outcome *)
            PtrMap.update
              leaf.burrow
              (fun b -> match b with
               | None -> failwith "TODO: Check if this case can happen."
               | Some burrow ->
                 let slices = Option.get Burrow.(burrow.liquidation_slices) in
                 match (leaf.younger, leaf.older) with
                   | (None, None) ->
                     assert (slices.youngest = leaf_ptr);
                     assert (slices.oldest = leaf_ptr);
                     Some { burrow with
                       liquidation_slices = None
                     }
                   | (None, Some older) ->
                     assert (slices.youngest = leaf_ptr);
                     Some { burrow with
                       liquidation_slices = Some
                         Burrow.{ slices with youngest = older }
                     }
                   | (Some younger, None) ->
                     assert (slices.oldest = leaf_ptr);
                     Some { burrow with
                       liquidation_slices = Some
                         Burrow.{ slices with oldest = younger }
                     }
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
              { state with auctions = { state.auctions with
                storage =
                  Avl.update_leaf
                    state.auctions.storage
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
              { state with auctions = { state.auctions with
                storage =
                  Avl.update_leaf
                    state.auctions.storage
                    older_ptr
                    (fun older ->
                      assert (older.younger = Some leaf_ptr);
                      { older with younger = leaf.younger }
                    )
              }}
        ) in
        state

  let touch_liquidation_slices (state: t) (slices: Avl.leaf_ptr list) : t =
    List.fold_left touch_liquidation_slice state slices

  (* ************************************************************************* *)
  (**                                UNISWAP                                   *)
  (* ************************************************************************* *)

  (* NOTE: an address is needed too, eventually. *)
  let buy_kit (state:t) ~now ~amount ~min_kit_expected ~deadline =
    match Uniswap.buy_kit state.uniswap ~amount ~min_kit_expected ~now ~deadline with
    | Ok (kit, updated_uniswap) -> Ok (kit, {state with uniswap = updated_uniswap})
    | Error err -> Error err

  (* NOTE: an address is needed too, eventually. *)
  let sell_kit (state:t) ~now (kit:Kit.t) ~min_tez_expected ~deadline =
    match Uniswap.sell_kit state.uniswap kit ~min_tez_expected ~now ~deadline with
    | Ok (tez, updated_uniswap) -> Ok (tez, {state with uniswap = updated_uniswap})
    | Error err -> Error err

  (* NOTE: an address is needed too, eventually. *)
  let add_liquidity (state:t) ~amount ~max_kit_deposited ~min_lqt_minted ~now ~deadline =
    match Uniswap.add_liquidity state.uniswap ~amount ~max_kit_deposited ~min_lqt_minted ~now ~deadline with
    | Error err -> Error err
    | Ok (tokens, leftover_tez, leftover_kit, updated_uniswap) ->
        Ok (tokens, leftover_tez, leftover_kit, {state with uniswap = updated_uniswap})

  (* NOTE: an address is needed too, eventually. *)
  let remove_liquidity (state:t) ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~now ~deadline =
    match Uniswap.remove_liquidity state.uniswap ~lqt_burned ~min_tez_withdrawn ~min_kit_withdrawn ~now ~deadline with
    | Error err -> Error err
    | Ok (tez, kit, updated_uniswap) ->
        Ok (tez, kit, {state with uniswap = updated_uniswap})

  (* ************************************************************************* *)
  (**                               AUCTIONS                                   *)
  (* ************************************************************************* *)

  let place_bid state ~now ~sender ~amount =
    let bid = { Auction.address=sender; kit=amount; } in
    match
      Auction.with_current_auction state.auctions @@
      fun auction -> Auction.place_bid now auction bid with
    | Error err -> Error err
    | Ok (new_auctions, bid_ticket) ->
      Ok (
        bid_ticket,
        {state with auctions=new_auctions;}
      )

  let reclaim_bid state ~address:_ ~bid_ticket =
    Auction.reclaim_bid state.auctions bid_ticket

  let reclaim_winning_bid state ~address:_ ~bid_ticket =
    Auction.reclaim_winning_bid state.auctions bid_ticket

end

