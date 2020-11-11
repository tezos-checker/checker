open Address
open Burrow
open Parameters

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

module AddressMap = Map.Make(Address)

(* ************************************************************************* *)
(**                               CHECKER                                    *)
(* ************************************************************************* *)

module Checker : sig
  type t =
    { burrows : Burrow.t AddressMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      auctions : Auction.auctions;
    }

  (** Perform housekeeping tasks on the contract state. This includes:
    * - Updating the parameters. TODO: We have to find a way to represent
    *   external inputs here; the inputs that Parameters.step requires.
    * - NOTE: Are there any other tasks to put in this list?
  *)
  val touch : t -> now:Timestamp.t -> index:FixedPoint.t -> t

  (** Create and return a new burrow owned by the given owner, containing the
    * given tez as collateral, minus the creation deposit. Fail if the tez is
    * not enough to cover the creation deposit.
    * NOTE: Call Checker.touch too. *)
  val create_burrow : t -> owner:Address.t -> amount:Tez.t -> (Address.t * t, Error.error) result

  (** Deposit a non-negative amount of tez as collateral to a burrow. Fail if
    * someone else owns the burrow, or if the burrow does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val deposit_tez : t -> owner:Address.t -> address:Address.t -> amount:Tez.t -> (t, Error.error) result

  (** Withdraw a non-negative amount of tez from a burrow. Fail if someone else
    * owns this burrow, if this action would overburrow it, or if the burrow
    * does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val withdraw_tez : t -> owner:Address.t -> address:Address.t -> amount:Tez.t -> (Tez.t * t, Error.error) result

  (** Mint kits from a specific burrow. Fail if there is not enough collateral,
    * if the burrow owner does not match, or if the burrow does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val mint_kit : t -> owner:Address.t -> address:Address.t -> amount:Kit.t -> (Kit.t * t, Error.error) result

  (** Deposit/burn a non-negative amount of kit to a burrow. If there is
    * excess kit, simply store it into the burrow. Fail if the burrow owner
    * does not match, or if the burrow does not exist.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val burn_kit : t -> owner:Address.t -> address:Address.t -> amount:Kit.t -> (t, Error.error) result

  (** Mark a burrow for liquidation. Fail if the burrow is not a candidate for
    * liquidation or if the burrow does not exist. If successful, return the
    * reward, to be credited to the liquidator.
    * NOTE: Call Checker.touch too.
    * NOTE: Call Burrow.touch too. *)
  val mark_for_liquidation : t -> liquidator:Address.t -> address:Address.t -> (Tez.t * t, Error.error) result

  (** Bid in current auction. Fail if the auction is closed, or if the bid is
    * too low. If successful, return a token which can be used to either
    * reclaim the kit when overbid, or claim the auction result. *)
  val place_bid : t -> now:Timestamp.t ->  sender:Address.t -> amount:Kit.t -> (Auction.bid_ticket * t, Error.error) result

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
    { burrows : Burrow.t AddressMap.t;
      uniswap : Uniswap.t;
      parameters : Parameters.t;
      auctions : Auction.auctions;
    }

  type Error.error +=
    | OwnershipMismatch of Address.t * Burrow.t
    | NonExistentBurrow of Address.t
    | NotLiquidationCandidate of Address.t

  (* Utility function to give us burrow addresses *)
  let mk_next_burrow_address (burrows: Burrow.t AddressMap.t) : Address.t =
    match AddressMap.max_binding_opt burrows with
    | None -> Address.initial_address
    | Some (a, _) -> Address.next a

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
      (* TODO: Add more tasks here *)
      { burrows = state.burrows; (* leave as-is *)
        parameters = updated_parameters;
        uniswap = updated_uniswap;
        auctions = state.auctions; (* TODO: this definitely needs to be updated! *)
      }

  let create_burrow (state:t) ~(owner:Address.t) ~(amount:Tez.t) =
    (* TODO: Call Checker.touch. *)
    let address = mk_next_burrow_address state.burrows in
    match Burrow.create state.parameters owner amount with
    | Ok burrow -> Ok (address, {state with burrows = AddressMap.add address burrow state.burrows})
    | Error err -> Error err

  let deposit_tez (state:t) ~(owner:Address.t) ~(address:Address.t) ~(amount:Tez.t) =
    (* TODO: Call Checker.touch. *)
    (* TODO: Call Burrow.touch. *)
    match AddressMap.find_opt address state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner ->
      let updated_burrow = Burrow.deposit_tez state.parameters amount burrow in
      Ok {state with burrows = AddressMap.add address updated_burrow state.burrows}
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow address)

  let mint_kit (state:t) ~(owner:Address.t) ~(address:Address.t) ~(amount:Kit.t) =
    (* TODO: Call Checker.touch. *)
    (* TODO: Call Burrow.touch. *)
    match AddressMap.find_opt address state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner -> (
        match Burrow.mint_kit state.parameters amount burrow with
        | Ok (updated_burrow, minted) ->
          assert (amount = minted);
          Ok ( minted,
               {state with
                burrows = AddressMap.add address updated_burrow state.burrows;
                parameters = Parameters.add_circulating_kit state.parameters minted;
               }
             )
        | Error err -> Error err
      )
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow address)

  let withdraw_tez (state:t) ~(owner:Address.t) ~(address:Address.t) ~(amount:Tez.t) =
    (* TODO: Call Checker.touch. *)
    (* TODO: Call Burrow.touch. *)
    match AddressMap.find_opt address state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner -> (
        match Burrow.withdraw_tez state.parameters amount burrow with
        | Ok (updated_burrow, withdrawn) ->
          assert (amount = withdrawn);
          Ok (withdrawn, {state with burrows = AddressMap.add address updated_burrow state.burrows})
        | Error err -> Error err
      )
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow address)

  let burn_kit (state:t) ~(owner:Address.t) ~(address:Address.t) ~(amount:Kit.t) =
    (* TODO: Call Checker.touch. *)
    (* TODO: Call Burrow.touch. *)
    match AddressMap.find_opt address state.burrows with
    | Some burrow when Burrow.is_owned_by burrow owner ->
      let updated_burrow = Burrow.burn_kit state.parameters amount burrow in
      (* TODO: What should happen if the following is violated? *)
      assert (state.parameters.circulating_kit >= amount);
      Ok {state with
          burrows = AddressMap.add address updated_burrow state.burrows;
          parameters = Parameters.remove_circulating_kit state.parameters amount;
         }
    | Some burrow -> Error (OwnershipMismatch (owner, burrow))
    | None -> Error (NonExistentBurrow address)

  (* TODO: the liquidator's address must be used, eventually. *)
  let mark_for_liquidation (state:t) ~liquidator:_ ~(address:Address.t) =
    (* TODO: Call Checker.touch. *)
    (* TODO: Call Burrow.touch. *)
    match AddressMap.find_opt address state.burrows with
    | Some burrow -> (
        match Burrow.request_liquidation state.parameters burrow with
        | Unnecessary -> Error (NotLiquidationCandidate address)
        | Partial details | Complete details | Close details ->
          (* TODO: At the moment the auction machinery and representation do
           * not have support for the min_received_kit_for_unwarranted field
           * (used to determine what to return to the burrow, once an auction
           * is over), but they should.  *)
          let liquidation_slice = Auction.{burrow = address; tez = details.tez_to_auction} in
          let updated_auctions, _leaf_ptr = Auction.send_to_auction state.auctions liquidation_slice in
          (* TODO: updated_burrow needs to keep track of (leaf_ptr, tez_to_auction) here! *)
          let updated_burrow = details.burrow_state in
          Ok ( details.liquidation_reward,
               {state with
                burrows = AddressMap.add address updated_burrow state.burrows;
                auctions = updated_auctions;
               }
             )
      )
    | None -> Error (NonExistentBurrow address)

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

