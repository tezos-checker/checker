open Ctok
open Kit
open Tok
open Lqt
open Burrow
open OUnit2
open TestLib
open CheckerTypes
open Fa2Interface
open Fa2Ledger
open Fa2Implementation
open Error
open Ptr
open LiquidationAuctionTypes
open LiquidationAuction

let property_test_count = 10000
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

module PtrMap = Map.Make(struct type t = ptr let compare = compare_ptr end)

let checker_address = !Ligo.Tezos.self_address

let empty_checker =
  initial_checker
    { oracle = oracle_addr;
      collateral_fa2 = collateral_fa2_addr;
      cfmm_token_fa12 = cfmm_token_fa12_addr;
    }

(* The starting checker state should satisfy the invariants to begin with. *)
let _ = Checker.assert_checker_invariants empty_checker

(* Enhance the initial checker state with a populated cfmm in a consistent way. *)
let empty_checker_with_cfmm (cfmm: CfmmTypes.cfmm) =
  let checker_kit = kit_sub cfmm.kit (kit_of_denomination (Ligo.nat_from_literal "1n")) in
  let checker_liquidity = lqt_sub cfmm.lqt (lqt_of_denomination (Ligo.nat_from_literal "1n")) in
  let checker =
    { empty_checker with
      parameters = { empty_checker.parameters with circulating_kit = checker_kit };
      cfmm = cfmm;
      fa2_state =
        let fa2_state = initial_fa2_state in
        let fa2_state = ledger_issue_lqt (fa2_state, !Ligo.Tezos.self_address, checker_liquidity) in
        let fa2_state = ledger_issue_kit (fa2_state, !Ligo.Tezos.self_address, checker_kit) in
        fa2_state;
    } in
  Checker.assert_checker_invariants checker;
  checker

(* Produces a checker state with liquidatable burrows.
 * Returns a list of the liquidatable burrow ids, underburrowed burrow ids, and the contract state
*)
let checker_with_liquidatable_burrows () =
  let checker = empty_checker in
  (* Create some burrows and mint some kit *)
  let alice_burrow_1 = Ligo.nat_from_literal "0n" in
  let alice_burrow_nos = List.init 20 (fun i -> Ligo.nat_from_int64 (Int64.of_int (i+1))) in
  let bob_burrow_1 = Ligo.nat_from_literal "0n" in
  (* Alice burrow 1. Will NOT be liquidatable *)
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:2 ~sender:alice_addr ~amount:Common.tez_zero;
  let _, checker = Checker.entrypoint_create_burrow (checker, (alice_burrow_1, None, tok_of_denomination (Ligo.nat_from_literal "2_000_000n"))) in
  (* Alice burrow 2:N. Will be liquidatable *)
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:3 ~sender:alice_addr ~amount:Common.tez_zero;
  let _, checker = Checker.entrypoint_mint_kit (checker, (alice_burrow_1, (kit_of_denomination (Ligo.nat_from_literal "100n")))) in
  let checker = List.fold_left (
      fun checker alice_burrow_no ->
        Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
        let _, checker = Checker.entrypoint_create_burrow (checker, (alice_burrow_no, None, tok_of_denomination (Ligo.nat_from_literal "2_000_000n"))) in
        Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
        let _, checker =
          let max_kit = (Checker.view_burrow_max_mintable_kit ((alice_addr, alice_burrow_no), checker)) in
          Checker.entrypoint_mint_kit (checker, (alice_burrow_no, max_kit)) in
        checker
    )
      checker
      alice_burrow_nos
  in
  (* Bob burrow 1. Will be liquidatable. *)
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bob_addr ~amount:Common.tez_zero;
  let _, checker = Checker.entrypoint_create_burrow (checker, (bob_burrow_1, None, tok_of_denomination (Ligo.nat_from_literal "20_000_000n"))) in
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker =
    let max_kit = (Checker.view_burrow_max_mintable_kit ((bob_addr, bob_burrow_1), checker)) in
    Checker.entrypoint_mint_kit (checker, (bob_burrow_1, max_kit)) in

  (* Increase value of kit to make some of the burrows liquidatable by touching checker *)
  (* Note: setting the transaction to far in the future to ensure that the protected_index will become adequately high
   * for the burrows to be liquidatable.
  *)
  Ligo.Tezos.new_transaction ~seconds_passed:10_000_000 ~blocks_passed:100_000 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_100_000n") in
  (* Touch burrows *)
  Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.entrypoint_touch_burrow (checker, (alice_addr, alice_burrow_1)) in
  Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.entrypoint_touch_burrow (checker, (bob_addr, bob_burrow_1)) in
  let checker = List.fold_left (
      fun checker alice_burrow_no ->
        Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
        let _, checker = Checker.entrypoint_touch_burrow (checker, (alice_addr, alice_burrow_no)) in
        checker
    )
      checker
      alice_burrow_nos
  in

  (* Check the expected properties of this test fixture *)
  assert_bool "alice_burrow_1 was liquidatable but it is expected to not be"
    (not (Burrow.burrow_is_liquidatable checker.parameters (Option.get (Ligo.Big_map.find_opt (alice_addr, alice_burrow_1) checker.burrows))));
  assert_bool "bob_burrow_1 was not liquidatable but it is expected to be"
    (Burrow.burrow_is_liquidatable checker.parameters (Option.get (Ligo.Big_map.find_opt (bob_addr, bob_burrow_1) checker.burrows)));
  List.fold_left (
    fun _ alice_burrow_no ->
      assert_bool ("alice_burrow_" ^ (Ligo.string_of_nat alice_burrow_no) ^ " was not liquidatable but it is expected to be")
        (Burrow.burrow_is_liquidatable checker.parameters (Option.get (Ligo.Big_map.find_opt (alice_addr, alice_burrow_no) checker.burrows))))
    ()
    alice_burrow_nos;
  Checker.assert_checker_invariants checker;

  let liquidatable_burrow_ids = List.append (List.map (fun x -> (alice_addr, x)) alice_burrow_nos) [(bob_addr, bob_burrow_1)] in
  let underburrowed_burrow_ids = [(alice_addr, alice_burrow_1)] in
  liquidatable_burrow_ids, underburrowed_burrow_ids, checker

(* Produces a checker state with liquidation slices in the queue but no current auction.
 * Returns a list of details for queued slices related to a Close liquidation,
 * a list of details for all other slices in the queue, and the contract state.
*)
let checker_with_queued_liquidation_slices () =
  let liquidatable_burrow_ids, _, checker = checker_with_liquidatable_burrows () in
  (* Mark the liquidatable burrows for liquidation. This will add slices to the queue. *)
  let checker, close_slice_details, other_slice_details = List.fold_left
      (fun (checker, close_liquidation_slices, other_liquidation_slices) burrow_id ->
         Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let _, checker = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in
         let new_slice = Option.get (SliceList.slice_list_youngest (SliceList.slice_list_from_auction_state checker.liquidation_auctions burrow_id) checker.liquidation_auctions) in
         let slice_ptr = SliceList.slice_list_element_ptr new_slice in
         let slize_tez = (SliceList.slice_list_element_contents new_slice).tok in
         let is_burrow_now_closed = not (burrow_active (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))) in
         let close_liquidation_slices, other_liquidation_slices =
           if is_burrow_now_closed then
             (List.append close_liquidation_slices [(burrow_id, slice_ptr, slize_tez)]), other_liquidation_slices
           else
             close_liquidation_slices, (List.append other_liquidation_slices [(burrow_id, slice_ptr, slize_tez)])
         in
         checker, close_liquidation_slices, other_liquidation_slices
      )
      (checker, [], [])
      liquidatable_burrow_ids
  in
  assert_bool
    "liquidation auction queue was empty, but it was expected to have some slices"
    (Option.is_some (Avl.avl_peek_front checker.liquidation_auctions.avl_storage checker.liquidation_auctions.queued_slices));
  assert (List.length close_slice_details > 0);
  assert (List.length other_slice_details > 0);
  close_slice_details, other_slice_details, checker

(* Produces a checker state with an active liquidation auction *)
let checker_with_active_auction () =
  let _, _, checker = checker_with_queued_liquidation_slices () in
  (* Touch checker to start an auction *)
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.entrypoint_touch (checker, ()) in
  assert_bool "a current liquidation auction should have been started but was not" (Option.is_some checker.liquidation_auctions.current_auction);
  checker

(* Produces a checker state with a completed liquidation auction *)
let checker_with_completed_auction () =
  let checker = checker_with_active_auction () in
  (* Get the current auction minimum bid *)
  let auction_details = Checker.view_current_liquidation_auction_details ((), checker) in
  (* Mint enough kit to bid *)
  let bidder = alice_addr in
  let new_burrow_no = Ligo.nat_from_literal "100n" in
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bidder ~amount:Common.tez_zero;
  let _, checker = Checker.entrypoint_create_burrow (checker, (new_burrow_no, None, tok_of_denomination (Ligo.nat_from_literal "1_000_000_000n"))) in
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bidder ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.entrypoint_mint_kit (checker, (new_burrow_no, auction_details.minimum_bid)) in
  (* Place a bid *)
  Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bidder ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.entrypoint_liquidation_auction_place_bid (checker, ((Option.get checker.liquidation_auctions.current_auction).contents, auction_details.minimum_bid)) in
  (* Wait until enough time has passed for the auction to be completable then touch checker *)
  (* Touch checker to start an auction *)
  Ligo.Tezos.new_transaction ~seconds_passed:1202 ~blocks_passed:22 ~sender:bidder ~amount:(Ligo.tez_from_literal "0mutez");
  let _, checker = Checker.entrypoint_touch (checker, ()) in
  assert_bool
    "there was not a completed liquidation auction but one should exist"
    (Option.is_some checker.liquidation_auctions.completed_auctions);
  bidder, checker

(* Helper for creating new burrows and extracting their ID from the corresponding Ligo Ops *)
let newly_created_burrow (checker: checker) (burrow_no: string) (collateral: tok) : burrow_id * checker =
  let _ops, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, collateral)) in
  ((!Ligo.Tezos.sender, Ligo.nat_from_literal burrow_no), checker)

let get_balance_of (checker: checker) (addr: Ligo.address) (tok: fa2_token_id): Ligo.nat =
  let ops, _checker = Checker.strict_entrypoint_balance_of (checker, { requests = [{ owner=addr; token_id=tok }]; callback=Ligo.contract_of_address addr}) in
  match ops with
  | [ Transaction (FA2BalanceOfResponseTransactionValue [ { request = _; balance = kit } ], _, _) ] -> kit
  | _ -> failwith ("Unexpected fa2 response, got: " ^ show_operation_list ops)

let suite =
  "Checker tests" >::: [
    ("initial touch (noop)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker1 = empty_checker in
       let ops, checker2 = Checker.touch_with_index checker1 (Ligo.nat_from_literal "0n") in

       assert_operation_list_equal ~expected:[] ~real:ops;
       assert_equal checker1 checker2; (* NOTE: we really want them to be identical here, hence the '='. *)
       ()
    );

    ("create_burrow - updates checker storage" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;

       let burrow_id, checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "1_000_000n")) in

       assert_bool
         "No matching burrow found after calling create_burrow"
         (Option.is_some (Ligo.Big_map.find_opt burrow_id checker.burrows));
       assert_bool
         "The burrow existed before calling create_burrow"
         (Option.is_none (Ligo.Big_map.find_opt burrow_id empty_checker.burrows))
    );

    ("create_burrow - collateral in burrow representation does not include creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;

       let burrow_id, checker = newly_created_burrow empty_checker "0n" Constants.creation_deposit in

       let expected_collateral = tok_zero in
       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_tok_equal ~expected:expected_collateral ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("create_burrow - fails when transaction amount is one mutez below creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = tok_sub Constants.creation_deposit (tok_of_denomination (Ligo.nat_from_literal "1n")) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;

       assert_raises
         (Failure (Ligo.string_of_int error_InsufficientFunds))
         (fun () -> Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)))
    );

    ("create_burrow - passes when transaction amount is exactly the creation deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let burrow_id, checker = newly_created_burrow empty_checker "0n" Constants.creation_deposit in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow ->
         assert_tok_equal ~expected:tok_zero ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("deposit_collateral - owner can deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = tok_of_denomination (Ligo.nat_from_literal "3_000_000n") in
       let deposit = tok_of_denomination (Ligo.nat_from_literal "3_000_000n") in
       let expected_collateral = tok_add deposit (tok_sub initial_deposit Constants.creation_deposit) in

       (* Create the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let (_, burrow_no) as burrow_id, checker = newly_created_burrow empty_checker "0n" initial_deposit in
       (* Make a deposit *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_deposit_collateral (checker, (burrow_no, deposit)) in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_tok_equal ~expected:expected_collateral ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("deposit_collateral - non-owner cannot deposit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;

       let _, checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:Common.tez_zero;
       assert_raises
         (Failure (Ligo.string_of_int error_NonExistentBurrow))
         (fun () -> Checker.entrypoint_deposit_collateral (checker, (Ligo.nat_from_literal "0n", tok_of_denomination (Ligo.nat_from_literal "1_000_000n"))))
    );

    ("withdraw_collateral - owner can withdraw" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = tok_of_denomination (Ligo.nat_from_literal "3_000_000n") in
       let withdrawal = tok_of_denomination (Ligo.nat_from_literal "1_000_000n") in
       let expected_collateral = tok_sub initial_deposit (tok_add Constants.creation_deposit withdrawal) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let burrow_id, checker = newly_created_burrow empty_checker "0n" initial_deposit in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_withdraw_collateral (checker, (Ligo.nat_from_literal "0n", withdrawal)) in

       match Ligo.Big_map.find_opt burrow_id checker.burrows with
       | Some burrow -> assert_tok_equal ~expected:expected_collateral ~real:(burrow_collateral burrow)
       | None -> assert_failure "Expected a burrow representation to exist but none was found"
    );

    ("withdraw_collateral - non-owner cannot withdraw" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let initial_deposit = tok_of_denomination (Ligo.nat_from_literal "3_000_000n") in
       let withdrawal = tok_of_denomination (Ligo.nat_from_literal "1_000_000n") in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = newly_created_burrow empty_checker "0n" initial_deposit in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_NonExistentBurrow))
         (fun () -> Checker.entrypoint_withdraw_collateral (checker, (Ligo.nat_from_literal "0n", withdrawal)))
    );

    ("entrypoint_activate_burrow - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow and deactivate it *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let (_, burrow_no), checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_deactivate_burrow (checker, (burrow_no, alice_addr)) in
       (* Then activate it *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let ops, _ = Checker.entrypoint_activate_burrow (checker, (burrow_no, Constants.creation_deposit)) in
       let burrow = Option.get (Ligo.Big_map.find_opt (alice_addr, burrow_no) checker.burrows) in
       let expected_ops = [
         (LigoOp.Tezos.fa2_transfer_transaction
            [ Fa2Interface.(
                  { from_ = alice_addr;
                    txs = [
                      { to_ = burrow_address burrow;
                        token_id = Tok.tok_token_id;
                        amount = Ligo.nat_from_literal "1_000_000n";
                      };
                    ];
                  }
                )
            ]
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.collateral_fa2))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_add_liquidity - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       (* Create a burrow and mint some kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", (kit_of_denomination (Ligo.nat_from_literal "10_000_000n")))) in

       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_add_liquidity
           (checker,
            (* Note: all values here were arbitrarily chosen based on the amount of kit we minted above *)
            ( ctok_of_muctok (Ligo.nat_from_literal "5_000_000n")
            , kit_of_denomination (Ligo.nat_from_literal "5_000_000n")
            , lqt_of_denomination (Ligo.nat_from_literal "5_000_000n")
            , Ligo.timestamp_from_seconds_literal 999
            )
           ) in

       let expected_ops = [
         (LigoOp.Tezos.fa12_transfer_transaction
            Fa12Interface.(
              {address_to=checker_address;
               address_from=alice_addr;
               value=(Ligo.nat_from_literal "5_000_000n")}
            )
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.cfmm_token_fa12))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_burn_kit - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       (* Create a burrow and mint some kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", (kit_of_denomination (Ligo.nat_from_literal "10_000_000n")))) in
       (* Then burn the kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", (kit_of_denomination (Ligo.nat_from_literal "10_000_000n")))) in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_create_burrow - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let amnt = tok_of_denomination (Ligo.nat_from_literal "100_000_000n") in
       let ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amnt)) in
       match ops with
       (* Note: it's not really possible to check the first parameter of the contract here which is the
        * function which defines the contract's logic.
       *)
       | [ (CreateBurrowContract (_, delegate, tez, storage)) ;
           (Transaction (FA2TransferTransactionValue _, _, _)) as op;
         ] ->
         (* burrow creation values *)
         assert_key_hash_option_equal ~expected:None ~real:delegate;
         assert_tez_equal ~expected:Common.tez_zero ~real:tez;
         assert_equal BurrowTypes.({checker_address=checker_address; collateral_fa2=collateral_fa2_addr}) storage;
         (* collateral initialization values *)
         let burrow = Option.get (Ligo.Big_map.find_opt (alice_addr, (Ligo.nat_from_literal "0n")) checker.burrows) in
         assert_operation_equal
           ~expected:(
             LigoOp.Tezos.fa2_transfer_transaction
               [ Fa2Interface.(
                     { from_ = alice_addr;
                       txs = [
                         { to_ = burrow_address burrow;
                           token_id = Tok.tok_token_id;
                           amount = tok_to_denomination_nat amnt;
                         };
                       ];
                     }
                   )
               ]
               (Ligo.tez_from_literal "0mutez")
               (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.collateral_fa2))
           )
           ~real:op
       | _ -> failwith ("Expected [CreateBurrowContract (_, _, _, _); Transaction (FA2TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
    );

    ("entrypoint_deactivate_burrow - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow and deactivate it *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let (_, burrow_no), checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "100_000_000n")) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, checker = Checker.entrypoint_deactivate_burrow (checker, (burrow_no, alice_addr)) in
       let burrow = Option.get (Ligo.Big_map.find_opt (alice_addr, burrow_no) checker.burrows) in
       let expected_ops = [
         (LigoOp.Tezos.address_nat_transaction
            (alice_addr, (Ligo.nat_from_literal "100_000_000n"))
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow)))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_deposit_collateral - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let (_, burrow_no), checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "3_000_000n")) in
       (* Make a deposit *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let ops, checker = Checker.entrypoint_deposit_collateral (checker, (burrow_no, tok_of_denomination (Ligo.nat_from_literal "3_000_000n"))) in
       let burrow = Option.get (Ligo.Big_map.find_opt (alice_addr, burrow_no) checker.burrows) in
       let expected_ops = [
         (LigoOp.Tezos.fa2_transfer_transaction
            [ Fa2Interface.(
                  { from_ = alice_addr;
                    txs = [
                      { to_ = burrow_address burrow;
                        token_id = Tok.tok_token_id;
                        amount = Ligo.nat_from_literal "3_000_000n";
                      };
                    ];
                  }
                )
            ]
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.collateral_fa2))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_liquidation_auction_place_bid - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = checker_with_active_auction () in
       (* Lookup the current minimum bid *)
       let auction_details = Checker.view_current_liquidation_auction_details ((), checker) in
       (* Mint some kit to be able to bid *)
       let new_burrow_no = Ligo.nat_from_literal "100n" in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (new_burrow_no, None, tok_of_denomination (Ligo.nat_from_literal "1_000_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (new_burrow_no, auction_details.minimum_bid)) in

       (* Place a bid *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _checker = Checker.entrypoint_liquidation_auction_place_bid
           (checker,
            ((Option.get checker.liquidation_auctions.current_auction).contents, auction_details.minimum_bid))
       in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_mark_for_liquidation - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Use a checker state already containing some liquidatable burrows *)
       let liquidatable_burrow_ids, _, checker = checker_with_liquidatable_burrows () in
       let burrow_id = List.nth liquidatable_burrow_ids 0 in
       let sender = bob_addr in

       (* Mark one of the liquidatable burrows for liquidation *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in

       let burrow = Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows) in
       let expected_ops = [
         (LigoOp.Tezos.address_nat_transaction
            (sender, (Ligo.nat_from_literal "1_001_000n"))
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow)))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_cancel_liquidation_slice - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Use a checker state already containing some liquidatable burrows *)
       (* Note: using a non-closed burrow for this test so we don't have to also re-activate the burrow *)
       let _, slice_details, checker = checker_with_queued_liquidation_slices () in
       let ((burrow_owner, burrow_no), slice_ptr, _) = List.nth slice_details 0 in

       (* Deposit some extra collateral to one of the burrows with slices in the auction queue *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:burrow_owner ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_deposit_collateral (checker, (burrow_no, tok_of_denomination (Ligo.nat_from_literal "4_000_000n"))) in

       (* Now cancel one of the burrow's liquidation slices *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:burrow_owner ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_cancel_liquidation_slice (checker, slice_ptr) in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_liquidation_auction_claim_win - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let winning_bidder, checker = checker_with_completed_auction () in
       let auction_ptr = (Option.get checker.liquidation_auctions.completed_auctions).oldest in
       let sold_tok = (Option.get (Avl.avl_root_data checker.liquidation_auctions.avl_storage auction_ptr)).sold_tok in
       let slice_ptrs = avl_leaves_to_list checker.liquidation_auctions.avl_storage auction_ptr in

       (* Touch the remaining slices so the bid can be claimed. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_touch_liquidation_slices (checker, slice_ptrs) in

       (* Claim the winning bid *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:winning_bidder ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_liquidation_auction_claim_win (checker, auction_ptr) in
       let expected_ops = [
         (LigoOp.Tezos.fa2_transfer_transaction
            [ Fa2Interface.(
                  { from_ = !Ligo.Tezos.self_address;
                    txs = [
                      { to_ = winning_bidder;
                        token_id = Tok.tok_token_id;
                        amount = tok_to_denomination_nat sold_tok;
                      };
                    ];
                  }
                )
            ]
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.collateral_fa2))
         );

       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_mint_kit - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       (* Create a burrow and mint some kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", (kit_of_denomination (Ligo.nat_from_literal "10_000_000n")))) in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_set_burrow_delegate - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create the burrow with no delegate *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let (_, burrow_no), checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "3_000_000n")) in
       (* Then set the burrow's delegate *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, checker = Checker.entrypoint_set_burrow_delegate (checker, (burrow_no, Some charles_key_hash)) in
       let burrow = Option.get (Ligo.Big_map.find_opt (alice_addr, burrow_no) checker.burrows) in
       let expected_ops = [
         (LigoOp.Tezos.opt_key_hash_transaction
            (Some charles_key_hash)
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowSetDelegate" (burrow_address burrow)))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_receive_price - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:(checker.external_contracts.oracle) ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_receive_price (checker, Ligo.nat_from_literal "42n") in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_remove_liquidity - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       (* Create a burrow and mint some kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", (kit_of_denomination (Ligo.nat_from_literal "10_000_000n")))) in
       (* Add some liquidity to the contract *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_add_liquidity
           (checker,
            (* Note: all values here were arbitrarily chosen based on the amount of kit we minted above *)
            ( ctok_of_muctok (Ligo.nat_from_literal "5_000_000n")
            , kit_of_denomination (Ligo.nat_from_literal "5_000_000n")
            , lqt_of_denomination (Ligo.nat_from_literal "5_000_000n")
            , Ligo.timestamp_from_seconds_literal 999
            )
           ) in
       (* Now remove the liquidity *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_remove_liquidity
           (checker,
            (* Note: all values here were arbitrarily chosen based on the amount of kit we minted above *)
            ( lqt_of_denomination (Ligo.nat_from_literal "5_000_000n")
            , ctok_of_muctok (Ligo.nat_from_literal "5_000_000n")
            , kit_of_denomination (Ligo.nat_from_literal "5_000_000n")
            , Ligo.timestamp_from_seconds_literal 999
            )
           ) in

       let expected_ops = [
         (LigoOp.Tezos.fa12_transfer_transaction
            Fa12Interface.(
              {address_to=alice_addr;
               address_from=checker_address;
               value=(Ligo.nat_from_literal "5_000_000n")}
            )
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.cfmm_token_fa12))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_touch - emits expected operations when checker needs to be touched" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_touch (checker, ()) in

       let expected_ops = [
         (LigoOp.Tezos.nat_contract_transaction
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%receive_price" !Ligo.Tezos.self_address))
            (Ligo.tez_from_literal "0mutez")
            (Checker.get_oracle_entrypoint checker.external_contracts)
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_touch - emits expected operations when checker has already been touched" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_touch (checker, ()) in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_touch_liquidation_slices - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let _, checker = checker_with_completed_auction () in
       let auction_ptr = (Option.get checker.liquidation_auctions.completed_auctions).oldest in
       let slice_ptrs = avl_leaves_to_list checker.liquidation_auctions.avl_storage auction_ptr in
       let slices = List.map (fun ptr -> Avl.avl_read_leaf checker.liquidation_auctions.avl_storage ptr) slice_ptrs in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_touch_liquidation_slices (checker, slice_ptrs) in
       (* Note: opening LiquidationAuctionPrimitiveTypes locally here since we have overloaded
        * the "contents" record accessor in LiquidationAuctionTypes
       *)

       let expected_ops = let open LiquidationAuctionPrimitiveTypes in
         List.rev (List.map (
             fun slice ->
               let burrow = Option.get (Ligo.Big_map.find_opt slice.contents.burrow checker.burrows) in
               LigoOp.Tezos.address_nat_transaction
                 (checker_address, tok_to_denomination_nat slice.contents.tok)
                 (Ligo.tez_from_literal "0mutez")
                 (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow)))
           ) slices) in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("entrypoint_touch_burrow - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))) in
       (* Then touch it *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_touch_burrow (checker, (alice_addr, Ligo.nat_from_literal "0n")) in
       assert_operation_list_equal ~expected:[] ~real:ops
    );

    ("entrypoint_withdraw_collateral - emits expected operations" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let (_, burrow_no), checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "3_000_000n")) in
       (* Try to withdraw some tez from the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, checker = Checker.entrypoint_withdraw_collateral (checker, (Ligo.nat_from_literal "0n", tok_of_denomination (Ligo.nat_from_literal "1_000_000n"))) in
       let burrow = Option.get (Ligo.Big_map.find_opt (alice_addr, burrow_no) checker.burrows) in
       let expected_ops = [
         (LigoOp.Tezos.address_nat_transaction
            (alice_addr, (Ligo.nat_from_literal "1_000_000n"))
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowTransfer" (burrow_address burrow)))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("calculate_touch_reward - expected result for last_touched 2s ago" >::
     fun _ ->
       (* The division in this case should return a remainder < 1/2 *)
       Ligo.Tezos.reset ();
       let time_delta = 2 in
       (* remainder: 12000 / 36000 *)
       let expected_reward = Ligo.int_from_literal "3333" in
       let last_touched = Ligo.timestamp_from_seconds_literal 0 in
       Ligo.Tezos.new_transaction ~seconds_passed:time_delta ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let actual_reward = kit_to_denomination_int (Checker.calculate_touch_reward last_touched) in

       assert_int_equal ~expected:expected_reward ~real:actual_reward;
    );

    ("calculate_touch_reward - expected result for last_touched 3s ago" >::
     fun _ ->
       (* The division in this case should produce no remainder *)
       Ligo.Tezos.reset ();
       let time_delta = 3 in
       (* remainder: 0 *)
       let expected_reward = Ligo.int_from_literal "5000" in
       let last_touched = Ligo.timestamp_from_seconds_literal 0 in
       Ligo.Tezos.new_transaction ~seconds_passed:time_delta ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let actual_reward = kit_to_denomination_int (Checker.calculate_touch_reward last_touched) in

       assert_int_equal ~expected:expected_reward ~real:actual_reward;
    );

    ("calculate_touch_reward - expected result for last_touched 4s ago" >::
     fun _ ->
       (* The division in this case should return a remainder > 1/2 *)
       Ligo.Tezos.reset ();
       let time_delta = 4 in
       (* remainder: 24000 / 36000 *)
       let expected_reward = Ligo.int_from_literal "6666" in
       let last_touched = Ligo.timestamp_from_seconds_literal 0 in
       Ligo.Tezos.new_transaction ~seconds_passed:time_delta ~blocks_passed:2 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let actual_reward = kit_to_denomination_int (Checker.calculate_touch_reward last_touched) in

       assert_int_equal ~expected:expected_reward ~real:actual_reward;

    );

    ("burn_kit - owner can burn" >::
     fun _ ->
       Ligo.Tezos.reset ();

       let sender = alice_addr in

       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:Common.tez_zero;
       let _, checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "10_000_000n")) in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "4_285_714n"))
           ) in

       (* There should be no operations emitted. *)
       assert_operation_list_equal ~expected:[] ~real:ops;

       (* The owner should be able to burn it back. *)
       let kit_token = kit_of_denomination (Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender)) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", kit_token)) in

       ()
    );

    ("burn_kit - non-owner cannot burn" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = newly_created_burrow empty_checker "0n" (tok_of_denomination (Ligo.nat_from_literal "10_000_000n")) in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "4_285_714n"))
           ) in

       (* There should be no operations emitted. *)
       assert_operation_list_equal ~expected:[] ~real:ops;

       (* Have the wrong person try to burn it back; this should fail. *)
       assert_raises
         (Failure (Ligo.string_of_int error_NonExistentBurrow))
         (fun () ->
            let kit_token = kit_of_denomination (Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, bob_addr)) in
            Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
            Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", kit_token))
         );

       ()
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_respects_min_kit_expected"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctok_amount, min_kit_expected, deadline) ->

      let sender = alice_addr in
      let checker = empty_checker_with_cfmm cfmm in

      let senders_old_kit = Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender) in (* before *)

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, checker = Checker.entrypoint_buy_kit (checker, (ctok_amount, min_kit_expected, deadline)) in

      let senders_new_kit = Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender) in (* after *)

      begin match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:sender ~real:transfer.address_from;
            assert_address_equal ~expected:checker_address ~real:transfer.address_to;
            assert_nat_equal ~expected:(ctok_to_muctok_nat ctok_amount) ~real:transfer.value;
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      end;

      Ligo.geq_nat_nat
        senders_new_kit
        (Ligo.add_nat_nat senders_old_kit (kit_to_denomination_nat min_kit_expected))
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_preserves_kit"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctok_amount, min_kit_expected, deadline) ->

      let checker = empty_checker_with_cfmm cfmm in
      let sender = alice_addr in

      let checker_cfmm_old_kit = kit_to_denomination_nat checker.cfmm.kit in
      let senders_old_kit = Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender) in (* before *)

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, checker = Checker.entrypoint_buy_kit (checker, (ctok_amount, min_kit_expected, deadline)) in

      let checker_cfmm_new_kit = kit_to_denomination_nat checker.cfmm.kit in
      let senders_new_kit = Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender) in (* after *)

      begin match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:sender ~real:transfer.address_from;
            assert_address_equal ~expected:checker_address ~real:transfer.address_to;
            assert_nat_equal ~expected:(ctok_to_muctok_nat ctok_amount) ~real:transfer.value;
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      end;

      Ligo.eq_nat_nat
        (Ligo.add_nat_nat checker_cfmm_old_kit senders_old_kit)
        (Ligo.add_nat_nat checker_cfmm_new_kit senders_new_kit)
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_buy_kit_preserves_tez"
        ~count:property_test_count
        make_inputs_for_buy_kit_to_succeed
      @@ fun (cfmm, ctok_amount, min_kit_expected, deadline) ->
      let checker = empty_checker_with_cfmm cfmm in
      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
      let _, new_checker = Checker.entrypoint_buy_kit (checker, (ctok_amount, min_kit_expected, deadline)) in
      ctok_add checker.cfmm.ctok ctok_amount = new_checker.cfmm.ctok
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_sell_kit_respects_min_tez_expected"
        ~count:property_test_count
        make_inputs_for_sell_kit_to_succeed
      @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
      let sender = alice_addr in
      let checker =
        let checker = empty_checker_with_cfmm cfmm in
        { checker with
          parameters =
            { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_amount };
          fa2_state = ledger_issue_kit (checker.fa2_state, sender, kit_amount);
        } in
      Checker.assert_checker_invariants checker;

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, _ = Checker.entrypoint_sell_kit (checker, (kit_amount, min_ctok_expected, deadline)) in
      let bought_muctok = match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:checker_address ~real:transfer.address_from;
            assert_address_equal ~expected:sender ~real:transfer.address_to;
            transfer.value
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      in
      ctok_of_muctok bought_muctok >= min_ctok_expected
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_sell_kit_preserves_kit"
        ~count:property_test_count
        make_inputs_for_sell_kit_to_succeed
      @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
      let sender = alice_addr in
      let checker =
        let checker = empty_checker_with_cfmm cfmm in
        { checker with
          parameters =
            { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_amount };
          fa2_state = ledger_issue_kit (checker.fa2_state, sender, kit_amount);
        } in
      Checker.assert_checker_invariants checker;

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let _, new_checker = Checker.entrypoint_sell_kit (checker, (kit_amount, min_ctok_expected, deadline)) in
      kit_add checker.cfmm.kit kit_amount = new_checker.cfmm.kit
    );

    (
      Ligo.Tezos.reset();

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"test_sell_kit_preserves_tez"
        ~count:property_test_count
        make_inputs_for_sell_kit_to_succeed
      @@ fun (cfmm, kit_amount, min_ctok_expected, deadline) ->
      let sender = alice_addr in
      let checker =
        let checker = empty_checker_with_cfmm cfmm in
        { checker with
          parameters =
            { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_amount };
          fa2_state = ledger_issue_kit (checker.fa2_state, sender, kit_amount);
        } in
      Checker.assert_checker_invariants checker;

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, new_checker = Checker.entrypoint_sell_kit (checker, (kit_amount, min_ctok_expected, deadline)) in

      let bought_muctok = match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:checker_address ~real:transfer.address_from;
            assert_address_equal ~expected:sender ~real:transfer.address_to;
            transfer.value
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      in
      ctok_add new_checker.cfmm.ctok (ctok_of_muctok bought_muctok) = checker.cfmm.ctok
    );

    (
      let cfmm_kit = Ligo.nat_from_literal ("1_000n") in
      let cfmm_ctok = ctok_of_muctok (Ligo.nat_from_literal ("1_000n")) in
      (* The maximum amount of kit that you can buy with a finite amount of tez is
       * (1 - fee) * cfmm.kit - 1
      *)
      let max_buyable_kit = 997 in
      let arb_kit = QCheck.map (fun x -> kit_of_denomination (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(1 -- max_buyable_kit) in
      let arb_tez = TestArbitrary.arb_small_positive_tez in

      qcheck_to_ounit
      @@ QCheck.Test.make
        ~name:"buy_kit - returns geq min_kit_expected kit for transactions with sufficient tez"
        ~count:property_test_count
        (QCheck.pair arb_kit arb_tez)
      @@ fun (min_expected_kit, additional_tez) ->

      Ligo.Tezos.reset();
      let sender = alice_addr in

      (* Populate cfmm with initial liquidity *)
      let open Ratio in
      let checker =
        empty_checker_with_cfmm
          { empty_checker.cfmm with
            ctok = cfmm_ctok;
            kit = kit_of_denomination cfmm_kit;
          } in

      (* Calculate minimum tez to get the min_expected kit given the state of the cfmm defined above*)
      let ratio_minimum_tez = div_ratio
          (ratio_of_nat cfmm_kit)
          (
            sub_ratio
              (div_ratio (ratio_of_nat (Ligo.nat_from_literal "998n")) (ratio_of_nat (kit_to_denomination_nat min_expected_kit)))
              (ratio_of_nat (Ligo.nat_from_literal "1n"))
          ) in
      let minimum_tez = Ligo.mul_nat_tez (Ligo.abs (Common.cdiv_int_int ratio_minimum_tez.num ratio_minimum_tez.den)) (Ligo.tez_from_literal "1mutez") in
      (* Adjust transaction by a random amount of extra tez *)
      let tez_provided = Ligo.add_tez_tez minimum_tez additional_tez in

      let senders_old_kit = Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender) in (* before *)

      Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
      let ops, checker = Checker.entrypoint_buy_kit (checker, (ctok_from_tez tez_provided, min_expected_kit, Ligo.timestamp_from_seconds_literal 1)) in

      begin match ops with
        | [Transaction (FA12TransferTransactionValue transfer, _, _)] ->
          begin
            assert_address_equal ~expected:sender ~real:transfer.address_from;
            assert_address_equal ~expected:checker_address ~real:transfer.address_to;
            assert_nat_equal ~expected:(Ligo.abs (Common.tez_to_mutez tez_provided)) ~real:transfer.value;
          end
        | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops)
      end;

      let senders_new_kit = Fa2Ledger.get_fa2_ledger_value checker.fa2_state.ledger (Kit.kit_token_id, sender) in (* after *)

      Ligo.geq_nat_nat
        senders_new_kit
        (Ligo.add_nat_nat senders_old_kit (kit_to_denomination_nat min_expected_kit))
        (* FIXME: This test only rarely evaluates the 'eq' part of 'geq'. Reducing the range of possible `additional_tez` or increasing the
         * number of QCheck samples may improve this.
        *)
    );

    ("buy_kit - returns expected kit" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Populate the cfmm with some liquidity *)
       let checker =
         empty_checker_with_cfmm
           { empty_checker.cfmm with
             ctok = ctok_of_muctok (Ligo.nat_from_literal "2n");
             kit = kit_of_denomination (Ligo.nat_from_literal "2n");
           } in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, checker = Checker.entrypoint_buy_kit (checker, (ctok_of_muctok (Ligo.nat_from_literal "1_000_000n"), kit_of_denomination (Ligo.nat_from_literal "1n"), Ligo.timestamp_from_seconds_literal 1)) in
       let kit = get_balance_of checker alice_addr kit_token_id in

       let expected_ops = [
         (LigoOp.Tezos.fa12_transfer_transaction
            Fa12Interface.(
              {address_to=checker_address;
               address_from=alice_addr;
               value=(Ligo.nat_from_literal "1_000_000n")}
            )
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.cfmm_token_fa12))
         );
       ] in
       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:kit;
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("sell_kit - returns expected tez" >::
     fun _ ->
       Ligo.Tezos.reset ();

       let kit_to_sell = kit_of_denomination (Ligo.nat_from_literal "1_000_000n") in
       let min_ctok_expected = ctok_of_muctok (Ligo.nat_from_literal "1n") in

       let checker =
         let checker =
           empty_checker_with_cfmm
             { empty_checker.cfmm with
               ctok = ctok_of_muctok (Ligo.nat_from_literal "2n");
               kit = kit_of_denomination (Ligo.nat_from_literal "2n");
               lqt = lqt_of_denomination (Ligo.nat_from_literal "1n");
             } in
         { checker with
           parameters =
             { checker.parameters with circulating_kit = kit_add checker.parameters.circulating_kit kit_to_sell };
           fa2_state = ledger_issue_kit (checker.fa2_state, alice_addr, kit_to_sell);
         } in
       Checker.assert_checker_invariants checker;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, _ = Checker.entrypoint_sell_kit (checker, (kit_to_sell, min_ctok_expected, Ligo.timestamp_from_seconds_literal 1)) in

       let expected_ops = [
         (LigoOp.Tezos.fa12_transfer_transaction
            Fa12Interface.(
              {address_to=alice_addr;
               address_from=checker_address;
               value=(Ligo.nat_from_literal "1n")}
            )
            (Ligo.tez_from_literal "0mutez")
            (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.cfmm_token_fa12))
         );
       ] in
       assert_operation_list_equal ~expected:expected_ops ~real:ops
    );

    ("remove_liquidity - returns expected kit and tez" >::
     fun _ ->
       Ligo.Tezos.reset ();

       let min_kit_expected = kit_of_denomination (Ligo.nat_from_literal "1n") in
       let min_ctok_expected = ctok_of_muctok (Ligo.nat_from_literal "1n") in
       let my_liquidity_tokens = lqt_of_denomination (Ligo.nat_from_literal "1n") in
       let sender = alice_addr in

       (* Populate the cfmm with some liquidity (carefully crafted) *)
       let checker =
         { empty_checker with
           parameters = { empty_checker.parameters with circulating_kit = kit_of_denomination (Ligo.nat_from_literal "1n")};
           cfmm =
             { empty_checker.cfmm with
               ctok = ctok_of_muctok (Ligo.nat_from_literal "2n");
               kit = kit_of_denomination (Ligo.nat_from_literal "2n");
               lqt = lqt_of_denomination (Ligo.nat_from_literal "2n");
             };
           fa2_state =
             let fa2_state = initial_fa2_state in
             let fa2_state = ledger_issue_lqt (fa2_state, sender, my_liquidity_tokens) in
             let fa2_state = ledger_issue_kit (fa2_state, !Ligo.Tezos.self_address, kit_of_denomination (Ligo.nat_from_literal "1n")) in
             fa2_state;
         } in
       Checker.assert_checker_invariants checker;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let ops, checker = Checker.entrypoint_remove_liquidity (checker, (my_liquidity_tokens, min_ctok_expected, min_kit_expected, Ligo.timestamp_from_seconds_literal 1)) in
       let ctok = match ops with
         | [ Transaction (FA12TransferTransactionValue transfer, _, _); ] ->
           begin
             assert_address_equal ~expected:checker_address ~real:transfer.address_from;
             transfer.value
           end
         | _ -> failwith ("Expected [Transaction (FA12TransferTransactionValue _, _, _); Transaction (KitTransactionValue _, _, _)] but got " ^ show_operation_list ops)
       in
       let kit = get_balance_of checker sender kit_token_id in

       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:kit;
       assert_nat_equal ~expected:(Ligo.nat_from_literal "1n") ~real:ctok;
       ()
    );

    (* ************************************************************************* *)
    (**                               FA2                                        *)
    (* ************************************************************************* *)
    ("fa2 scenario" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in

       let initial_addr = Ligo.address_of_string "INIT_ADDR" in

       (* mint some kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:initial_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "100_000_000n"))) in
       let max_kit = Checker.view_burrow_max_mintable_kit ((initial_addr, Ligo.nat_from_literal "0n"), checker) in

       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:initial_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", max_kit)) in

       (* get some liquidity *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:initial_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker =
         Checker.entrypoint_add_liquidity
           ( checker,
             ( ctok_of_muctok (Ligo.nat_from_literal "5_000_000n")
             , kit_of_denomination (Ligo.nat_from_literal "5_000_000n")
             , lqt_of_denomination (Ligo.nat_from_literal "5n")
             , Ligo.timestamp_from_seconds_literal 999
             )
           ) in

       (* initialize alice, bob and leena accounts *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:initial_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.strict_entrypoint_transfer (checker, [
           { from_ = initial_addr;
             txs = [
               { to_ = alice_addr; token_id = kit_token_id; amount = Ligo.nat_from_literal "5n" };
               { to_ = bob_addr; token_id = lqt_token_id; amount = Ligo.nat_from_literal "5n" }
             ];
           }]) in

       let balance chk addr tok = Checker.view_get_balance ((addr, tok), chk) in

       (* you can see the initial balances here for reference *)
       assert_nat_equal ~real:(balance checker alice_addr kit_token_id) ~expected:(Ligo.nat_from_literal "5n");
       assert_nat_equal ~real:(balance checker alice_addr lqt_token_id) ~expected:(Ligo.nat_from_literal "0n");

       assert_nat_equal ~real:(balance checker bob_addr   kit_token_id) ~expected:(Ligo.nat_from_literal "0n");
       assert_nat_equal ~real:(balance checker bob_addr   lqt_token_id) ~expected:(Ligo.nat_from_literal "5n");

       assert_nat_equal ~real:(balance checker leena_addr kit_token_id) ~expected:(Ligo.nat_from_literal "0n");
       assert_nat_equal ~real:(balance checker leena_addr lqt_token_id) ~expected:(Ligo.nat_from_literal "0n");

       (* make leena an operator of bob for kit *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_update_operators (checker, [
           (Add_operator { owner = bob_addr; operator = leena_addr; token_id = kit_token_id })]) in

       assert_equal true (Checker.view_is_operator ((bob_addr, (leena_addr, kit_token_id)), checker));
       assert_equal false (Checker.view_is_operator ((bob_addr, (leena_addr, lqt_token_id)), checker));
       assert_equal false (Checker.view_is_operator ((leena_addr, (bob_addr, kit_token_id)), checker));

       (* alice can transfer some kit to bob *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.strict_entrypoint_transfer (checker, [
           { from_=alice_addr; txs=[{to_=bob_addr; token_id=kit_token_id;amount=Ligo.nat_from_literal "2n"}]}]) in

       assert_nat_equal ~real:(balance checker alice_addr kit_token_id) ~expected:(Ligo.nat_from_literal "3n");
       assert_nat_equal ~real:(balance checker bob_addr   kit_token_id) ~expected:(Ligo.nat_from_literal "2n");

       (* but she can not transfer more than she has *)
       assert_raises
         (Failure "FA2_INSUFFICIENT_BALANCE")
         (fun () -> Checker.strict_entrypoint_transfer (checker, [
              { from_=alice_addr; txs=[{to_=bob_addr; token_id=kit_token_id; amount=Ligo.nat_from_literal "10n"}]}]));

       (* and leena can send some of that kit back to alice *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.strict_entrypoint_transfer (checker, [
           { from_=bob_addr; txs=[{to_=alice_addr; token_id=kit_token_id; amount=Ligo.nat_from_literal "1n"}]}]) in

       assert_nat_equal ~real:(balance checker alice_addr kit_token_id) ~expected:(Ligo.nat_from_literal "4n");
       assert_nat_equal ~real:(balance checker bob_addr   kit_token_id) ~expected:(Ligo.nat_from_literal "1n");

       (* but leena can not even send a single kit from bob's account when he's not an operator anymore *)
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_update_operators (checker, [
           (Remove_operator { owner = bob_addr; operator = leena_addr; token_id = kit_token_id })]) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:leena_addr ~amount:(Ligo.tez_from_literal "0mutez");

       assert_raises
         (Failure "FA2_NOT_OPERATOR")
         (fun () -> Checker.strict_entrypoint_transfer (checker, [
              { from_=bob_addr; txs=[{to_=alice_addr; token_id=kit_token_id; amount=Ligo.nat_from_literal "1n"}]}]));
       ()
    );

    ("view_total_supply (FA2) - initial kit supply" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let total_kit_amount = Checker.view_total_supply (Kit.kit_token_id, empty_checker) in
       assert_nat_equal ~expected:(Ligo.nat_from_literal "0n") ~real:total_kit_amount;
       ()
    );

    ("view_total_supply (FA2) - initial lqt supply" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let total_lqt_amount = Checker.view_total_supply (Lqt.lqt_token_id, empty_checker) in
       assert_nat_equal ~expected:(Ligo.nat_from_literal "0n") ~real:total_lqt_amount;
       ()
    );

    ("view_total_supply (FA2) - undefined token id" >::
     fun _ ->
       assert_raises
         (Failure "FA2_TOKEN_UNDEFINED")
         (fun () -> Checker.view_total_supply (Ligo.nat_from_literal "3n", empty_checker))
    );

    ("view_all_tokens (FA2)" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let all_tokens = Checker.view_all_tokens ((), empty_checker) in
       assert_nat_list_equal
         ~expected:[ Kit.kit_token_id; Lqt.lqt_token_id ]
         ~real:all_tokens;
       ()
    );

    (* ************************************************************************* *)
    (**                      LiquidationAuctions                                 *)
    (* ************************************************************************* *)
    ("entrypoint_liquidation_auction_place_bid: should only allow the current auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = { empty_checker with last_price = Some (Ligo.nat_from_literal "1_000_000n") } in

       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_touch (checker, ()) in

       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "200_000_000n"))) in
       let max_kit = Checker.view_burrow_max_mintable_kit ((alice_addr, Ligo.nat_from_literal "0n"), checker) in

       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", max_kit)) in
       let checker = { checker with last_price = Some (Ligo.nat_from_literal "10_000_000n") } in
       let _, checker = Checker.entrypoint_touch (checker, ()) in

       Ligo.Tezos.new_transaction ~seconds_passed:1_000_000 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_touch (checker, ()) in

       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_touch_burrow (checker, (alice_addr, Ligo.nat_from_literal "0n")) in
       let _, checker = Checker.entrypoint_mark_for_liquidation (checker, (alice_addr, Ligo.nat_from_literal "0n")) in
       let _, checker = Checker.entrypoint_touch (checker, ()) in

       let res = Checker.view_current_liquidation_auction_details ((), checker) in
       let other_ptr = match res.auction_id with AVLPtr i -> Ptr.ptr_next i in

       assert_raises
         (Failure (Ligo.string_of_int error_InvalidLiquidationAuction))
         (fun () -> Checker.entrypoint_liquidation_auction_place_bid (checker, (AVLPtr other_ptr, res.minimum_bid)));
    );

    ("can complete a liquidation auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in

       (* mint some kit to convert to liquidity *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "200_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "10_000_000n"))) in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _lqt_minted_ret_kit_ops, checker =
         Checker.entrypoint_add_liquidity
           ( checker
           , ( ctok_of_muctok (Ligo.nat_from_literal "1_000_000n")
             , kit_one
             , lqt_of_denomination (Ligo.nat_from_literal "1n")
             , Ligo.timestamp_from_seconds_literal 1
             )
           ) in (* barely on time *)

       (* Activation/deactivation tests *)
       let () =
         (* Creation/deactivation does not incur any costs. *)
         let tez = tok_of_denomination (Ligo.nat_from_literal "12_345_678n") in (* NOTE: tez is a misnomer; it's tok really *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:Common.tez_zero;
         let (ops, checker0) = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tez)) in
         (* created burrow should be deposited (incl. the creation deposit) *)
         let burrow_addr =
           burrow_address
             (Option.get (Ligo.Big_map.find_opt (bob_addr, Ligo.nat_from_literal "0n") checker0.burrows)) in
         let () = match ops with
           | [ CreateBurrowContract (_, cb_delegate, cb_tez, cb_storage) ;
               (Transaction (FA2TransferTransactionValue _, _, _)) as op ;
             ] ->
             (* burrow creation values *)
             assert_key_hash_option_equal ~expected:None ~real:cb_delegate;
             assert_tez_equal ~expected:Common.tez_zero ~real:cb_tez;
             assert_equal BurrowTypes.({checker_address=checker_address; collateral_fa2=collateral_fa2_addr}) cb_storage;
             (* collateral initialization values *)
             assert_operation_equal
               ~expected:(
                 LigoOp.Tezos.fa2_transfer_transaction
                   [ Fa2Interface.(
                         { from_ = bob_addr;
                           txs = [
                             { to_ = burrow_addr;
                               token_id = Tok.tok_token_id;
                               amount = tok_to_denomination_nat tez;
                             };
                           ];
                         }
                       )
                   ]
                   (Ligo.tez_from_literal "0mutez")
                   (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.collateral_fa2))
               )
               ~real:op
           | _ -> assert_failure ("Expected [CreateBurrowContract (_, _, _, _); Transaction (FA2TransferTransactionValue _, _, _)] but got " ^ show_operation_list ops) in

         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let (ops, checker1) = Checker.entrypoint_deactivate_burrow (checker0, (Ligo.nat_from_literal "0n", alice_addr)) in
         assert_operation_list_equal
           ~expected:[
             LigoOp.Tezos.address_nat_transaction
               (alice_addr, tok_to_denomination_nat tez)
               (Ligo.tez_from_literal "0mutez")
               (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowTransfer" burrow_addr))
           ]
           ~real:ops;
         (* deactivation/activation = identity (if conditions are met ofc). *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:Common.tez_zero;
         let _ops, checker2 = Checker.entrypoint_activate_burrow (checker1, (Ligo.nat_from_literal "0n", tez)) in
         (* FIXME: cfmm contains a ratio, which cannot be compared for equality using (=). So, the next line can give false positives. *)
         assert_equal checker0 checker2;
         () in

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:Common.tez_zero;
       let (_, checker) = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "0n", None, tok_of_denomination (Ligo.nat_from_literal "10_000_000n"))) in
       let burrow_id = (bob_addr, Ligo.nat_from_literal "0n") in
       let burrow_addr =
         burrow_address
           (Option.get (Ligo.Big_map.find_opt (bob_addr, Ligo.nat_from_literal "0n") checker.burrows)) in

       (* Mint as much kit as possible *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) =
         Checker.entrypoint_mint_kit
           ( checker
           , (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "4_285_714n"))
           ) in

       let kit = get_balance_of checker bob_addr kit_token_id in
       assert_nat_equal ~expected:(Ligo.nat_from_literal "4_285_714n") ~real:kit;

       assert_bool
         "should not be overburrowed right after minting"
         (not
          @@ burrow_is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* Minting another kit should fail *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_MintKitFailure))
         (fun () ->
            Checker.entrypoint_mint_kit
              ( checker
              , (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "1n"))
              )
         );

       (* Over time the burrows with outstanding kit should be overburrowed
          	* (NOTE: even if the index stays where it was before, but that would
          	* take more time I guess). *)
       Ligo.Tezos.new_transaction ~seconds_passed:60 ~blocks_passed:1 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let _ops, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_001n") in

       let ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in
       assert_operation_list_equal ~expected:[] ~real:ops;

       assert_bool
         "if the index goes up, then burrows should become overburrowed"
         (burrow_is_overburrowed
            checker.parameters
            (Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows))
         );

       (* If enough time passes and the index remains up, then the burrow is even liquidatable. *)
       Ligo.Tezos.new_transaction ~seconds_passed:(211*60) ~blocks_passed:211 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let kit_before_reward = get_balance_of checker bob_addr kit_token_id in
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_200_000n") in
       let kit_after_reward = get_balance_of checker bob_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       let ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in
       assert_operation_list_equal ~expected:[] ~real:ops;

       assert_int_equal
         ~expected:(Ligo.int_from_literal "202_000_000") (* wow, high reward, many blocks have passed. *)
         ~real:touch_reward;

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in

       assert_operation_list_equal
         ~expected:[
           LigoOp.Tezos.address_nat_transaction
             (alice_addr, Ligo.nat_from_literal "1_009_000n")
             (Ligo.tez_from_literal "0mutez")
             (Option.get (LigoOp.Tezos.get_entrypoint_opt "%burrowTransfer" burrow_addr))
         ]
         ~real:ops;

       let slice =
         (Ligo.Big_map.find_opt burrow_id checker.liquidation_auctions.burrow_slices)
         |> Option.get
         |> fun i -> i.youngest_slice in

       (* We shouldn't be able to cancel the liquidation of this slice if the
        * prices don't change, even if it's not in an auction yet. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_UnwarrantedCancellation))
         (fun () -> Checker.entrypoint_cancel_liquidation_slice (checker, slice));

       (* Trying to cancel a liquidation using an invalid pointer should fail. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidLeafPtr))
         (fun () ->
            let undefined_slice = LiquidationAuctionPrimitiveTypes.LeafPtr (ptr_next checker.liquidation_auctions.avl_storage.last_ptr) in
            Checker.entrypoint_cancel_liquidation_slice (checker, undefined_slice)
         );

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       assert_raises
         (Failure (Ligo.string_of_int error_NoOpenAuction))
         (fun () -> Checker.view_current_liquidation_auction_details ((), checker));

       let kit_before_reward = get_balance_of checker bob_addr kit_token_id in
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_200_000n") in
       let kit_after_reward = get_balance_of checker bob_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       assert_bool "should start an auction"
         (Option.is_some checker.liquidation_auctions.current_auction);

       assert_int_equal
         ~expected:(Ligo.int_from_literal "500_000")
         ~real:touch_reward;

       Ligo.Tezos.new_transaction ~seconds_passed:(5*60) ~blocks_passed:5 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let kit_before_reward = get_balance_of checker alice_addr kit_token_id in
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_200_000n") in
       let kit_after_reward = get_balance_of checker alice_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in
       let min_bid = Checker.view_current_liquidation_auction_details ((), checker) in

       let auction_id =
         min_bid.auction_id in
       assert_kit_equal
         ~expected:(kit_of_denomination (Ligo.nat_from_literal "2_709_185n"))
         ~real:min_bid.minimum_bid;

       (* Bid the minimum first *)
       let (ops, checker) =
         Checker.entrypoint_liquidation_auction_place_bid (checker, (auction_id, min_bid.minimum_bid)) in
       assert_operation_list_equal ~expected:[] ~real:ops;

       (* Same person increases the bid *)
       let (ops, checker) =
         Checker.entrypoint_liquidation_auction_place_bid
           ( checker
           , (auction_id, kit_of_denomination (Ligo.nat_from_literal "4_200_000n"))
           ) in

       let auction_id =
         match checker.liquidation_auctions.current_auction with
         | None -> assert_failure "entrypoint_liquidation_auction_place_bid should have succeeded"
         | Some current_auction -> current_auction.contents in

       assert_operation_list_equal ~expected:[] ~real:ops;

       assert_int_equal
         ~expected:(Ligo.int_from_literal "500_000")
         ~real:touch_reward;

       Ligo.Tezos.new_transaction ~seconds_passed:(30*60) ~blocks_passed:30 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");

       let kit_before_reward = get_balance_of checker alice_addr kit_token_id in
       let ops, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_200_000n") in
       let kit_after_reward = get_balance_of checker alice_addr kit_token_id in

       let touch_reward = Ligo.sub_nat_nat kit_after_reward kit_before_reward in

       assert_bool "auction should be completed"
         (Option.is_none checker.liquidation_auctions.current_auction);

       assert_int_equal
         ~expected:(Ligo.int_from_literal "21_000_000")
         ~real:touch_reward;

       (* Check that all the requests for burrows to send tez come _before_ the
        * request to the oracle to update the index. *)
       begin match ops with
         | [
           Transaction (AddressNatTransactionValue _, _, _);  (* send tez requests *)
           Transaction (NatContractTransactionValue _, _, _); (* oracle call *)
         ] -> ()
         | _ -> assert_failure ("Unexpected operations/operation order: " ^ show_operation_list ops)
       end;

       (* We don't need to touch the slice on this test case since
        * Checker.entrypoint_touch_with_index already touches the oldest 5
        * slices. *)
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidLeafPtr))
         (fun () -> Checker.entrypoint_touch_liquidation_slices (checker, [slice]));

       assert_bool "burrow should have no liquidation slices"
         (Ligo.Big_map.find_opt burrow_id checker.liquidation_auctions.burrow_slices= None);

       let result = Option.get (Ligo.Big_map.find_opt burrow_id checker.burrows) in
       assert_tok_equal
         ~expected:tok_zero
         ~real:(burrow_collateral_at_auction result);

       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (ops, checker) = Checker.entrypoint_liquidation_auction_claim_win (checker, auction_id) in

       assert_operation_list_equal
         ~expected:[
           (LigoOp.Tezos.fa2_transfer_transaction
              [ Fa2Interface.(
                    { from_ = checker_address;
                      txs = [
                        { to_ = alice_addr;
                          token_id = Tok.tok_token_id;
                          amount = Ligo.nat_from_literal "3_156_446n";
                        };
                      ];
                    }
                  )
              ]
              (Ligo.tez_from_literal "0mutez")
              (Option.get (LigoOp.Tezos.get_entrypoint_opt "%transfer" checker.external_contracts.collateral_fa2))
           );
         ]
         ~real:ops;
       (* This should fail; shouldn't be able to claim the win twice. *)
       assert_raises
         (Failure (Ligo.string_of_int error_InvalidAvlPtr))
         (fun () -> Checker.entrypoint_liquidation_auction_claim_win (checker, auction_id));

       ()
    );

    ("entrypoint_mark_for_liquidation - should not create empty slices" >::
     fun _ ->
       (* Setup. *)
       Ligo.Tezos.reset ();
       let sender = alice_addr in
       let checker = empty_checker in

       (* Create a burrow with a very little tez in it. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:Common.tez_zero;
       let (_, burrow_no) as burrow_id, checker = newly_created_burrow checker "0n" (tok_of_denomination (Ligo.nat_from_literal "2_001_001n")) in

       (* CALCULATIONS
          ~~~~~~~~~~~~
          Tez in the burrow is (1_001_001mutez + 1tez) so the reward is
          (1tez + 1_001mutez = 1_001_001). This means that
          - The slice we WOULD send to auctions is empty.
          - The burrow remains is empty so the next liquidation WOULD create another empty slice to auctions.
       *)

       (* Mint as much kit as possible. *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:sender ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) = Checker.entrypoint_mint_kit (checker, (burrow_no, kit_of_denomination (Ligo.nat_from_literal "476_667n"))) in

       (* Let some time pass. Over time the burrows with outstanding kit should
          	* become overburrowed, and eventually liquidatable. Note that this
          	* could be because of the index, but also it can happen because of the
          	* fees alone if the index remains the same. *)
       let blocks_passed = 211 in (* NOTE: I am a little surprised/worried about this being again 211... *)
       Ligo.Tezos.new_transaction ~seconds_passed:(60*blocks_passed) ~blocks_passed:blocks_passed ~sender:bob_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_105_283n") in (* sup *)
       let _ops, checker = Checker.entrypoint_touch_burrow (checker, burrow_id) in

       (* Ensure that the burrow is liquidatable. *)
       begin match Ligo.Big_map.find_opt burrow_id checker.burrows with
         | None -> assert_failure "bug"
         | Some burrow -> assert_bool "burrow needs to be liquidatable for the test to be potent." (Burrow.burrow_is_liquidatable checker.parameters burrow);
       end;

       (* Let's mark the burrow for liquidation now (first pass: leaves it empty but active). *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in
       Checker.assert_checker_invariants checker; (* Ensures no empty slices in the queue. *)

       (* Let's mark the burrow for liquidation now (second pass: deactivates it). *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let (_ops, checker) = Checker.entrypoint_mark_for_liquidation (checker, burrow_id) in
       Checker.assert_checker_invariants checker; (* Ensures no empty slices in the queue. *)

       ()
    );

    ("deposit_collateral - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to deposit some tez to the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ = Checker.entrypoint_deposit_collateral (checker, (Ligo.nat_from_literal "0n", amount)) in
       ()
    );

    ("entrypoint_withdraw_collateral - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = tok_add Constants.creation_deposit Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to withdraw some tez from the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ = Checker.entrypoint_withdraw_collateral (checker, (Ligo.nat_from_literal "0n", Constants.creation_deposit)) in
       ()
    );

    ("entrypoint_mint_kit - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       (* Create a burrow *)
       let amount = tok_add Constants.creation_deposit Constants.creation_deposit in
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to mint some kit out of the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "1n"))) in
       ()
    );

    ("entrypoint_burn_kit - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = tok_add Constants.creation_deposit Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Mint some kit out of the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "1n"))) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to burn some kit into the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_burn_kit (checker, (Ligo.nat_from_literal "0n", kit_of_denomination (Ligo.nat_from_literal "1n"))) in
       ()
    );

    ("entrypoint_activate_burrow - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Deactivate the burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ops, checker = Checker.entrypoint_deactivate_burrow (checker, (Ligo.nat_from_literal "0n", !Ligo.Tezos.sender)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to activate the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ = Checker.entrypoint_activate_burrow (checker, (Ligo.nat_from_literal "0n", amount)) in
       ()
    );

    ("entrypoint_deactivate_burrow - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to deactivate the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_deactivate_burrow (checker, (Ligo.nat_from_literal "0n", !Ligo.Tezos.sender)) in
       ()
    );

    ("entrypoint_mark_for_liquidation - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to mark the untouched burrow for liquidation *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       (* TODO: Would be nice to create the conditions for entrypoint_mark_for_liquidation
        * to really succeed instead of failing for another reason. *)
       assert_raises
         (Failure (Ligo.string_of_int error_NotLiquidationCandidate))
         (fun () -> Checker.entrypoint_mark_for_liquidation (checker, burrow_id));
    );

    (* TODO: Add test "entrypoint_cancel_liquidation_slice - fails on untouched burrows" *)

    ("entrypoint_set_burrow_delegate - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to set the delegate of the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.entrypoint_set_burrow_delegate (checker, (Ligo.nat_from_literal "0n", None)) in
       ()
    );

    ("cfmm views" >:::
     let
       with_cfmm_setup f =
       fun _ ->
         Ligo.Tezos.reset ();
         let checker = empty_checker in
         let burrow_id = Ligo.nat_from_literal "42n" in
         (* Create a burrow *)
         Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
         let _ops, checker = Checker.entrypoint_create_burrow (checker, (burrow_id, None, tok_of_denomination (Ligo.nat_from_literal "10_000_000n"))) in
         (* Mint some kit *)
         Ligo.Tezos.new_transaction ~seconds_passed:62 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let _ops, checker = Checker.entrypoint_mint_kit (checker, (burrow_id, kit_one)) in
         (* Add some liquidity *)
         Ligo.Tezos.new_transaction ~seconds_passed:121 ~blocks_passed:2 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let ctok_to_give = Ctok.ctok_of_muctok (Ligo.nat_from_literal "400_000n") in
         let kit_to_give = Kit.kit_of_denomination (Ligo.nat_from_literal "400_000n") in
         let min_lqt_to_mint = Lqt.lqt_of_denomination (Ligo.nat_from_literal "5n") in
         let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
         let _ops, checker = Checker.entrypoint_add_liquidity (checker, (ctok_to_give, kit_to_give, min_lqt_to_mint, deadline)) in

         Ligo.Tezos.new_transaction ~seconds_passed:59 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
         let _ = f checker in ()
     in
     [
       "view_buy_kit_min_kit_expected" >:: with_cfmm_setup
         (fun checker ->
            let ctok_to_sell = Ctok.ctok_of_muctok (Ligo.nat_from_literal "100_000n") in
            let min_kit_to_buy = Checker.view_buy_kit_min_kit_expected (ctok_to_sell, checker) in
            let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
            (* must succeed, otherwise view_buy_kit_min_kit_expected overapproximated *)
            Checker.entrypoint_buy_kit (checker, (ctok_to_sell, min_kit_to_buy, deadline)));

       "view_buy_kit_min_kit_expected - fail if no ctok is given" >:: with_cfmm_setup
         (fun checker ->
            assert_raises
              (Failure (Ligo.string_of_int error_BuyKitNoCtokGiven))
              (fun () -> Checker.view_buy_kit_min_kit_expected (Ctok.ctok_zero, checker))
         );

       "view_sell_kit_min_ctok_expected" >:: with_cfmm_setup
         (fun checker ->
            let kit_to_sell = Kit.kit_of_denomination (Ligo.nat_from_literal "100_000n") in
            let min_ctok_to_buy = Checker.view_sell_kit_min_ctok_expected (kit_to_sell, checker) in
            let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
            (* must succeed, otherwise view_sell_kit_min_ctok_expected overapproximated *)
            Checker.entrypoint_sell_kit (checker, (kit_to_sell, min_ctok_to_buy, deadline)));

       "view_sell_kit_min_ctok_expected - fail if no kit is given" >:: with_cfmm_setup
         (fun checker ->
            assert_raises
              (Failure (Ligo.string_of_int error_SellKitNoKitGiven))
              (fun () -> Checker.view_sell_kit_min_ctok_expected (Kit.kit_zero, checker))
         );

       "view_add_liquidity_max_kit_deposited / view_add_liquidity_min_lqt_minted" >:: with_cfmm_setup
         (fun checker ->
            let ctok_to_sell = Ctok.ctok_of_muctok (Ligo.nat_from_literal "100_000n") in
            let max_kit_to_sell = Checker.view_add_liquidity_max_kit_deposited (ctok_to_sell, checker) in
            let min_lqt_to_buy = Checker.view_add_liquidity_min_lqt_minted (ctok_to_sell, checker) in
            let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
            (* must succeed, otherwise
             * view_add_liquidity_max_kit_deposited underapproximated or
             * view_add_liquidity_min_lqt_minted overapproximated (or both of them did) *)
            Checker.entrypoint_add_liquidity (checker, (ctok_to_sell, max_kit_to_sell, min_lqt_to_buy, deadline)));

       "view_add_liquidity_max_kit_deposited - fail if no ctok is given" >:: with_cfmm_setup
         (fun checker ->
            assert_raises
              (Failure (Ligo.string_of_int error_AddLiquidityNoCtokGiven))
              (fun () -> Checker.view_add_liquidity_max_kit_deposited (Ctok.ctok_zero, checker))
         );

       "view_add_liquidity_min_lqt_minted - fail if no ctok is given" >:: with_cfmm_setup
         (fun checker ->
            assert_raises
              (Failure (Ligo.string_of_int error_AddLiquidityNoCtokGiven))
              (fun () -> Checker.view_add_liquidity_min_lqt_minted (Ctok.ctok_zero, checker))
         );

       "view_remove_liquidity_min_ctok_withdrawn / view_remove_liquidity_min_kit_withdrawn" >:: with_cfmm_setup
         (fun checker ->
            let lqt_to_sell = Lqt.lqt_of_denomination (Ligo.nat_from_literal "5n") in
            let min_ctok_to_buy = Checker.view_remove_liquidity_min_ctok_withdrawn (lqt_to_sell, checker) in
            let min_kit_to_buy = Checker.view_remove_liquidity_min_kit_withdrawn (lqt_to_sell, checker) in
            let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "20") in
            (* must succeed, otherwise
             * view_remove_liquidity_min_ctok_withdrawn overapproximated or
             * view_remove_liquidity_min_kit_withdrawn overapproximated (or both of them did) *)
            Checker.entrypoint_remove_liquidity (checker, (lqt_to_sell, min_ctok_to_buy, min_kit_to_buy, deadline)));

       "view_remove_liquidity_min_ctok_withdrawn - fail if no liquidity is given" >:: with_cfmm_setup
         (fun checker ->
            assert_raises
              (Failure (Ligo.string_of_int error_RemoveLiquidityNoLiquidityBurned))
              (fun () -> Checker.view_remove_liquidity_min_ctok_withdrawn (Lqt.lqt_zero, checker))
         );

       "view_remove_liquidity_min_ctok_withdrawn - too much lqt withdrawn (equal)" >:: with_cfmm_setup
         (fun checker ->
            let lqt_to_withdraw = checker.cfmm.lqt in
            assert_raises
              (Failure (Ligo.string_of_int error_RemoveLiquidityTooMuchLiquidityWithdrawn))
              (fun () -> Checker.view_remove_liquidity_min_ctok_withdrawn (lqt_to_withdraw, checker))
         );

       "view_remove_liquidity_min_ctok_withdrawn - too much lqt withdrawn (more than)" >:: with_cfmm_setup
         (fun checker ->
            let lqt_to_withdraw = Lqt.lqt_add checker.cfmm.lqt (Lqt.lqt_of_denomination (Ligo.nat_from_literal "1n")) in
            assert_raises
              (Failure (Ligo.string_of_int error_RemoveLiquidityTooMuchLiquidityWithdrawn))
              (fun () -> Checker.view_remove_liquidity_min_ctok_withdrawn (lqt_to_withdraw, checker))
         );

       "view_remove_liquidity_min_kit_withdrawn - fail if no liquidity is given" >:: with_cfmm_setup
         (fun checker ->
            assert_raises
              (Failure (Ligo.string_of_int error_RemoveLiquidityNoLiquidityBurned))
              (fun () -> Checker.view_remove_liquidity_min_kit_withdrawn (Lqt.lqt_zero, checker))
         );

       "view_remove_liquidity_min_kit_withdrawn - too much lqt withdrawn (equal)" >:: with_cfmm_setup
         (fun checker ->
            let lqt_to_withdraw = checker.cfmm.lqt in
            assert_raises
              (Failure (Ligo.string_of_int error_RemoveLiquidityTooMuchLiquidityWithdrawn))
              (fun () -> Checker.view_remove_liquidity_min_kit_withdrawn (lqt_to_withdraw, checker))
         );

       "view_remove_liquidity_min_kit_withdrawn - too much lqt withdrawn (more than)" >:: with_cfmm_setup
         (fun checker ->
            let lqt_to_withdraw = Lqt.lqt_add checker.cfmm.lqt (Lqt.lqt_of_denomination (Ligo.nat_from_literal "1n")) in
            assert_raises
              (Failure (Ligo.string_of_int error_RemoveLiquidityTooMuchLiquidityWithdrawn))
              (fun () -> Checker.view_remove_liquidity_min_kit_withdrawn (lqt_to_withdraw, checker))
         );
     ]
    );

    ("view_burrow_max_mintable_kit - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to view the max mintable kit from the untouched burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.view_burrow_max_mintable_kit (burrow_id, checker) in
       ()
    );

    ("view_is_burrow_overburrowed - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to view whether the untouched burrow is overburrowed *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.view_is_burrow_overburrowed (burrow_id, checker) in
       ()
    );

    ("view_is_burrow_liquidatable - does not fail on untouched burrows" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let amount = Constants.creation_deposit in
       (* Create a burrow *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:Common.tez_zero;
       let _ops, checker = Checker.entrypoint_create_burrow (empty_checker, (Ligo.nat_from_literal "0n", None, amount)) in
       let burrow_id = (!Ligo.Tezos.sender, Ligo.nat_from_literal "0n") in
       (* Touch checker *)
       Ligo.Tezos.new_transaction ~seconds_passed:1 ~blocks_passed:1 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.touch_with_index checker (Ligo.nat_from_literal "1_000_000n") in
       (* Try to view whether the untouched burrow is liquidatable *)
       Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:alice_addr ~amount:(Ligo.tez_from_literal "0mutez");
       let _ = Checker.view_is_burrow_liquidatable (burrow_id, checker) in
       ()
    );

    ("view_current_liquidation_auction_details - raises error when there is no current auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = empty_checker in
       assert_raises
         (Failure (Ligo.string_of_int error_NoOpenAuction))
         (fun _ -> Checker.view_current_liquidation_auction_details ((), checker))
    );

    ("view_current_liquidation_auction_details - expected value for descending auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = checker_with_active_auction () in
       let auction = Option.get checker.liquidation_auctions.current_auction in
       let auction_details = Checker.view_current_liquidation_auction_details ((), checker) in
       let expected_auction_details = {
         auction_id = auction.contents;
         collateral = tok_of_denomination (Ligo.nat_from_literal "23_669_648n");
         minimum_bid = liquidation_auction_current_auction_minimum_bid auction;
         current_bid = None;
         remaining_blocks = None;
         remaining_seconds = None;
       }
       in
       assert_view_current_liquidation_auction_details_result_equal ~expected:expected_auction_details ~real:auction_details
    );

    ("view_current_liquidation_auction_details - expected value for ascending auction" >::
     fun _ ->
       Ligo.Tezos.reset ();
       let checker = checker_with_active_auction () in
       let auction = Option.get checker.liquidation_auctions.current_auction in
       (* Place a bid to turn the descending auction into an ascending one *)
       let bidder = bob_addr in
       let bid_amnt = liquidation_auction_current_auction_minimum_bid auction in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bidder ~amount:Common.tez_zero;
       let _, checker = Checker.entrypoint_create_burrow (checker, (Ligo.nat_from_literal "1n", None, tok_of_denomination (Ligo.nat_from_literal "1_000_000_000n"))) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bidder ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_mint_kit (checker, (Ligo.nat_from_literal "1n", bid_amnt)) in
       Ligo.Tezos.new_transaction ~seconds_passed:10 ~blocks_passed:1 ~sender:bidder ~amount:(Ligo.tez_from_literal "0mutez");
       let _, checker = Checker.entrypoint_liquidation_auction_place_bid (checker, (auction.contents, bid_amnt)) in

       Ligo.Tezos.new_transaction ~seconds_passed:500 ~blocks_passed:22 ~sender:bidder ~amount:(Ligo.tez_from_literal "0mutez");
       let auction = Option.get checker.liquidation_auctions.current_auction in
       let auction_details = Checker.view_current_liquidation_auction_details ((), checker) in
       let expected_auction_details = {
         auction_id = auction.contents;
         collateral = tok_of_denomination (Ligo.nat_from_literal "23_669_648n");
         minimum_bid = liquidation_auction_current_auction_minimum_bid auction;
         current_bid = Some LiquidationAuctionPrimitiveTypes.({address=bidder; kit=bid_amnt;});
         remaining_blocks = Some (Ligo.int_from_literal "-2");
         remaining_seconds = Some (Ligo.int_from_literal "700");
       }
       in
       assert_view_current_liquidation_auction_details_result_equal ~expected:expected_auction_details ~real:auction_details
    );
  ]

let () =
  run_test_tt_main
    suite
