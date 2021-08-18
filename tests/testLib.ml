open LiquidationAuctionPrimitiveTypes

let alice_addr = Ligo.address_from_literal "alice_addr"
let bob_addr = Ligo.address_from_literal "bob_addr"
let leena_addr = Ligo.address_from_literal "leena_addr"
let charles_key_hash = Ligo.key_hash_from_literal "charles_key_hash"

let ctez_addr = Ligo.address_of_string "ctez_addr"
let oracle_addr = Ligo.address_of_string "oracle_addr"

let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t
let assert_stdlib_int_equal ~expected ~real = OUnit2.assert_equal ~printer:string_of_int expected real
let assert_string_equal ~expected ~real = OUnit2.assert_equal ~printer:(fun x -> x) expected real

let assert_tez_equal ~expected ~real = OUnit2.assert_equal ~printer:Ligo.string_of_tez expected real
let assert_nat_equal ~expected ~real = OUnit2.assert_equal ~printer:Ligo.string_of_nat expected real
let assert_int_equal ~expected ~real = OUnit2.assert_equal ~printer:Ligo.string_of_int expected real
let assert_kit_equal ~expected ~real = OUnit2.assert_equal ~printer:Kit.show_kit expected real
let assert_lqt_equal ~expected ~real = OUnit2.assert_equal ~printer:Lqt.show_lqt expected real
let assert_tok_equal ~expected ~real = OUnit2.assert_equal ~printer:Tok.show_tok expected real
let assert_ratio_equal ~expected ~real = OUnit2.assert_equal ~printer:Common.show_ratio ~cmp:Ratio.eq_ratio_ratio expected real
type key_hash_option = Ligo.key_hash option [@@deriving show]
let assert_key_hash_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_key_hash_option expected real
let assert_address_equal ~expected ~real = OUnit2.assert_equal ~printer:Ligo.string_of_address expected real
let assert_fixedpoint_equal ~expected ~real = OUnit2.assert_equal ~printer:FixedPoint.show_fixedpoint_raw expected real
let assert_bid_equal ~expected ~real = OUnit2.assert_equal ~printer:LiquidationAuctionPrimitiveTypes.show_bid expected real
type bid_option = bid option [@@deriving show]
let assert_bid_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_bid_option expected real
let assert_liquidation_result_equal ~expected ~real = OUnit2.assert_equal ~printer:Burrow.show_liquidation_result expected real
let assert_avl_ptr_equal ~expected ~real = OUnit2.assert_equal ~printer:LiquidationAuctionPrimitiveTypes.show_avl_ptr expected real
let assert_ctez_equal ~expected ~real = OUnit2.assert_equal ~printer:Ctez.show_ctez expected real
let assert_parameters_equal ~expected ~real = OUnit2.assert_equal ~printer:Parameters.show_parameters expected real (* FIXME: contains a ratio *)
let assert_liquidation_slice_contents_equal ~expected ~real = OUnit2.assert_equal ~printer:LiquidationAuctionPrimitiveTypes.show_liquidation_slice_contents expected real
let assert_view_current_liquidation_auction_details_result_equal ~expected ~real = OUnit2.assert_equal ~printer:CheckerTypes.show_view_current_liquidation_auction_details_result expected real

type liquidation_auction_id_option = liquidation_auction_id option [@@deriving show]
let assert_liquidation_auction_id_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_liquidation_auction_id_option expected real

type kit_option = Kit.kit option [@@deriving show]
let assert_kit_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_kit_option expected real

type tez_option = Ligo.tez option [@@deriving show]
let assert_tez_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_tez_option expected real

type tok_option = Tok.tok option [@@deriving show]
let assert_tok_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_tok_option expected real

let assert_burrow_equal ~expected ~real = OUnit2.assert_equal ~printer:Burrow.show_burrow expected real
type slice_content_list = LiquidationAuctionPrimitiveTypes.liquidation_slice_contents list [@@deriving show]
let assert_slice_content_list_equal ~expected ~real = OUnit2.assert_equal ~printer:show_slice_content_list expected real

type liquidation_slice_list = LiquidationAuctionPrimitiveTypes.liquidation_slice list [@@deriving show]
let assert_liquidation_slice_list_equal ~expected ~real = OUnit2.assert_equal ~printer:show_liquidation_slice_list expected real

type operation_list = LigoOp.operation list [@@deriving show]
let assert_operation_list_equal ~expected ~real = OUnit2.assert_equal ~printer:show_operation_list expected real

type slice_option = LiquidationAuctionPrimitiveTypes.liquidation_slice option [@@deriving show]
let assert_slice_option_equal ~expected ~real = OUnit2.assert_equal ~printer:show_slice_option expected real

type nat_list = Ligo.nat list [@@deriving show]
let assert_nat_list_equal ~expected ~real = OUnit2.assert_equal ~printer:show_nat_list expected real

let eq_cfmm (u1: CfmmTypes.cfmm) (u2: CfmmTypes.cfmm) : bool =
  Ctez.ctez_compare u1.ctez u2.ctez = 0
  && Kit.kit_compare u1.kit u2.kit = 0
  && Lqt.lqt_compare u1.lqt u2.lqt = 0
  && Ratio.eq_ratio_ratio u1.kit_in_ctez_in_prev_block u2.kit_in_ctez_in_prev_block
  && Ligo.eq_nat_nat u1.last_level u2.last_level

let assert_cfmm_equal ~expected ~real = OUnit2.assert_equal ~printer:CfmmTypes.show_cfmm ~cmp:eq_cfmm expected real

let with_sealed_wrapper f =
  fun _ ->

  let checker_deployer = leena_addr in
  Ligo.Tezos.reset ();
  Ligo.Tezos.new_transaction ~seconds_passed:0 ~blocks_passed:0 ~sender:checker_deployer ~amount:(Ligo.tez_from_literal "0mutez");

  let wrapper = CheckerMain.initial_wrapper checker_deployer in (* unsealed *)
  let op = CheckerMain.SealContract (oracle_addr, ctez_addr) in
  let _ops, wrapper = CheckerMain.main (op, wrapper) in (* sealed *)
  f wrapper

let cfmm_make_for_test ~ctez ~kit ~lqt ~kit_in_ctez_in_prev_block ~last_level =
  { CfmmTypes.ctez = ctez;
    CfmmTypes.kit = kit;
    CfmmTypes.lqt = lqt;
    CfmmTypes.kit_in_ctez_in_prev_block = kit_in_ctez_in_prev_block;
    CfmmTypes.last_level = last_level;
  }

(* Create an arbitrary state for the cfmm contract (NB: some values are fixed). *)
let arbitrary_non_empty_cfmm (kit_in_ctez_in_prev_block: Common.ratio) (last_level: Ligo.nat) =
  QCheck.map
    (fun (ctez, kit, lqt) ->
       (ctez, kit, lqt, cfmm_make_for_test ~ctez ~kit ~lqt ~kit_in_ctez_in_prev_block ~last_level)
    )
    (QCheck.triple TestArbitrary.arb_positive_ctez TestArbitrary.arb_positive_kit TestArbitrary.arb_lqt)

(* amount >= cfmm_tez * (1 - fee) / fee *)
(* 1 (kit) <= min_kit_expected < FLOOR{amount * (cfmm_kit / (cfmm_tez + amount)) * FACTOR} *)
(* NB: some values are fixed *)
let make_inputs_for_buy_kit_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _kit being ignored. *)
    (fun (ctez, _kit, _lqt, cfmm) ->
       let amount =
         let { Common.num = x_num; Common.den = x_den; } =
           Ratio.div_ratio (Ratio.mul_ratio (Ctez.ratio_of_ctez ctez) (Ratio.sub_ratio Common.one_ratio Constants.cfmm_fee)) Constants.cfmm_fee in
         Ctez.ctez_of_fraction_ceil x_num x_den in
       let min_kit_expected = Kit.kit_of_denomination (Ligo.nat_from_literal "1n") in (* absolute minimum *)
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (cfmm, amount, min_kit_expected, deadline)
    )
    (arbitrary_non_empty_cfmm Common.one_ratio !Ligo.Tezos.level)

(* kit >= cfmm_kit * (1 - fee) / fee *)
(* 1mutez <= min_ctez_expected < FLOOR{kit * (cfmm_tez / (cfmm_kit + kit)) * FACTOR} *)
(* NB: some values are fixed *)
let make_inputs_for_sell_kit_to_succeed =
  QCheck.map
    (* NOTE: this could still give us tough numbers I think. Due to _tez being ignored. *)
    (fun (_tez, kit, _lqt, cfmm) ->
       let token =
         let { Common.num = x_num; Common.den = x_den; } =
           Ratio.div_ratio (Ratio.mul_ratio (Kit.kit_to_ratio kit) (Ratio.sub_ratio Common.one_ratio Constants.cfmm_fee)) Constants.cfmm_fee in
         Kit.kit_of_fraction_ceil x_num x_den
       in
       let min_ctez_expected = Ctez.ctez_of_muctez (Ligo.nat_from_literal "1n") in (* absolute minimum *)
       let deadline = Ligo.add_timestamp_int !Ligo.Tezos.now (Ligo.int_from_literal "1") in (* always one second later *)
       (cfmm, token, min_ctez_expected, deadline)
    )
    (arbitrary_non_empty_cfmm Common.one_ratio !Ligo.Tezos.level)

let debug_print_all_kit_in_sealed_state msg wrapper =
  let open CheckerTypes in
  match wrapper.deployment_state with
  | Unsealed _ ->
    print_string "\nUnsealed state; no kit to show\n"
  | Sealed state ->
    print_string ("\n===== " ^ msg ^ " =====");
    print_string ("\ncirculating = " ^ Kit.show_kit state.parameters.circulating_kit);
    print_string ("\noutstanding = " ^ Kit.show_kit state.parameters.outstanding_kit);
    print_string ("\ncfmm.kit    = " ^ Kit.show_kit state.cfmm.kit);
    print_string "\nkit_on_ledger :\n";
    List.iter
      (fun (addr, amnt) -> print_string ("  " ^ Ligo.string_of_address addr ^ " : " ^ Ligo.string_of_nat amnt ^ "\n"))
      (Fa2Interface.get_kit_credits_from_fa2_state state.fa2_state)

(* Extracts the pointers to all of a given AVL tree's leaves *)
let avl_leaves_to_list (mem: Mem.mem) (AVLPtr ptr) : leaf_ptr list =
  let rec go ptr: leaf_ptr list =
    match Mem.mem_get mem ptr with
    | Root (None, _) -> []
    | Root (Some ptr, _) -> go ptr
    | Leaf _ ->
      [LeafPtr ptr]
    | Branch branch ->
      List.append (go branch.left) (go branch.right) in
  go ptr
