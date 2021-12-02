open Fa2Interface
open Fa2Ledger

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

type wctez_state =
  { fa2_state : fa2_state;
    ctez_fa12_address : Ligo.address;
    metadata: (string, Ligo.bytes) Ligo.big_map;
  }

type wctez_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Mint of Ligo.nat
  | Redeem of Ligo.nat

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

val ledger_issue_wctez_token : (fa2_state * Ligo.address * Ligo.nat) -> fa2_state
val ledger_withdraw_wctez_token : (fa2_state * Ligo.address * Ligo.nat) -> fa2_state

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

val fa2_get_balance : (fa2_state * Ligo.address * fa2_token_id) -> Ligo.nat
val fa2_run_balance_of : (fa2_state * fa2_balance_of_request list) -> fa2_balance_of_response list
val fa2_run_transfer : (wctez_state * fa2_transfer list) -> (wctez_state * LigoOp.operation list)
val fa2_run_update_operators : (fa2_state * fa2_update_operator list) -> fa2_state

val balance_of : wctez_state -> fa2_balance_of_param -> (LigoOp.operation list * wctez_state)
val transfer : wctez_state -> fa2_transfer list -> (LigoOp.operation list * wctez_state)
val update_operators : wctez_state -> fa2_update_operator list -> (LigoOp.operation list * wctez_state)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

val mint : wctez_state -> Ligo.nat -> (LigoOp.operation list * wctez_state)
val redeem : wctez_state -> Ligo.nat -> (LigoOp.operation list * wctez_state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

val main : (wctez_params * wctez_state) -> (LigoOp.operation list * wctez_state)

(*****************************************************************************)
(**                       {1 OFFLINE FA2 VIEWS}                              *)
(*****************************************************************************)

val view_get_balance : ((Ligo.address * fa2_token_id) * wctez_state) -> Ligo.nat
(* FIXME: We'll need to extend the state to get this one to work:
 * val view_total_supply : (fa2_token_id * checker) -> Ligo.nat
*)
val view_all_tokens : (unit * wctez_state) -> fa2_token_id list
val view_is_operator : ((Ligo.address * (Ligo.address * fa2_token_id)) * wctez_state) -> bool
