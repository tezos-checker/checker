open Fa2Interface
open Fa2Ledger

val originate_vault : Ligo.address -> (LigoOp.operation * Ligo.address)

(*****************************************************************************)
(**                          {1 WRAPPER TYPES}                               *)
(*****************************************************************************)

(** Map from vault owner addresses to vault addresses. *)
type vault_map = (Ligo.address, Ligo.address) Ligo.big_map

type wtez_state =
  { fa2_state : fa2_state;
    vaults : vault_map;
    metadata: (string, Ligo.bytes) Ligo.big_map;
  }

type wtez_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Wrapper-specific entrypoints *)
  | Deposit of unit (* TODO: not nice, having a unit type. Perhaps pass the tez as a number too? *)
  | Withdraw of Ligo.tez
  | Set_delegate of (Ligo.key_hash option)
  (* Internal entrypoints *)
  | Call_vault_receive_tez of (Ligo.address * Ligo.tez)
  | Call_vault_send_tez_to_contract of (Ligo.address * Ligo.tez * Ligo.address)
  | Call_vault_send_tez_to_vault of (Ligo.address * Ligo.tez * Ligo.address)
  | Call_vault_set_delegate of (Ligo.address * Ligo.key_hash option)

type vault_found = VaultFound | VaultNotFound

val find_vault_address_append : vault_map -> Ligo.address -> LigoOp.operation list -> (vault_found * LigoOp.operation list * vault_map * Ligo.address)

(*****************************************************************************)
(**                             {1 LEDGER}                                   *)
(*****************************************************************************)

val ledger_issue_tez_token : (fa2_state * Ligo.address * Ligo.tez) -> fa2_state
val ledger_withdraw_tez_token : (fa2_state * Ligo.address * Ligo.tez) -> fa2_state

(*****************************************************************************)
(**                        {1 FA2 ENTRYPOINTS}                               *)
(*****************************************************************************)

val fa2_get_balance : (fa2_state * Ligo.address * fa2_token_id) -> Ligo.nat
val fa2_run_balance_of : (fa2_state * fa2_balance_of_request list) -> fa2_balance_of_response list
val reverse_op_list : LigoOp.operation list -> LigoOp.operation list
val fa2_run_transfer : (wtez_state * fa2_transfer list) -> (wtez_state * LigoOp.operation list)
val fa2_run_update_operators : (fa2_state * fa2_update_operator list) -> fa2_state

val balance_of : wtez_state -> fa2_balance_of_param -> (LigoOp.operation list * wtez_state)
val transfer : wtez_state -> fa2_transfer list -> (LigoOp.operation list * wtez_state)
val update_operators : wtez_state -> fa2_update_operator list -> (LigoOp.operation list * wtez_state)

(*****************************************************************************)
(**                      {1 WRAPPER ENTRYPOINTS}                             *)
(*****************************************************************************)

val deposit : wtez_state -> unit -> (LigoOp.operation list * wtez_state)
val withdraw : wtez_state -> Ligo.tez -> (LigoOp.operation list * wtez_state)
val set_delegate : wtez_state -> Ligo.key_hash option -> (LigoOp.operation list * wtez_state)

(*****************************************************************************)
(**                      {1 INTERNAL ENTRYPOINTS}                            *)
(*****************************************************************************)

val call_vault_receive_tez : wtez_state -> (Ligo.address * Ligo.tez) -> (LigoOp.operation list * wtez_state)
val call_vault_send_tez_to_contract : wtez_state -> (Ligo.address * Ligo.tez * Ligo.address) -> (LigoOp.operation list * wtez_state)
val call_vault_send_tez_to_vault : wtez_state -> (Ligo.address * Ligo.tez * Ligo.address) -> (LigoOp.operation list * wtez_state)
val call_vault_set_delegate : wtez_state -> (Ligo.address * Ligo.key_hash option) -> (LigoOp.operation list * wtez_state)

(*****************************************************************************)
(**                              {1 MAIN}                                    *)
(*****************************************************************************)

val main : (wtez_params * wtez_state) -> (LigoOp.operation list * wtez_state)

(*****************************************************************************)
(**                       {1 OFFLINE FA2 VIEWS}                              *)
(*****************************************************************************)

val view_get_balance : ((Ligo.address * fa2_token_id) * wtez_state) -> Ligo.nat
(* FIXME: We'll need to extend the state to get this one to work:
 * val view_total_supply : (fa2_token_id * checker) -> Ligo.nat
*)
val view_all_tokens : (unit * wtez_state) -> fa2_token_id list
val view_is_operator : ((Ligo.address * (Ligo.address * fa2_token_id)) * wtez_state) -> bool
