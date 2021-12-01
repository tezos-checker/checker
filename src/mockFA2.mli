open Fa2Interface
open Fa2Ledger

type mock_fa2_params =
  (* FA2 entrypoints *)
  | Balance_of of fa2_balance_of_param
  | Transfer of fa2_transfer list
  | Update_operators of fa2_update_operator list
  (* Contract-specific entrypoints *)
  | Mint of Ligo.nat
  | Redeem of Ligo.nat

type mock_fa2_state =
  { fa2_state : fa2_state;
    metadata: (string, Ligo.bytes) Ligo.big_map;
  }

val fa2_get_balance : (fa2_state * Ligo.address * fa2_token_id) -> Ligo.nat
val fa2_run_balance_of : (fa2_state * fa2_balance_of_request list) -> fa2_balance_of_response list
val fa2_run_transfer : (fa2_state * fa2_transfer list) -> (fa2_state * LigoOp.operation list)
val fa2_run_update_operators : (fa2_state * fa2_update_operator list) -> fa2_state

val balance_of : mock_fa2_state -> fa2_balance_of_param -> (LigoOp.operation list * mock_fa2_state)
val transfer : mock_fa2_state -> fa2_transfer list -> (LigoOp.operation list * mock_fa2_state)
val update_operators : mock_fa2_state -> fa2_update_operator list -> (LigoOp.operation list * mock_fa2_state)

val mint : mock_fa2_state -> Ligo.nat -> (LigoOp.operation list * mock_fa2_state)
val redeem : mock_fa2_state -> Ligo.nat -> (LigoOp.operation list * mock_fa2_state)

val main : (mock_fa2_params * mock_fa2_state) -> (LigoOp.operation list * mock_fa2_state)
