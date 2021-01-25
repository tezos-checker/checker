open Ptr

(* todo: please find a better name for this. *)
type right_collection =
  { deposit_tez: bool;
    withdraw_tez: bool;
    mint_kit: bool;
    burn_kit: bool;
    set_delegate: bool;
    cancel_liquidation: bool;
  }
[@@deriving show]

(** A right can be an admin right (which implies all right), or a user right,
  * which can include depositing/withdrawing tez, minting/burning kit, setting
  * the delegate, and/or canceling liquidations. *)
type right =
  | Admin
  | User of right_collection
[@@deriving show]

type permission_content = right * ptr * Ligo.nat
[@@deriving show]

(** A permission is a ticket containing a right. *)
type permission = permission_content Ligo.ticket
[@@deriving show]

let does_right_allow_tez_deposits (right: right) : bool =
  match right with
  | Admin -> true
  | User r -> r.deposit_tez

let does_right_allow_tez_withdrawals (right: right) : bool =
  match right with
  | Admin -> true
  | User r -> r.withdraw_tez

let does_right_allow_kit_minting (right: right) : bool =
  match right with
  | Admin -> true
  | User r -> r.mint_kit

let does_right_allow_kit_burning (right: right) : bool =
  match right with
  | Admin -> true
  | User r -> r.burn_kit

let does_right_allow_setting_delegate (right: right) : bool =
  match right with
  | Admin -> true
  | User r -> r.set_delegate

let does_right_allow_cancelling_liquidations (right: right) : bool =
  match right with
  | Admin -> true
  | User r -> r.cancel_liquidation

let is_admin_right (right: right) : bool =
  match right with
  | Admin -> true
  | User _ -> false
