open TokenTypes

(** A permission is a ticket containing a right. *)
type permission = permission_content Ligo.ticket
[@@deriving show]

let does_right_allow_tez_deposits (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.deposit_tez

let does_right_allow_tez_withdrawals (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.withdraw_tez

let does_right_allow_kit_minting (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.mint_kit

let does_right_allow_kit_burning (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.burn_kit

let does_right_allow_setting_delegate (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.set_delegate

let does_right_allow_cancelling_liquidations (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.cancel_liquidation

let is_admin_right (right: rights) : bool =
  match right with
  | Admin -> true
  | User _ -> false
