open TokenTypes

let[@inline] does_right_allow_tez_deposits (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.deposit_tez

let[@inline] does_right_allow_tez_withdrawals (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.withdraw_tez

let[@inline] does_right_allow_kit_minting (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.mint_kit

let[@inline] does_right_allow_kit_burning (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.burn_kit

let[@inline] does_right_allow_setting_delegate (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.set_delegate

let[@inline] does_right_allow_cancelling_liquidations (right: rights) : bool =
  match right with
  | Admin -> true
  | User r -> r.cancel_liquidation

let[@inline] is_admin_right (right: rights) : bool =
  match right with
  | Admin -> true
  | User _ -> false
