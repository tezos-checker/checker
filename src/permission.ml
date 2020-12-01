
(** A right can be an admin right (which implies all rights), or a user right,
  * which can include depositing/withdrawing tez, minting/burning kit, and
  * setting the delegate. *)
type rights =
  | Admin
  | User of
      { deposit_tez: bool;
        withdraw_tez: bool;
        mint_kit: bool;
        burn_kit: bool;
        set_delegate: bool;
      }
[@@deriving show]

(** A permission is a ticket containing a right. *)
type t = (rights * Ptr.t * int) Ticket.t [@@deriving show]

let does_right_allow_tez_deposits (rights: rights) : bool =
  match rights with
  | Admin -> true
  | User r -> r.deposit_tez

let does_right_allow_tez_withdrawals (rights: rights) : bool =
  match rights with
  | Admin -> true
  | User r -> r.withdraw_tez

let does_right_allow_kit_minting (rights: rights) : bool =
  match rights with
  | Admin -> true
  | User r -> r.mint_kit

let does_right_allow_kit_burning (rights: rights) : bool =
  match rights with
  | Admin -> true
  | User r -> r.burn_kit

let does_right_allow_setting_delegate (rights: rights) : bool =
  match rights with
  | Admin -> true
  | User r -> r.set_delegate

let is_admin_right (rights: rights) : bool =
  match rights with
  | Admin -> true
  | User _ -> false

