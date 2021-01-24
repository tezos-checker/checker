open Ptr
open Kit

type avl_ptr = AVLPtr of ptr
[@@deriving show]

type leaf_ptr = LeafPtr of ptr
[@@deriving show]

type liquidation_slice = {
  burrow: ptr;
  tez: Ligo.tez;
  (* BEGIN_OCAML *) min_kit_for_unwarranted: kit; (* END_OCAML *)
  older: leaf_ptr option;
  younger: leaf_ptr option;
}
[@@deriving show]

(* BEGIN_OCAML *) (*FIXME after moving kit *)
type bid = { address: Ligo.address; kit: kit }
[@@deriving show]
(* END_OCAML *)

type auction_outcome = {
  sold_tez: Ligo.tez;
  (* BEGIN_OCAML *) winning_bid: bid; (* END_OCAML *) (* FIXME after moving kit *)
  younger_auction: avl_ptr option;
  older_auction: avl_ptr option;
}
[@@deriving show]

type leaf = {
  value: liquidation_slice;
  parent: ptr;
}
[@@deriving show]

(* TODO Instead of storing left_height and right_height, we could just
 * store the sum type LeftHeavy | Balanced | RightHeavy. However, I think
 * we might want to leave some trees unbalanced, and I think this approach
 * might work better in that case. *)
type branch = {
  left: ptr;
  left_height: Ligo.int;
  left_tez: Ligo.tez;
  right_tez: Ligo.tez;
  right_height: Ligo.int;
  right: ptr;
  parent: ptr;
}
[@@deriving show]

type node =
  | Leaf of leaf
  | Branch of branch
  | Root of (ptr option * auction_outcome option)
[@@deriving show]
