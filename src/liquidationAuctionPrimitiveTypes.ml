open Ptr
open Kit

type avl_ptr = AVLPtr of ptr
[@@deriving show]

type leaf_ptr = LeafPtr of ptr
[@@deriving show]

type liquidation_slice = {
  burrow: Ligo.address;
  tez: Ligo.tez;
  min_kit_for_unwarranted: kit;
  older: leaf_ptr option;
  younger: leaf_ptr option;
}
[@@deriving show]

type liquidation_auction_id = avl_ptr
[@@deriving show]

type bid = { address: Ligo.address; kit: kit }
[@@deriving show]

let bid_eq (b1: bid) (b2: bid) =
  let {address=a1; kit=k1} = b1 in
  let {address=a2; kit=k2} = b2 in
  a1 = a2 && k1 = k2

type auction_outcome = {
  sold_tez: Ligo.tez;
  winning_bid: bid;
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
