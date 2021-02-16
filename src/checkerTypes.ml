open Burrow
open UniswapTypes
open Parameters
open LiquidationAuctionTypes
open DelegationAuctionTypes

type burrow_id = Ligo.address

type checker =
  { burrows : (burrow_id, burrow) Ligo.big_map;
    uniswap : uniswap;
    parameters : parameters;
    liquidation_auctions : liquidation_auctions;
    delegation_auction : delegation_auction;
    delegate : Ligo.key_hash option;
  }

(** Make a fresh state. *)
let initial_checker =
  { burrows = (Ligo.Big_map.empty: (burrow_id, burrow) Ligo.big_map);
    uniswap = uniswap_make_initial;
    parameters = initial_parameters;
    liquidation_auctions = liquidation_auction_empty;
    delegation_auction = delegation_auction_empty;
    delegate = (None : Ligo.key_hash option);
  }

(*
(* FIXME: DUMMY, JUST SO THAT WE CAN CALL compile-storage. Until the upgrade to
 * edo is complete
 * (https://gitlab.com/ligolang/ligo/-/issues/1096#note_503486776), we have to
 * stick with this patch. *)
let main (op_and_state: unit * checker): LigoOp.operation list * checker =
  let _op, state = op_and_state in
  (([]: LigoOp.operation list), state)
*)
