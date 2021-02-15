open Kit
open Ratio

type uniswap =
  { tez: Ligo.tez;
    kit: kit;
    lqt: Ligo.nat;
    (* George: I don't expect this to get really big in size cause it's
     * always derived by dividing uniswap.tez / uniswap.kit (i.e. even if they
     * are relatively prime, we are OK). *)
    kit_in_tez_in_prev_block: ratio [@printer pp_ratio];
    last_level: Ligo.nat;
  }
[@@deriving show]

(** The initial state of the uniswap contract. We always start with 1mukit,
  * 1mutez, and 1lqt token (effectively setting the starting price to 1
  * tez/kit). The price will eventually reach the value it should, but this
  * saves us from having the first/non-first liquidity provider separation, and
  * all division-by-zero checks. *)
(* NOTE: this is not a function but a value (perhaps "make_" is not appropriate here). *)
let uniswap_make_initial : uniswap =
  { tez = Ligo.tez_from_literal "1mutez";
    kit = kit_of_mukit (Ligo.nat_from_literal "1n");
    lqt = Ligo.nat_from_literal "1n";
    kit_in_tez_in_prev_block = one_ratio; (* Same as tez/kit now. *)
    last_level = Ligo.nat_from_literal "0n"; (* FIXME: must be !Ligo.tezos_level *)
  }
