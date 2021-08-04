open Ctez
open Kit
open Lqt
open Common

[@@@coverage off]

type cfmm =
  { ctez: ctez;
    kit: kit;
    lqt: lqt;
    kit_in_ctez_in_prev_block: ratio [@printer pp_ratio];
    last_level: Ligo.nat;
  }
[@@deriving show]

[@@@coverage on]

(** The initial state of the cfmm contract. We always start with 1mukit,
    1muctez, and 1lqt token (effectively setting the starting price to 1
    ctez/kit). The price will eventually reach the value it should, but this
    saves us from having the first/non-first liquidity provider separation, and
    all division-by-zero checks. *)
let initial_cfmm () : cfmm =
  { ctez = ctez_of_muctez (Ligo.nat_from_literal "1n");
    kit = kit_of_denomination (Ligo.nat_from_literal "1n");
    lqt = lqt_of_denomination (Ligo.nat_from_literal "1n");
    kit_in_ctez_in_prev_block = one_ratio; (* Same as ctez/kit now. *)
    last_level = !Ligo.Tezos.level;
  }
