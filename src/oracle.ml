open Common
open Error

(* Map of oracle addresses and their entrypoints. *)
type oracles = (Ligo.address, string) Ligo.map

let oracle1 : Ligo.address = (Ligo.address_from_literal "KT1NNfziS5orym8pLvp2qsTjbq2ai9H8sDSr" : Ligo.address) (* FIXME: Use real address *)
let oracle2 : Ligo.address = (Ligo.address_from_literal "KT1Jr5t9UvGiqkvvsuUbPJHaYx24NzdUwNW9" : Ligo.address) (* FIXME: Use real address *)
let oracle3 : Ligo.address = (Ligo.address_from_literal "KT1AdbYiPYb5hDuEuVrfxmFehtnBCXv4Np7r" : Ligo.address) (* FIXME: Use real address *)

let initial_oracles : oracles =
  Ligo.Map.literal [
    (oracle1, "%getPrice1TODO"); (* FIXME: Use real entrypoint *)
    (oracle2, "%getPrice2TODO"); (* FIXME: Use real entrypoint *)
    (oracle3, "%getPrice3TODO"); (* FIXME: Use real entrypoint *)
  ]

(* Last observed prices from oracles. *)
type price_map = (Ligo.address, Ligo.nat) Ligo.map

(* ENTRYPOINT. Emits no operations but changes the state. *)
let receive_price (* (oracles: oracles) *) (price_map: price_map) (price: Ligo.nat): price_map =
  if Ligo.Map.mem !Ligo.Tezos.sender initial_oracles (* oracles *)
  then Ligo.Map.update !Ligo.Tezos.sender (Some price) price_map
  else (Ligo.failwith error_UnauthorisedCaller : price_map)

(* TO BE CALLED BY TOUCH IN A NON-BLOCKING WAY. Only emits operations but does
 * not change the state. Note that the order of the operations is the reverse
 * of what one would expect, but the order here is irrelevant and it costs gas
 * to reverse it. *)
(* FIXME: Adds the operations to the front. The order of operations has been
 * totally random until now in general actually; we have to discuss this. *)
let[@inline] ask_oracle_values (oracles: oracles) (starting: LigoOp.operation list) : LigoOp.operation list =
  (* NOTE: EXPECTATION: type params = ... | ReceivePrice of Ligo.nat *)
  let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receivePrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : Ligo.nat LigoOp.contract) in
  Ligo.Map.fold
    (fun (ops, (addr, ep) : LigoOp.operation list * (Ligo.address * string)) ->
       let oracle = match (LigoOp.Tezos.get_entrypoint_opt ep addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
         | Some c -> c
         | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint : (Ligo.nat LigoOp.contract) LigoOp.contract) in
       let op = LigoOp.Tezos.nat_contract_transaction cb (Ligo.tez_from_literal "0mutez") oracle in
       (op :: ops)
    )
    oracles
    starting

let[@inline] median (x: Ligo.nat) (y: Ligo.nat) (z: Ligo.nat) : Ligo.nat =
  (* FIXME: probably we should hardwire the decision tree here. Not bothering
   * to do that just yet. *)
  let small, medium = min_max_nat x y in
  let medium, _large = min_max_nat medium z in
  let _small, medium = min_max_nat small medium in
  medium

(* FIXME: This only works for the three oracle addresses we have above. At the
 * end we should either make it all extensible, or totally hardwired.
 * FIXME: This is also an all-or-nothing approach (either we have all values
 * and thus we have a result, or some are missing and we have no result). Might
 * be what we want, just leaving this here for us to make sure. *)
let compute_current_median (* (oracles: oracles) *) (price_map: price_map) : Ligo.nat option =
  match Ligo.Map.find_opt oracle1 price_map with
  | None -> (None : Ligo.nat option)
  | Some p1 ->
    (match Ligo.Map.find_opt oracle2 price_map with
     | None -> (None : Ligo.nat option)
     | Some p2 ->
       (match Ligo.Map.find_opt oracle3 price_map with
        | None -> (None : Ligo.nat option)
        | Some p3 -> Some (median p1 p2 p3)
       )
    )
