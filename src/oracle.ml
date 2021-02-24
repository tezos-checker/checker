(* Medianizer *)

(*
open Common
*)
open Error

(* Map of oracle addresses and their entrypoints. *)
type oracles = (Ligo.address, string) Ligo.map

(* TODO: Figure out the real values for the following. *)
let oracle1 : Ligo.address = (Ligo.address_from_literal "oracle1TODO" : Ligo.address)
let oracle2 : Ligo.address = (Ligo.address_from_literal "oracle2TODO" : Ligo.address)
let oracle3 : Ligo.address = (Ligo.address_from_literal "oracle3TODO" : Ligo.address)

let initial_oracles : oracles =
  Ligo.Map.literal [
    (oracle1, "%getPrice1TODO");
    (oracle2, "%getPrice2TODO");
    (oracle3, "%getPrice3TODO");
  ]

(* Last observed prices from oracles. *)
type price_map = (Ligo.address, Ligo.nat) Ligo.map

(* ENTRYPOINT. Emits no operations but changes the state. *)
let receive_price (oracles: oracles) (price_map: price_map) (price: Ligo.nat): price_map =
  if Ligo.Map.mem !Ligo.Tezos.sender oracles
  then Ligo.Map.update !Ligo.Tezos.sender (Some price) price_map
  else (Ligo.failwith error_UnauthorisedCaller : price_map)

(* TO BE CALLED BY TOUCH IN A NON-BLOCKING WAY. Only emits operations but does
 * not change the state. Note that the order of the operations is the reverse
 * of what one would expect, but the order here is irrelevant and it costs gas
 * to reverse it. *)
let ask_oracle_values (oracles: oracles) : LigoOp.operation list =
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
    ([]: LigoOp.operation list)

(* FIXME: This only works for the three oracle addresses we have above. At the
 * end we should either make it all extensible, or totally hardwired. *)


(*
type oracle_param =
  | GetMedian of Ligo.nat LigoOp.contract
  | ReceiveFirstPrice of Ligo.nat
  | ReceiveSecondPrice of Ligo.nat
  | ReceiveThirdPrice of Ligo.nat

let oracle_main (op, state: oracle_param * oracle_storage): LigoOp.operation list * oracle_storage =
  match op with
  | GetMedian cb ->
    get_median state cb
  | ReceiveFirstPrice price1 ->
    receive_first_price state price1
  | ReceiveSecondPrice price2 ->
    receive_second_price state price2
  | ReceiveThirdPrice price3 ->
    receive_third_price state price3
*)
