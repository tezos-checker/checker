(* Medianizer *)

(*
open Common
*)
open Error

(* For each oracle we need to know (a) its address, and (b) its entrypoint (of
 * type Ligo.nat LigoOp.contract) for giving the current price. *)
type oracle_data =
  { entrypoint : string;
    price : Ligo.nat option;
  }

type oracle_map = (Ligo.address, oracle_data) Ligo.map

(* NOTE: Shall we put the entrypoints in a separate map? They are not supposed
 * to be updated as often as the prices so the current setup feels rather
 * inefficient. *)

(* TODO: Figure out the real values for the following. *)
let initial_oracles : oracle_map =
  Ligo.Map.literal [
    ((Ligo.address_from_literal "oracle1TODO" : Ligo.address),
     {entrypoint = "%getPrice1TODO"; price = (None : Ligo.nat option)}
    );
    ((Ligo.address_from_literal "oracle2TODO" : Ligo.address),
     {entrypoint = "%getPrice2TODO"; price = (None : Ligo.nat option)}
    );
    ((Ligo.address_from_literal "oracle3TODO" : Ligo.address),
     {entrypoint = "%getPrice3TODO"; price = (None : Ligo.nat option)}
    );
  ]

(* ENTRYPOINT. Emits no operations but changes the state. *)
let receive_price (oracle_map: oracle_map) (price: Ligo.nat): oracle_map =
  let entry_opt = Ligo.Map.find_opt !Ligo.Tezos.sender oracle_map in
  match entry_opt with
  | None -> (Ligo.failwith error_UnauthorisedCaller : oracle_map)
  | Some odata ->
    Ligo.Map.update
      !Ligo.Tezos.sender
      (Some {entrypoint = odata.entrypoint; price = Some price;})
      oracle_map

(* TO BE CALLED BY TOUCH IN A NON-BLOCKING WAY. Only emits operations but does
 * not change the state. Note that the order of the operations is the reverse
 * of what one would expect, but the order here is irrelevant and it costs gas
 * to reverse it. *)
let ask_oracle_values (oracle_map: oracle_map) : LigoOp.operation list =
  (* NOTE: EXPECTATION: type params = ... | ReceivePrice of Ligo.nat *)
  let cb = match (LigoOp.Tezos.get_entrypoint_opt "%receivePrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (Ligo.failwith error_GetEntrypointOptFailureReceivePrice : Ligo.nat LigoOp.contract) in
  Ligo.Map.fold
    (fun (ops, addr_and_data : LigoOp.operation list * (Ligo.address * oracle_data)) ->
       let addr, data = addr_and_data in
       let oracle = match (LigoOp.Tezos.get_entrypoint_opt data.entrypoint addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
         | Some c -> c
         | None -> (Ligo.failwith error_GetEntrypointOptFailureOracleEntrypoint : (Ligo.nat LigoOp.contract) LigoOp.contract) in
       let op = LigoOp.Tezos.nat_contract_transaction cb (Ligo.tez_from_literal "0mutez") oracle in
       (op :: ops)
    )
    oracle_map
    ([]: LigoOp.operation list)

(*
val fold : ('accumulator * ('key * 'value) -> 'accumulator) -> ('key, 'value) map -> 'accumulator -> 'accumulator

let fold_op (m : register) : int =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
*)

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
