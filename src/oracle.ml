(* Medianizer *)

open Common
open Error

(* For each oracle we need to know (a) its address, and (b) its entrypoint (of
 * type Ligo.nat LigoOp.contract) for giving the current price. *)
type oracle_data =
  { entrypoint : string;
    price : Ligo.nat option;
  }

type oracle_map = (Ligo.address, oracle_data) Ligo.map


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

(* ENTRYPOINT. Emits no operations *)
let receive_price (oracle_map: oracle_map) (price: Ligo.nat): oracle_map =
  let entry_opt = Ligo.Map.find_opt !Ligo.Tezos.sender oracle_map in
  match entry_opt with
  | None -> (Ligo.failwith error_UnauthorisedCaller : oracle_map)
  | Some odata ->
    Ligo.Map.update
      !Ligo.Tezos.sender
      (Some {entrypoint = odata.entrypoint; price = Some price;})
      oracle_map

(*
(* TO BE CALLED BY TOUCH IN A NON-BLOCKING WAY. *)
let ask_oracle_values (oracle_map: oracle_map) : LigoOp.operation list =
(*
  (failwith "WHATEVER" : LigoOp.operation list)
*)
  Ligo.Map.fold
    (fun ops addr_and_data ->
      (failwith "WHATEVER" : LigoOp.operation list)
    )
    oracle_map
    [] (* LigoOp.operation list *)
*)

(*
val fold : ('accumulator -> ('key * 'value) -> 'accumulator) -> ('key, 'value) map -> 'accumulator -> 'accumulator

let fold_op (m : register) : int =
  let folded = fun (i,j : int * (address * move)) -> i + j.1.1
  in Map.fold folded m 5
*)



(*
  let oracle3 = match (LigoOp.Tezos.get_entrypoint_opt "%getPrice3" oracle3_addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
    | Some c -> c
    | None -> (failwith "GetEntrypointOptFailure (%getPrice3)" : (Ligo.nat LigoOp.contract) LigoOp.contract) in
  let cb3 = match (LigoOp.Tezos.get_entrypoint_opt "%receiveThirdPrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (failwith "GetEntrypointOptFailure (%receiveThirdPrice)" : Ligo.nat LigoOp.contract) in
  let op = LigoOp.Tezos.nat_contract_transaction cb3 (Ligo.tez_from_literal "0mutez") oracle3 in
  let storage = {storage with p2 = price2} in
  ([op], storage)
*)


let oracle1_addr : Ligo.address = (Ligo.address_from_literal "oracle1TODO" : Ligo.address)
let oracle2_addr : Ligo.address = (Ligo.address_from_literal "oracle2TODO" : Ligo.address)
let oracle3_addr : Ligo.address = (Ligo.address_from_literal "oracle3TODO" : Ligo.address)
let oracle1_entrypoint : string = "%getPrice1TODO"
let oracle2_entrypoint : string = "%getPrice2TODO"
let oracle3_entrypoint : string = "%getPrice3TODO"










type oracle_storage =
  { p1: Ligo.nat;
    p2: Ligo.nat;
    p3: Ligo.nat;
    cb: Ligo.nat LigoOp.contract;
  }

let median (x: Ligo.nat) (y: Ligo.nat) (z: Ligo.nat) : Ligo.nat =
  let small, medium = min_max_nat x y in
  let medium, _large = min_max_nat medium z in
  let _small, medium = min_max_nat small medium in
  medium

let get_median (storage: oracle_storage) (callback: Ligo.nat LigoOp.contract) : LigoOp.operation list * oracle_storage =
  let oracle1 = match (LigoOp.Tezos.get_entrypoint_opt "%getPrice1" oracle1_addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
    | Some c -> c
    | None -> (failwith "GetEntrypointOptFailure (%getPrice1)" : (Ligo.nat LigoOp.contract) LigoOp.contract) in
  let cb1 = match (LigoOp.Tezos.get_entrypoint_opt "%receiveFirstPrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (failwith "GetEntrypointOptFailure (%receiveFirstPrice)" : Ligo.nat LigoOp.contract) in
  let op = LigoOp.Tezos.nat_contract_transaction cb1 (Ligo.tez_from_literal "0mutez") oracle1 in
  let storage = {storage with cb = callback} in
  ([op], storage)

let receive_first_price (storage: oracle_storage) (price1: Ligo.nat): LigoOp.operation list * oracle_storage =
  let oracle2 = match (LigoOp.Tezos.get_entrypoint_opt "%getPrice2" oracle2_addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
    | Some c -> c
    | None -> (failwith "GetEntrypointOptFailure (%getPrice2)" : (Ligo.nat LigoOp.contract) LigoOp.contract) in
  let cb2 = match (LigoOp.Tezos.get_entrypoint_opt "%receiveSecondPrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (failwith "GetEntrypointOptFailure (%receiveSecondPrice)" : Ligo.nat LigoOp.contract) in
  let op = LigoOp.Tezos.nat_contract_transaction cb2 (Ligo.tez_from_literal "0mutez") oracle2 in
  let storage = {storage with p1 = price1} in
  ([op], storage)

let receive_second_price (storage: oracle_storage) (price2: Ligo.nat): LigoOp.operation list * oracle_storage =
  let oracle3 = match (LigoOp.Tezos.get_entrypoint_opt "%getPrice3" oracle3_addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
    | Some c -> c
    | None -> (failwith "GetEntrypointOptFailure (%getPrice3)" : (Ligo.nat LigoOp.contract) LigoOp.contract) in
  let cb3 = match (LigoOp.Tezos.get_entrypoint_opt "%receiveThirdPrice" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (failwith "GetEntrypointOptFailure (%receiveThirdPrice)" : Ligo.nat LigoOp.contract) in
  let op = LigoOp.Tezos.nat_contract_transaction cb3 (Ligo.tez_from_literal "0mutez") oracle3 in
  let storage = {storage with p2 = price2} in
  ([op], storage)

let receive_third_price (storage: oracle_storage) (price3: Ligo.nat): LigoOp.operation list * oracle_storage =
  let storage = {storage with p3 = price3} in
  let median_price = median storage.p1 storage.p2 storage.p3 in
  let op = LigoOp.Tezos.nat_transaction median_price (Ligo.tez_from_literal "0mutez") storage.cb in
  ([op], storage)

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
