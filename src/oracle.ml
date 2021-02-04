(* Medianizer *)

open Common

(* TODO:
 * - Figure out the addresses of the oracles we're gonna use
 * - Figure out the entrypoints of the oracles we're gonna use
 * - NOTE: Tezos.self_address here is NOT checker's address. Perhaps we should make that a ref too.
 * - Would be nice to enforce a very specific call order (O1, O2, O3, exactly once each, all-or-nothing).
*)

type storage =
  { oracle1_addr: Ligo.address;
    oracle2_addr: Ligo.address;
    oracle3_addr: Ligo.address;
    feed1: Ligo.nat option; (* Some during calculation, None all other times *)
    feed2: Ligo.nat option; (* Some during calculation, None all other times *)
    feed3: Ligo.nat option; (* Some during calculation, None all other times *)
  }

let median (x: Ligo.nat) (y: Ligo.nat) (z: Ligo.nat) : Ligo.nat =
  let small, medium = min_max_nat x y in
  let medium, _large = min_max_nat medium z in
  let _small, medium = min_max_nat small medium in
  medium

let requestFirstValue (s: storage) =
  let oracle1 = match (LigoOp.Tezos.get_entrypoint_opt "%TODOgetPrice1" s.oracle1_addr : (Ligo.nat LigoOp.contract) LigoOp.contract option) with
    | Some c -> c
    | None -> (failwith "GetEntrypointOptFailure (%TODOgetPrice1)" : (Ligo.nat LigoOp.contract) LigoOp.contract) in
  let cb1 = match (LigoOp.Tezos.get_entrypoint_opt "%TODOreceivePrice1" Ligo.Tezos.self_address : (Ligo.nat LigoOp.contract) option) with
    | Some cb -> cb
    | None -> (failwith "GetEntrypointOptFailure (%TODOreceivePrice1)" : Ligo.nat LigoOp.contract) in
  let op = LigoOp.Tezos.nat_contract_transaction cb1 (Ligo.tez_from_literal "0mutez") oracle1 in
  ([op], s)


