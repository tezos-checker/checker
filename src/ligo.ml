(* This file mimics the various Ligo libraries, and will not be
   included in the Ligo output.

   https://ligolang.org/docs/reference/current-reference/
*)

(* big_map's have two type parameters, but the ocaml's map type only has one (just for the value,
   the key is a parameter to the Map.Make module). Below is the shortest way I was able to think of
   to have an associative array with two type parameters. We abuse the polymorphic 'Hashtbl.hash :: 'a -> int'
   and '=' functions to implement something similar to a hash table, where we use an immutable int map as
   the underlying storage, and the collisions are handled by storing a list.
   Please do refactor if you find a nice solution.
*)
module IntMap = Map.Make(Int)
type ('key, 'value) big_map = ('key * 'value) list IntMap.t

module Big_map = struct
  let empty = IntMap.empty

  let find_opt (k: 'key) (m: ('key, 'value) big_map) : 'value option =
    let hash = Hashtbl.seeded_hash 42 k in
    Option.bind
      (IntMap.find_opt hash m)
      (List.find_map (fun (k', v') -> if k' = k then Some v' else None))

  let update (k: 'key) (v: 'value option) (m: ('key, 'value) big_map) : ('key, 'value) big_map =
    let hash = Hashtbl.seeded_hash 42 k in
    IntMap.update
      hash
      (fun slot -> match slot with
         | None  -> Option.map (fun v -> [(k, v)]) v
         | Some slot ->
           let rec go xs =
             match xs with
             | [] -> (match v with | Some v -> [(k, v)] | None -> [])
             | ((k', v')::rest) ->
               if k' = k
               then match v with
                 | Some v -> (k, v) :: rest
                 | None -> rest
               else (k', v') :: go rest in
           match go slot with
           | [] -> None
           | xs -> Some xs
      )
      m

  let bindings i =
    IntMap.bindings i |> List.concat_map snd

  let mem (k: 'key) (m: ('key, 'value) big_map) = Option.is_some (find_opt k m)
end

(* UTILITY FUNCTIONS *)

let parse_int_with_suffix (expected_suffix: string) (s: string) : Z.t =
  let total_len = String.length s in
  let suffix_len = String.length expected_suffix in
  let prefix_len = total_len - suffix_len in

  let prefix = String.sub s 0 prefix_len in

  let suffix = String.sub s prefix_len suffix_len in

  try
    if not (String.equal suffix expected_suffix) then
      raise (Invalid_argument ("Expected suffix: " ^ expected_suffix ^ ", real suffix: " ^ suffix))
    else
      Z.of_string prefix
  with exc -> raise (Invalid_argument ("parse_int_with_suffix: bad inputs (suffix = " ^ expected_suffix ^ ", input = " ^ s ^ ") " ^ Printexc.to_string exc))

(* key_hash *)

type key_hash = string
let pp_key_hash = Format.pp_print_string
let key_hash_from_literal s = s

(* address *)

type address = string

let string_of_address s = s
let pp_address = Format.pp_print_string

let address_from_literal s = s

(* int *)

type int = Z.t

let int_from_literal s =
  try parse_int_with_suffix "" s
  with exc -> failwith ("Ligo.int_from_literal: " ^ Printexc.to_string exc)

let int_from_int64 = Z.of_int64

let add_int_int = Z.add

let sub_int_int = Z.sub

let mul_int_int = Z.mul

let eq_int_int = Z.equal

let lt_int_int = Z.lt

let gt_int_int = Z.gt

let leq_int_int = Z.leq

let geq_int_int = Z.geq

let div_int_int = Z.div

let mod_int_int = Z.rem

let ediv_int_int n d =
  try Some (Z.ediv_rem n d)
  with Division_by_zero -> None

let of_string_base_int = Z.of_string_base

(* nat *)

type nat = Z.t

let string_of_nat = Z.to_string
let pp_nat fmt z = Format.pp_print_string fmt (string_of_nat z)

let add_nat_nat = Z.add

let sub_nat_nat = Z.sub

let mul_nat_nat = Z.mul

let div_nat_nat = Z.div

let eq_nat_nat = Z.equal

let lt_nat_nat = Z.lt

let gt_nat_nat = Z.gt

let leq_nat_nat = Z.leq

let geq_nat_nat = Z.geq

let int x = x

let abs = Z.abs

let is_nat x = if Z.lt x Z.zero then None else Some x

let nat_from_literal s =
  try
    let n = parse_int_with_suffix "n" s in
    if Z.lt n Z.zero then
      failwith "Ligo.nat_from_literal: negative"
    else n
  with exc ->
    failwith ("Ligo.nat_from_literal: " ^ Printexc.to_string exc)

let ediv_nat_nat n d =
  try Some (Z.ediv_rem n d)
  with Division_by_zero -> None

(* timestamp *)

type timestamp = Z.t

let add_timestamp_int = Z.add

let sub_timestamp_timestamp = Z.sub

let timestamp_from_seconds_literal s =
  if s < 0 then
    failwith "Ligo.timestamp_from_seconds_literal: negative"
  else
    Z.of_int s

(* tez *)

type tez = Z.t

let tez_from_literal s =
  try
    let n = parse_int_with_suffix "mutez" s in
    if Z.lt n Z.zero then
      failwith "Ligo.tez_from_literal: negative"
    else n
  with exc ->
    failwith ("Ligo.tez_from_literal: " ^ Printexc.to_string exc)

let add_tez_tez = Z.add

let sub_tez_tez x y =
  if Z.lt x y then
    failwith "Ligo.sub_tez_tez: negative"
  else
    Z.sub x y

let mul_nat_tez = Z.mul

let mul_tez_nat = Z.mul

let div_tez_tez x y =
  try Z.div x y
  with Division_by_zero -> failwith "Ligo.div_tez_tez: zero denominator"

let ediv_tez_nat n d =
  try Some (Z.ediv_rem n d)
  with Division_by_zero -> None

let eq_tez_tez = Z.equal

let lt_tez_tez = Z.lt

let leq_tez_tez = Z.leq

let geq_tez_tez = Z.geq

(* tickets *)

type 'a ticket =
  { issuer : address;
    content : 'a;
    amount : nat;
  }
[@@deriving show]

module Tezos = struct
  let now = ref (timestamp_from_seconds_literal 0)
  let level = ref (nat_from_literal "0n")
  let self_address = "self_address"
  let sender = ref "sender"
  let amount = ref (tez_from_literal "0mutez")

  let create_ticket content amount =
    { issuer = self_address;
      content = content;
      amount = amount;
    }

  let read_ticket ticket = ((ticket.issuer, (ticket.content, ticket.amount)), ticket)

  let split_ticket ticket (left, right) =
    if (add_nat_nat left right) <> ticket.amount
    then None
    else
      (* NOTE: I hope the content has no tickets in it to duplicate! *)
      let l = {issuer = ticket.issuer; content = ticket.content; amount = left;} in
      let r = {issuer = ticket.issuer; content = ticket.content; amount = right;} in
      Some (l, r)

  let join_tickets (t1, t2) =
    if (t1.content <> t2.content) || (t1.issuer <> t2.issuer)
    then None
    else Some {issuer = t1.issuer; content = t1.content; amount = add_nat_nat t1.amount t2.amount;}

  let reset () =
    now := timestamp_from_seconds_literal 0;
    level := nat_from_literal "0n";
    amount := tez_from_literal "0mutez"

  let new_transaction ~seconds_passed ~blocks_passed ~sender:address_ ~amount:amount_ =
    (* You can not increase blocks_passed without seconds_passed, or vice versa. *)
    assert ((seconds_passed = 0 && blocks_passed = 0)
            || (seconds_passed > 0) && (blocks_passed > 0));
    now := Z.(!now + Z.of_int seconds_passed);
    level := Z.(!level + Z.of_int blocks_passed);
    sender := address_;
    amount := amount_
end
let tezos_level: nat ref = Tezos.level

let string_of_int = Z.to_string
let string_of_tez x = Z.to_string x ^ "mutez"
let string_of_timestamp = Z.to_string

let pp_int fmt z = Format.pp_print_string fmt (string_of_int z)
let pp_tez fmt z = Format.pp_print_string fmt (string_of_tez z)
let pp_timestamp fmt z = Format.pp_print_string fmt (string_of_timestamp z)

let format_int = Z.format
let div_rem_int_int = Z.div_rem
