open Ratio
open Common
open FixedPoint

(* TODO: Perhaps we should represent this as a Ligo.nat, instead of an integer. It
 * all boils down to what we wish to use when calculating (e.g. negative kit
 * can be useful for the imbalance adjustment, which can be either positive or
 * negative). Leave an int for now, but we should make an explicit decision on
 * this. *)
type kit = Ligo.int
let scaling_factor = Ligo.int_from_literal "1_000_000"

(* Basic arithmetic operations. *)
let add (x: kit) (y: kit) = Ligo.add_int_int x y
let sub (x: kit) (y: kit) = Ligo.sub_int_int x y

let min (x: kit) (y: kit) = if Ligo.leq_int_int x y then x else y
let max (x: kit) (y: kit) = if Ligo.geq_int_int x y then x else y

let zero = Ligo.int_from_literal "0"
let one = scaling_factor

(* Conversions to/from other types. *)
let of_mukit (amnt: kit) = amnt
let to_mukit (amnt: kit) = amnt

let to_ratio (amnt: kit) = make_ratio amnt scaling_factor
let of_ratio_ceil  (amnt: ratio) = cdiv_int_int (Ligo.mul_int_int (ratio_num amnt) scaling_factor) (ratio_den amnt)
let of_ratio_floor (amnt: ratio) = fdiv_int_int (Ligo.mul_int_int (ratio_num amnt) scaling_factor) (ratio_den amnt)
(* George: do we need flooring-division or truncating-division? more thought is needed *)

let scale (amnt: kit) (fp: fixedpoint) =
  of_ratio_floor (mul_ratio (fixedpoint_to_ratio fp) (to_ratio amnt))

(* Kit are really tickets. *)
type kit_token_content = Kit [@@deriving show]
type token = kit_token_content Ligo.ticket [@@deriving show]

let issue (kit: kit) : token =
  match Ligo.is_nat kit with
  | None -> (failwith "Kit.issue: cannot issue a negative number of mukit!": token)
  | Some n -> Ligo.Tezos.create_ticket (Kit) n

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let assert_valid_kit_token (token: token) : token =
  let (issuer, (content, amnt)), same_ticket = Ligo.Tezos.read_ticket token in
  let is_valid = issuer = Ligo.Tezos.self_address && amnt >= Ligo.nat_from_literal "0n" in (* TODO: > Nat.zero perhaps? *)
  if is_valid
  then same_ticket
  else (failwith "InvalidKitToken": token)

let read_kit (token: token) : kit * token =
  let (issuer, (content, mukit)), same_token = Ligo.Tezos.read_ticket token in
  (Ligo.int mukit, same_token)

let split_or_fail (token: token) (left: kit) (right: kit) : token * token =
  let l = match Ligo.is_nat left with Some l -> l | None -> (failwith "Kit.split_or_fail: cannot split using a negative number of mukit!": Ligo.nat) in
  let r = match Ligo.is_nat right with Some l -> l | None -> (failwith "Kit.split_or_fail: cannot split using a negative number of mukit!": Ligo.nat) in
  match Ligo.Tezos.split_ticket token (l, r) with
  | Some a -> a
  | None -> (failwith "split_or_fail: failed": token * token)

let join_or_fail (left: token) (right: token) : token =
  match Ligo.Tezos.join_tickets (left, right) with
  | Some a -> a
  | None -> (failwith "join_or_fail: failed": token)

(* BEGIN_OCAML *)
let compare x y = compare_int x y

let show amnt = Ligo.string_of_int amnt ^ "mukit"
let pp ppf amnt = Format.fprintf ppf "%s" (show amnt)
let pp_kit = pp
(* END_OCAML *)
