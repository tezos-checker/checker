open Ratio

(* TODO: Perhaps we should represent this as a Ligo.nat, instead of an integer. It
 * all boils down to what we wish to use when calculating (e.g. negative kit
 * can be useful for the imbalance adjustment, which can be either positive or
 * negative). Leave an int for now, but we should make an explicit decision on
 * this. *)
type t = Ligo.int
let scaling_factor = Ligo.int_from_literal "1_000_000"

(* Basic arithmetic operations. *)
let add x y = Ligo.add_int_int x y
let sub x y = Ligo.sub_int_int x y

let min x y = if Ligo.leq_int_int x y then x else y
let max x y = if Ligo.geq_int_int x y then x else y

let zero = Ligo.int_from_literal "0"
let one = scaling_factor

(* Conversions to/from other types. *)
let of_mukit amount = amount
let to_mukit amount = amount

let to_ratio amount = make_ratio amount scaling_factor
let of_ratio_ceil  amount = Common.cdiv_int_int (Ligo.mul_int_int (ratio_num amount) scaling_factor) (ratio_den amount)
let of_ratio_floor amount = Common.fdiv_int_int (Ligo.mul_int_int (ratio_num amount) scaling_factor) (ratio_den amount)
(* George: do we need flooring-division or truncating-division? more thought is needed *)

let scale amount fp =
  of_ratio_floor (mul_ratio (FixedPoint.to_ratio fp) (to_ratio amount))

(* Kit are really tickets. *)
type kit_token_content = Kit [@@deriving show]
type token = kit_token_content Ligo.ticket [@@deriving show]

let issue (kit: t) : token =
  match Ligo.is_nat kit with
  | None -> failwith "Kit.issue: cannot issue a negative number of mukit!"
  | Some n -> Ligo.Tezos.create_ticket Kit n

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let assert_valid_kit_token (token: token) : token =
  let (issuer, (_content, amount)), same_ticket = Ligo.Tezos.read_ticket token in
  let is_valid = issuer = Ligo.Tezos.self_address && amount >= Ligo.nat_from_literal "0n" in (* TODO: > Nat.zero perhaps? *)
  if is_valid then same_ticket else failwith "InvalidKitToken"

let read_kit (token: token) : t * token =
  let (_issuer, (_content, mukit)), same_token = Ligo.Tezos.read_ticket token in
  (Ligo.int mukit, same_token)

let split_or_fail (token: token) (left: t) (right: t) : token * token =
  match Ligo.is_nat left, Ligo.is_nat right with
  | Some l, Some r -> Option.get (Ligo.Tezos.split_ticket token (l, r))
  | _, _ -> failwith "Kit.split_or_fail: cannot split using a negative number of mukit!"

let join_or_fail (left: token) (right: token) : token =
  Option.get (Ligo.Tezos.join_tickets left right)

(* BEGIN_OCAML *)
let compare x y = Common.compare_int x y

let show amount = Ligo.string_of_int amount ^ "mukit"
let pp ppf amount = Format.fprintf ppf "%s" (show amount)
(* END_OCAML *)
