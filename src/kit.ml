open Ratio
open Common
open FixedPoint

(* TODO: Perhaps we should represent this as a Ligo.nat, instead of an integer. It
 * all boils down to what we wish to use when calculating (e.g. negative kit
 * can be useful for the imbalance adjustment, which can be either positive or
 * negative). Leave an int for now, but we should make an explicit decision on
 * this. *)
type kit = Ligo.nat
let kit_scaling_factor = Ligo.nat_from_literal "1_000_000n"

(* Basic arithmetic operations. *)
let kit_add (x: kit) (y: kit) = Ligo.add_nat_nat x y
let kit_sub (x: kit) (y: kit) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  | None -> (failwith "Kit.kit_sub: negative" : kit)

let kit_min (x: kit) (y: kit) = if Ligo.leq_nat_nat x y then x else y
let kit_max (x: kit) (y: kit) = if Ligo.geq_nat_nat x y then x else y

let kit_zero = Ligo.nat_from_literal "0n"
let kit_one = kit_scaling_factor

(* Conversions to/from other types. *)
let kit_of_mukit (amnt: Ligo.nat) : kit = amnt
let kit_to_mukit (amnt: kit) : Ligo.nat = amnt

let kit_to_ratio (amnt: kit) : ratio = make_ratio (Ligo.int amnt) (Ligo.int kit_scaling_factor)

let kit_of_ratio_ceil  (amnt: ratio) : kit =
  if lt_ratio_ratio amnt zero_ratio
  then (failwith "Kit.kit_of_ratio_ceil: negative" : kit)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int (ratio_num amnt) (Ligo.int kit_scaling_factor)) (ratio_den amnt))

let kit_of_ratio_floor (amnt: ratio) : kit =
  if lt_ratio_ratio amnt zero_ratio
  then (failwith "Kit.kit_of_ratio_floor: negative" : kit)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int (ratio_num amnt) (Ligo.int kit_scaling_factor)) (ratio_den amnt))

let kit_scale (amnt: kit) (fp: fixedpoint) =
  kit_of_ratio_floor (mul_ratio (fixedpoint_to_ratio fp) (kit_to_ratio amnt))

(* Kit are really tickets. *)
type kit_token_content = Kit [@@deriving show]
type kit_token = kit_token_content Ligo.ticket [@@deriving show]

let kit_issue (kit: kit) : kit_token = Ligo.Tezos.create_ticket (Kit) kit

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let assert_valid_kit_token (token: kit_token) : kit_token =
  let (issuer, (_content, _amnt)), same_ticket = Ligo.Tezos.read_ticket token in
  let is_valid = issuer = checker_address in (* TODO: amnt > Nat.zero perhaps? *)
  if is_valid
  then same_ticket
  else (failwith "InvalidKitToken": kit_token)

let read_kit (token: kit_token) : kit * kit_token =
  let (_issuer, (_content, mukit)), same_token = Ligo.Tezos.read_ticket token in
  (mukit, same_token)

let kit_split_or_fail (token: kit_token) (left: kit) (right: kit) : kit_token * kit_token =
  match Ligo.Tezos.split_ticket token (left, right) with
  | Some a -> a
  | None -> (failwith "split_or_fail: failed": kit_token * kit_token)

let kit_join_or_fail (left: kit_token) (right: kit_token) : kit_token =
  match Ligo.Tezos.join_tickets (left, right) with
  | Some a -> a
  | None -> (failwith "join_or_fail: failed": kit_token)

(* BEGIN_OCAML *)
let kit_compare x y = compare_nat x y

let show_kit amnt = Ligo.string_of_nat amnt ^ "mukit"
let pp_kit ppf amnt = Format.fprintf ppf "%s" (show_kit amnt)
(* END_OCAML *)
