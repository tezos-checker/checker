(* TODO: Perhaps we should represent this as a Ligo.nat, instead of an integer. It
 * all boils down to what we wish to use when calculating (e.g. negative kit
 * can be useful for the imbalance adjustment, which can be either positive or
 * negative). Leave an int for now, but we should make an explicit decision on
 * this. *)
type t = Ligo.int
let scaling_factor = Ligo.int_from_literal 1_000_000

(* Basic arithmetic operations. *)
let add x y = Ligo.add_int_int x y
let sub x y = Ligo.sub_int_int x y

let compare x y = Ligo.compare_int x y

let zero = Ligo.int_from_literal 0
let one = scaling_factor

(* Conversions to/from other types. *)
let of_mukit amount = amount
let to_mukit amount = amount

let to_ratio amount = Ratio.make amount scaling_factor
let of_ratio_ceil  amount = Ligo.cdiv_int_int (Ligo.mul_int_int (Ratio.num amount) scaling_factor) (Ratio.den amount)
let of_ratio_floor amount = Ligo.fdiv_int_int (Ligo.mul_int_int (Ratio.num amount) scaling_factor) (Ratio.den amount)
(* George: do we need flooring-division or truncating-division? more thought is needed *)

let scale amount fp =
  of_ratio_floor (Ratio.mul (FixedPoint.to_ratio fp) (to_ratio amount))

(* Pretty printing functions *)
let show amount = Ligo.string_of_int amount ^ "mukit"
let pp ppf amount = Format.fprintf ppf "%s" (show amount)

(* Kit are really tickets. *)
type kit_token_content = Kit [@@deriving show]
type token = kit_token_content Ticket.t [@@deriving show]

let issue ~(tezos: Tezos.t) (kit: t) : token =
  match Ligo.is_nat kit with
  | None -> failwith "Kit.issue: cannot issue a negative number of mukit!"
  | Some n -> Ticket.create tezos Kit n

type Error.error +=
  | InvalidKitToken

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let is_token_valid ~(tezos:Tezos.t) (token: token) : (token, Error.error) result =
  let (issuer, _content, amount), same_ticket = Ticket.read token in
  let is_valid = issuer = tezos.self && amount >= Ligo.nat_from_literal 0 in (* TODO: > Nat.zero perhaps? *)
  if is_valid then Ok same_ticket else Error InvalidKitToken

let with_valid_kit_token
    ~(tezos: Tezos.t)
    (token: token)
    (f: token -> ('a, Error.error) result)
  : ('a, Error.error) result =
  match is_token_valid ~tezos token with
  | Error err -> Error err
  | Ok token -> f token

let read_kit (token: token) : t * token =
  let (_issuer, _content, mukit), same_token = Ticket.read token in
  (Ligo.int mukit, same_token)

let split_or_fail (token: token) (left: t) (right: t) : token * token =
  match Ligo.is_nat left, Ligo.is_nat right with
  | Some l, Some r -> Option.get (Ticket.split token (l, r))
  | _, _ -> failwith "Kit.split_or_fail: cannot split using a negative number of mukit!"

let join_or_fail (left: token) (right: token) : token =
  Option.get (Ticket.join left right)

