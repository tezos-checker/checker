(* TODO: Perhaps we should represent this as a Nat.t, instead of an integer. It
 * all boils down to what we wish to use when calculating (e.g. negative kit
 * can be useful for the imbalance adjustment, which can be either positive or
 * negative). Leave an int for now, but we should make an explicit decision on
 * this. *)
type t = Z.t
let scaling_factor = Z.of_int64 1000000L
let scaling_exponent = 6

(* Basic arithmetic operations. *)
let ( + ) x y = Z.(x + y)
let ( - ) x y = Z.(x - y)

let compare x y = Z.compare x y

let zero = Z.zero
let one = scaling_factor

(* Conversions to/from other types. *)
let of_mukit amount = amount
let to_mukit amount = amount

let to_ratio amount = Ratio.make amount scaling_factor
let of_ratio_ceil amount = Z.(cdiv (Ratio.num amount * scaling_factor) (Ratio.den amount))
let of_ratio_floor amount = Z.(fdiv (Ratio.num amount * scaling_factor) (Ratio.den amount))
(* George: do we need flooring-division or truncating-division? more thought is needed *)

let scale amount fp = (* NOTE: IT FLOORS *)
  of_ratio_floor (Ratio.mul (FixedPoint.to_ratio fp) (to_ratio amount))

(* Pretty printing functions *)
let show amount =
  let zfill s width =
    let to_fill = Stdlib.(width - (String.length s)) in
    if to_fill <= 0
    then s
    else (String.make to_fill '0') ^ s in

  let sign = if amount < Z.zero then "-" else "" in
  let (upper, lower) = Z.div_rem (Z.abs amount) scaling_factor in
  Format.sprintf "%s%s.%s"
    sign
    (Z.to_string upper)
    (zfill (Z.to_string lower) scaling_exponent)

let pp ppf amount = Format.fprintf ppf "%s" (show amount)

(* Kit are really tickets. *)
type kit_token_content = Kit [@@deriving show]
type token = kit_token_content Ticket.t [@@deriving show]

let issue ~(tezos: Tezos.t) (kit: t) : token =
  match Nat.of_int kit with
  | None -> failwith "Kit.issue: cannot issue a negative number of mukit!"
  | Some n -> Ticket.create ~issuer:tezos.self ~amount:n ~content:Kit

type Error.error +=
  | InvalidKitToken

(** Check whether a kit token is valid. A kit token is valid if (a) it is
  * issued by checker, and (b) is tagged appropriately (this is already
  * enforced by its type). *)
let is_token_valid ~(tezos:Tezos.t) (token: token) : (token, Error.error) result =
  let issuer, amount, _content, same_ticket = Ticket.read token in
  let is_valid = issuer = tezos.self && amount >= Nat.zero in
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
  let _issuer, mukit, _content, same_token = Ticket.read token in
  (Nat.to_int mukit, same_token)

let split_or_fail (token: token) (left: t) (right: t) : token * token =
  match Nat.of_int left, Nat.of_int right with
  | Some l, Some r -> Option.get (Ticket.split token l r)
  | _, _ -> failwith "Kit.split_or_fail: cannot split using a negative number of mukit!"

let join_or_fail (left: token) (right: token) : token =
  Option.get (Ticket.join left right)

