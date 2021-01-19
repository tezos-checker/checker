(* FIXME find an efficient implementation.
 *   Map: Does not have two type parameters.
 *   Hashtbl: Mutable.
*)
type ('key, 'value) big_map = ('key * 'value) list

module Big_map = struct
  let empty = []

  let rec find_opt (k: 'key) (m: ('key, 'value) big_map) : 'value option =
    match m with
    | [] -> None
    | ((k', v')::xs) -> if k = k' then Some v' else find_opt k xs

  let rec update (k: 'key) (v: 'value option) (m: ('key, 'value) big_map) : ('key, 'value) big_map =
    match m with
    | [] -> (match v with | Some v -> [(k, v)] | None -> [])
    | ((k', v')::xs) ->
      if k = k'
      then match v with
        | Some v -> (k, v) :: xs
        | None -> xs
      else (k', v') :: update k v xs

  let bindings i = i

  let mem (k: 'key) (m: ('key, 'value) big_map) = Option.is_some (find_opt k m)
end

(* address *)

type address = string

let address_from_literal s = s

let compare_address = String.compare

let string_of_address s = s

let pp_address = Format.pp_print_string

(* int *)

type int = Z.t

let int_from_literal = Z.of_int
let int_from_int64 = Z.of_int64

let compare_int = Z.compare

let string_of_int = Z.to_string

let pp_int fmt z = Format.pp_print_string fmt (string_of_int z)

let add_int_int = Z.add

let sub_int_int = Z.sub

let mul_int_int = Z.mul

let eq_int_int = Z.equal

let lt_int_int = Z.lt

let leq_int_int = Z.leq

let geq_int_int = Z.geq

let div_int_int = Z.div

let cdiv_int_int = Z.cdiv

let fdiv_int_int = Z.fdiv

let pow_int_nat b e =
  assert (e >= Z.zero);
  assert (e <= Z.of_int max_int);
  Z.pow b (Z.to_int e)

let shift_right_trunc_int_nat = Z.shift_right_trunc

let shift_left_int_nat = Z.shift_left

let gcd_int_int = Z.gcd

let sign_int = Z.sign

let neg_int = Z.neg

let abs_int = Z.abs

let of_string_base_int = Z.of_string_base

let div_rem_int_int = Z.div_rem

let format_int = Z.format

let int_min x y = if leq_int_int x y then x else y

let int_max x y = if geq_int_int x y then x else y

(* nat *)

type nat = Z.t

let add_nat_nat = Z.add

let sub_nat_nat = Z.sub

let mul_nat_nat = Z.mul

let eq_nat_nat = Z.equal

let lt_nat_nat = Z.lt

let leq_nat_nat = Z.leq

let geq_nat_nat = Z.geq

let int x = x

let abs = Z.abs

let is_nat x = if Z.lt x Z.zero then None else Some x

let string_of_nat = Z.to_string

let pp_nat fmt z = Format.pp_print_string fmt (string_of_nat z)

let compare_nat = Z.compare

let nat_from_literal x =
  if x < 0 then
    failwith "Ligo.nat_from_literal: negative"
  else
    Z.of_int x

(* timestamp *)

type timestamp = Z.t

let add_timestamp_int = Z.add

let sub_timestamp_timestamp = Z.sub

let string_of_timestamp = Z.to_string

let pp_timestamp fmt z = Format.pp_print_string fmt (string_of_timestamp z)

let compare_timestamp = Z.compare

let timestamp_from_seconds_literal s =
  if s < 0 then
    failwith "Ligo.timestamp_from_seconds_literal: negative"
  else
    Z.of_int s

(* tez *)

type tez = Z.t

let tez_from_mutez_literal x =
  if x < 0 then
    failwith "Ligo.nat_from_literal: negative"
  else
    Z.of_int x

let add_tez_tez = Z.add

let sub_tez_tez x y =
  if Z.lt x y then
    failwith "Ligo.sub_tez_tez: negative"
  else
    Z.sub x y

let mul_nat_tez = Z.mul

let mul_tez_nat = Z.mul

let ediv_tez_nat n d =
  try Some (Z.ediv_rem n d)
  with Division_by_zero -> None

let compare_tez = Z.compare

let string_of_tez x = Z.to_string x ^ "mutez"

let pp_tez fmt z = Format.pp_print_string fmt (string_of_tez z)

let eq_tez_tez = Z.equal

let lt_tez_tez = Z.lt

let leq_tez_tez = Z.leq

let geq_tez_tez = Z.geq

let tez_min x y = if leq_tez_tez x y then x else y

let tez_max x y = if geq_tez_tez x y then x else y

let tez_to_mutez x = x
