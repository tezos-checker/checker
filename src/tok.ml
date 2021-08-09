open Common
open Error

type tok = Ligo.nat

let[@inline] tok_decimal_digits = Ligo.nat_from_literal "6n"
let[@inline] tok_scaling_factor_int = Ligo.int_from_literal "1_000_000"
let[@inline] tok_scaling_factor_nat = Ligo.nat_from_literal "1_000_000n"

(* Basic arithmetic operations. *)
let[@inline] tok_add (x: tok) (y: tok) = Ligo.add_nat_nat x y
let tok_sub (x: tok) (y: tok) =
  match Ligo.is_nat (Ligo.sub_nat_nat x y) with
  | Some n -> n
  | None -> (Ligo.failwith internalError_TokSubNegative : tok)

let[@inline] tok_zero = Ligo.nat_from_literal "0n"
let[@inline] tok_one = tok_scaling_factor_nat

(* Conversions to/from other types. *)
let[@inline] tok_of_denomination (amnt: Ligo.nat) : tok = amnt
let[@inline] tok_to_denomination_int (amnt: tok) : Ligo.int = Ligo.int amnt
let[@inline] tok_to_denomination_nat (amnt: tok) : Ligo.nat = amnt

let tok_of_fraction_ceil (x_num: Ligo.int) (x_den: Ligo.int) : tok =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (Ligo.failwith internalError_TokOfFractionCeilNegative : tok)
  else Ligo.abs (cdiv_int_int (Ligo.mul_int_int x_num tok_scaling_factor_int) x_den)

let tok_of_fraction_floor (x_num: Ligo.int) (x_den: Ligo.int) : tok =
  assert (Ligo.gt_int_int x_den (Ligo.int_from_literal "0"));
  if Ligo.lt_int_int x_num (Ligo.int_from_literal "0")
  then (Ligo.failwith internalError_TokOfFractionFloorNegative : tok)
  else Ligo.abs (fdiv_int_int (Ligo.mul_int_int x_num tok_scaling_factor_int) x_den)

(* TOKFIX: temporary, compatibility layer *)
let tok_of_tez (tz: Ligo.tez) : tok =
  tok_of_fraction_floor
    (Ligo.int (Ligo.div_tez_tez tz (Ligo.tez_from_literal "1mutez")))
    (Ligo.int_from_literal "1_000_000")

let tez_of_tok (tk: tok) : Ligo.tez =
  fraction_to_tez_floor
    (tok_to_denomination_int tk)
    tok_scaling_factor_int

let[@inline] geq_tok_tok = Ligo.geq_nat_nat
let[@inline] leq_tok_tok = Ligo.leq_nat_nat

let[@inline] eq_tok_tok = Ligo.eq_nat_nat
let[@inline] gt_tok_tok = Ligo.gt_nat_nat
let[@inline] lt_tok_tok = Ligo.lt_nat_nat

let max_tok (x: tok) (y: tok) : tok = if geq_tok_tok x y then x else y

(* BEGIN_OCAML *)
[@@@coverage off]

let[@inline] tok_to_ratio (amnt: tok) : ratio = make_ratio (Ligo.int amnt) tok_scaling_factor_int

let tok_compare = compare_nat

let show_tok amnt =
  let zfill s width = match Stdlib.(width - (String.length s)) with
    | to_fill when to_fill <= 0 -> s
    | to_fill -> (String.make to_fill '0') ^ s
  in
  let as_string =
    if tok_decimal_digits = Ligo.nat_from_literal "0n" then
      Ligo.string_of_nat amnt
    else
      let d, r = Option.get (Ligo.ediv_nat_nat amnt tok_scaling_factor_nat) in
      let tok_decimal_digits = Stdlib.int_of_string (Ligo.string_of_nat tok_decimal_digits) in (* little hacky *)
      (Ligo.string_of_nat d) ^ "." ^ zfill (Ligo.string_of_nat r) tok_decimal_digits
  in as_string ^ "tok"

let pp_tok ppf amnt = Format.fprintf ppf "%s" (show_tok amnt)

[@@@coverage on]
(* END_OCAML *)
