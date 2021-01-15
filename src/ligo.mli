(* type address *)
(**
   An untyped address which can refer to a smart contract or account.
*)

(* type ('key, 'value) big_map *)

(**
   The type of a big map from values of type key to values of type value is big_map (key, value).

   Be aware that a big_map cannot appear inside another big_map.
*)

(* type bool *)

(* NOTE: Booleans use different notation in LIGO than they do in OCaml:
 * - https://ligolang.org/docs/language-basics/boolean-if-else#booleans
 * Differences:
 * - OCaml: true / false vs. LIGO: True / False
 * - OCaml: && / ||      vs. LIGO: and / or
 * - OCaml: <>           vs. LIGO: =/=
*)

(* type bytes *)

(* type 'parameter contract *)
(**
   A typed contract.

   Use unit as parameter to indicate an implicit account.
*)

(* type chain_id *)
(**
    The identifier of a chain, used to indicate test or main chains.
*)

type int
(**
   An integer.

   The only size limit to integers is gas.
*)

val int_from_literal : Int.t -> int   (* IN LIGO: replace with "". *)
val compare_int : int -> int -> Int.t (* NON-LIGO *)
val string_of_int : int -> string     (* NON-LIGO *)

type nat
(**
    A natural number.

     The only size limit to natural numbers is gas.
*)

val nat_from_literal : Int.t -> nat   (* IN LIGO: replace with "" and add "n" ssiffix. *)
val compare_nat : nat -> nat -> Int.t (* NON-LIGO *)
val string_of_nat : nat -> string     (* NON-LIGO *)

type timestamp
(**
    A date in the real world.
*)
val timestamp_from_seconds_literal : Int.t -> timestamp (* NON-LIGO: in LIGO they come from strings, or Tezos.now *)
val compare_timestamp : timestamp -> timestamp -> Int.t (* NON-LIGO *)
val string_of_timestamp : timestamp -> string           (* NON-LIGO *)

(* type key *)
(**
    A public cryptographic key.
*)

(* type key_hash *)
(**
    The hash of a public cryptographic key.
*)

(* type 't list *)
(**
    A sequence of elements of the same type.
*)

(* type ('key, 'value) map *)
(**
    The type of a map from values of type key to values of type value is map (key, value).
*)

(* type operation *)
(**
    An operation emitted by the contract
*)

(* type 'value set *)

(* type signature *)
(**
    A cryptographic signature.
*)

(* type string *)
(**
    A sequence of characters.
*)

(* type tez *)
(**
    A specific type for tokens.
*)

(* type unit *)

(* val is_nat: int -> nat option *)
(**
   Convert an int to a nat if possible.

   Note that Michelson.is_nat is deprecated. Please use is_nat instead.
*)

(* val abs: int -> nat *)
(**
   Cast an int to nat.
*)

(* val int: nat -> int *)
(**
   Cast an nat to int.
*)

(* val unit: unit *)
(**
   A helper to create a unit.
*)

(* val failwith : 'a -> unit *)
(**
   Cause the contract to fail with an error message or integer. Other types are not supported at the moment.

   Using this currently requires in general a type annotation on the failwith call.
*)

(* val assert : bool -> unit *)
(**
    Check if a certain condition has been met. If not the contract will fail.
*)

(* val ediv_int_int : int -> int -> (int * nat) option *)

(* val ediv_tez_nat : tez -> nat -> (tez * tez) option *)

(* val ediv_tez_tez : tez -> tez -> (nat * tez) option *)

(* val ediv_nat_nat : nat -> nat -> (nat * nat) option *)

(* OPERATIONS ON int *)
val add_int_int : int -> int -> int  (* IN LIGO: ( + ) *)
val sub_int_int : int -> int -> int  (* IN LIGO: ( - ) *)
val mul_int_int : int -> int -> int  (* IN LIGO: ( * ) *)
val div_int_int : int -> int -> int  (* IN LIGO: ( / ) *)
val eq_int_int : int -> int -> bool  (* IN LIGO: ( = ) *)
val lt_int_int : int -> int -> bool  (* IN LIGO: ( < ) *)
val leq_int_int : int -> int -> bool (* IN LIGO: ( <= ) *)
val geq_int_int : int -> int -> bool (* IN LIGO: ( >= ) *)

val cdiv_int_int : int -> int -> int (* NON-LIGO *)
val fdiv_int_int : int -> int -> int (* NON-LIGO *)
val pow_int_nat : int -> nat -> int (* NON-LIGO *)
val shift_right_trunc_int_nat : int -> Int.t -> int (* NON-LIGO, wrong type also, must be (int -> nat -> int) *)
val shift_left_int_nat : int -> Int.t -> int (* NON-LIGO, wrong type also, must be (int -> nat -> int) *)
val gcd_int_int : int -> int -> int (* NON-LIGO *)
val sign_int : int -> Int.t (* NON-LIGO, I think? *)
val neg_int : int -> int (* NON-LIGO, I think? *)
val abs_int : int -> int (* NON-LIGO. Ideally we should use Ligo.(abs : int -> nat) and then lift back to int? *)
val of_string_base_int : Int.t -> string -> int (* NON-LIGO. Eventually find a different way to input FP numbers. *)
val div_rem_int_int : int -> int -> (int * int) (* NON-LIGO. Would be nice to use ediv_int_int for this.. *)

(* OPERATIONS ON nat *)

val add_nat_nat : nat -> nat -> nat (* IN LIGO: ( + ) *)
val sub_nat_nat : nat -> nat -> int (* IN LIGO: ( - ) *)
val int : nat -> int                (* IN LIGO: int *)
val abs : int -> nat                (* IN LIGO: abs *)
val is_nat : int -> nat option      (* IN LIGO: is_nat *)

(* OPERATIONS ON timestamp *)

val add_timestamp_int : timestamp -> int -> timestamp       (* IN LIGO: ( + ) *)
val sub_timestamp_timestamp : timestamp -> timestamp -> int (* IN LIGO: ( - ) *)

(* val add_tez_tez : tez -> tez -> tez *)

(* val add_nat_int : nat -> int -> int *)

(* val add_int_nat : int -> nat -> int *)

(* val add_int_timestamp : int -> timestamp -> timestamp *)

(* Subtractions *)

(* val sub_timestamp_int : timestamp -> int -> timestamp *)


(* val sub_int_nat : int -> nat -> int *)

(* val sub_nat_int : nat -> int -> int *)

(* TODO: The following also seem to be allowed,
 *
 *   val sub_mutez_mutez : mutez -> mutez -> mutez
 *   val sub_mutez_tez : mutez -> tez -> mutez
 *   val sub_tez_mutez : tez -> mutez -> mutez
 *   val sub_tez_tez : tez -> tez -> mutez
 *
 * but I (George) am totally unclear as to what is mutez as a type. My
 * impression is that the only type we have for tez is "tez", values of which
 * we can construct by giving a number of tez or mutez.
*)

(* TODO: arithmetic shim functions *)

(* BEGIN_OCAML *)

val pp_int : Format.formatter -> int -> unit
val pp_nat : Format.formatter -> nat -> unit
val pp_timestamp : Format.formatter -> timestamp -> unit
val format_int : string -> int -> string
