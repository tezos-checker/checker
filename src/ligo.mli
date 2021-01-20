type ('key, 'value) big_map

module Big_map : sig
  val empty : ('key, 'value) big_map
  val find_opt : 'key -> ('key, 'value) big_map -> 'value option
  val update: 'key -> 'value option -> ('key, 'value) big_map -> ('key, 'value) big_map
  val mem: 'key -> ('key, 'value) big_map -> bool

  (*NON_LIGO*)
  val bindings : ('key, 'value) big_map -> ('key * 'value) list
end

(**
   The type of a big map from values of type key to values of type value is big_map (key, value).

   Be aware that a big_map cannot appear inside another big_map.
*)

(* type bool *)

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

type address
(**
   An untyped address which can refer to a smart contract or account.
*)
val address_from_literal : string -> address      (* IN LIGO: type-annotate with "address". *)

type int
(**
   An integer.

   The only size limit to integers is gas.
*)

val int_from_literal : Int.t -> int   (* IN LIGO: replace with "". *)
val int_from_int64: Int64.t -> int    (* NON-LIGO, temporary*)
val compare_int : int -> int -> Int.t (* NON-LIGO *)

type nat
(**
    A natural number.

     The only size limit to natural numbers is gas.
*)
val nat_from_literal : Int.t -> nat   (* IN LIGO: replace with "" and add "n" suffix. *)

type timestamp
(**
    A date in the real world.
*)
val timestamp_from_seconds_literal : Int.t -> timestamp (* NON-LIGO: in LIGO they come from strings, or Tezos.now *)

type tez
(**
    A specific type for tokens.
*)
val tez_from_mutez_literal : Int.t -> tez (* IN LIGO: replace with "" and add "mutez" suffix. *)

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

(* type unit *)

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

(* val ediv_tez_tez : tez -> tez -> (nat * tez) option *)

(* OPERATIONS ON int *)
val add_int_int : int -> int -> int  (* IN LIGO: ( + ) *)
val sub_int_int : int -> int -> int  (* IN LIGO: ( - ) *)
val mul_int_int : int -> int -> int  (* IN LIGO: ( * ) *)
val div_int_int : int -> int -> int  (* IN LIGO: ( / ) *)
val mod_int_int : int -> int -> nat  (* IN LIGO: mod, infix *)

val eq_int_int : int -> int -> bool  (* IN LIGO: ( = ) *)
val lt_int_int : int -> int -> bool  (* IN LIGO: ( < ) *)
val gt_int_int : int -> int -> bool  (* IN LIGO: ( > ) *)
val leq_int_int : int -> int -> bool (* IN LIGO: ( <= ) *)
val geq_int_int : int -> int -> bool (* IN LIGO: ( >= ) *)

val ediv_int_int : int -> int -> (int * nat) option

val of_string_base_int : Int.t -> string -> int (* NON-LIGO. Eventually find a different way to input FP numbers. *)

(* OPERATIONS ON nat *)
val add_nat_nat : nat -> nat -> nat (* IN LIGO: ( + ) *)
val sub_nat_nat : nat -> nat -> int (* IN LIGO: ( - ) *)
val mul_nat_nat : nat -> nat -> nat (* IN LIGO: ( * ) *)

val eq_nat_nat : nat -> nat -> bool  (* IN LIGO: ( = ) *)
val lt_nat_nat : nat -> nat -> bool  (* IN LIGO: ( < ) *)
val leq_nat_nat : nat -> nat -> bool (* IN LIGO: ( <= ) *)
val geq_nat_nat : nat -> nat -> bool (* IN LIGO: ( >= ) *)

val int : nat -> int                (* IN LIGO: int *)
val abs : int -> nat                (* IN LIGO: abs *)
val is_nat : int -> nat option      (* IN LIGO: is_nat *)

val ediv_nat_nat : nat -> nat -> (nat * nat) option (* IN LIGO: ediv *)

(* OPERATIONS ON timestamp *)
val add_timestamp_int : timestamp -> int -> timestamp       (* IN LIGO: ( + ) *)
val sub_timestamp_timestamp : timestamp -> timestamp -> int (* IN LIGO: ( - ) *)

(* OPERATIONS ON tez *)
val add_tez_tez : tez -> tez -> tez (* IN LIGO: ( + ) *)
val sub_tez_tez : tez -> tez -> tez (* IN LIGO: ( - ) *)
val mul_nat_tez : nat -> tez -> tez (* IN LIGO: ( * ) *)
val mul_tez_nat : tez -> nat -> tez (* IN LIGO: ( * ) *)
val div_tez_tez : tez -> tez -> nat (* IN LIGO: ( / ) *)

val ediv_tez_nat : tez -> nat -> (tez * tez) option (* IN LIGO: ediv *)

val eq_tez_tez : tez -> tez -> bool  (* IN LIGO: ( = ) *)
val lt_tez_tez : tez -> tez -> bool  (* IN LIGO: ( < ) *)
val leq_tez_tez : tez -> tez -> bool (* IN LIGO: ( <= ) *)
val geq_tez_tez : tez -> tez -> bool (* IN LIGO: ( >= ) *)

(* val add_nat_int : nat -> int -> int *)

(* val add_int_nat : int -> nat -> int *)

(* val add_int_timestamp : int -> timestamp -> timestamp *)

(* Subtractions *)

(* val sub_timestamp_int : timestamp -> int -> timestamp *)

(* val sub_int_nat : int -> nat -> int *)

(* val sub_nat_int : nat -> int -> int *)

(* BEGIN_OCAML *)
val string_of_int : int -> string
val string_of_nat : nat -> string
val string_of_tez : tez -> string
val string_of_timestamp : timestamp -> string
val string_of_address : address -> string

val pp_address : Format.formatter -> address -> unit
val pp_int : Format.formatter -> int -> unit
val pp_nat : Format.formatter -> nat -> unit
val pp_tez : Format.formatter -> tez -> unit
val pp_timestamp : Format.formatter -> timestamp -> unit

val format_int : string -> int -> string
val div_rem_int_int : int -> int -> (int * int)
(* END_OCAML *)
