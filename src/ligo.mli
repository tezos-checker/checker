type address
(**
   An untyped address which can refer to a smart contract or account.
*)

type ('key, 'value) big_map

(**
   The type of a big map from values of type key to values of type value is big_map (key, value).

   Be aware that a big_map cannot appear inside another big_map.
*)

(* type bool *)

type bytes

type 'parameter contract
(**
   A typed contract.

   Use unit as parameter to indicate an implicit account.
*)

type chain_id
(**
    The identifier of a chain, used to indicate test or main chains.
*)

type int
(**
   An integer.

   The only size limit to integers is gas.
*)

type key
(**
    A public cryptographic key.
*)

type key_hash
(**
    The hash of a public cryptographic key.
*)

type 't list
(**
    A sequence of elements of the same type.
*)

(* type ('key, 'value) map *)
(**
    The type of a map from values of type key to values of type value is map (key, value).
*)

type nat
(**
    A natural number.

     The only size limit to natural numbers is gas.
*)

type mutez

type operation
(**
    An operation emitted by the contract
*)

type 'value set

type signature
(**
    A cryptographic signature.
*)

type string
(**
    A sequence of characters.
*)

type tez
(**
    A specific type for tokens.
*)

type timestamp
(**
    A date in the real world.
*)

(* type unit *)

val is_nat: int -> nat option
(**
   Convert an int to a nat if possible.

   Note that Michelson.is_nat is deprecated. Please use is_nat instead.
*)

val abs: int -> nat
(**
   Cast an int to nat.
*)

val int: nat -> int
(**
   Cast an nat to int.
*)

val unit: unit
(**
   A helper to create a unit.
*)

val failwith : 'a -> unit
(**
   Cause the contract to fail with an error message or integer. Other types are not supported at the moment.

   Using this currently requires in general a type annotation on the failwith call.
*)

(* val assert : bool -> unit *)
(**
    Check if a certain condition has been met. If not the contract will fail.
*)

val ediv_int_int : int -> int -> (int * nat) option

val ediv_mutez_nat : mutez -> nat -> (mutez * mutez) option

val ediv_mutez_mutez : mutez -> mutez -> (nat * mutez) option

val ediv_nat_nat : nat -> nat -> (nat * nat) option

(* TODO: arithmetic shim functions *)
