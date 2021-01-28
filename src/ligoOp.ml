open Ligo

(* operation *)

type operation =
  | SetDelegate of key_hash option
  | NotImplementedYet

module Tezos = struct
  let set_delegate hash_option = SetDelegate hash_option
end
