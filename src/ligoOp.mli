open Ligo

type operation =
  | SetDelegate of key_hash option
  | NotImplementedYet
  (**
      An operation emitted by the contract
  *)

module Tezos : sig
  val set_delegate : key_hash option -> operation
end
