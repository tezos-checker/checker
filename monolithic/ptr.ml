(* ************************************************************************* *)
(*                                Pointers                                   *)
(* ************************************************************************* *)
type t = int64 [@@deriving show]

let null = Int64.zero
let init = Int64.one
let next t = Int64.succ t

let compare = Int64.compare

let to_string = Int64.to_string
