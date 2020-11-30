#include "avl.mligo"

type storage = {
   mem: mem;
   root: avl_ptr;
}

type action =
  | Add of int

let main ((p, storage): action * storage): operation list * storage =
 let storage =
   match p with
   | Add i ->
     let (mem, leaf) = avl_push_back storage.mem storage.root i 1mutez in
     let storage = { storage with mem = mem; } in
     storage
 in ([] : operation list), storage

