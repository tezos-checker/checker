#include "avl.mligo"

type storage = {
   mem: mem;
   root: avl_ptr;
}

type action =
  | Nop
  | AddMany of int

let initial_storage: storage =
  let mem = {max_id = 0; mem = (Big_map.empty: (ptr, node) big_map); } in
  let (mem, ptr) = avl_mk_empty mem 42 in
  { mem = mem; root = ptr }

let rec add_many (p: int * storage) : storage =
  let (count, storage) = p in
  if count <= 0
  then storage
  else begin
    let (mem, leaf) = avl_push_back storage.mem storage.root 1 1mutez in
    let storage = { storage with mem = mem; } in
    add_many (count-1, storage)
  end

let main ((p, storage): action * storage): operation list * storage =
 let storage =
   match p with
   | Nop ->
     storage
   | AddMany count ->
     add_many (count, storage) in
 (([] : operation list), storage)

