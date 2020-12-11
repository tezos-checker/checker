type branch = {
  left: int;
  left_height: int;
  left_tez: tez;
  right_tez: tez;
  right_height: int;
  right: int;
  parent: int;
}

type leaf = {
  value: int;
  tez: tez;
  parent: int;
}

type node =
  | Leaf of leaf
  | Branch of branch
  | Root of (int * int)

type storage = {
   contents: (int, node) big_map ;
}

type action =
  | Add of (int * node)
  | Lookup of int
  | Nop

let initial_storage: storage =
  { contents = (Big_map.empty: (int, node) big_map); }

let main ((p, storage): action * storage): operation list * storage =
 let storage =
   match p with
   | Add i ->
     let (k, v) = i in
     let contents = Big_map.update k (Some v) storage.contents in
     { storage with contents=contents; }
   | Lookup i ->
     let ign = Big_map.find i storage.contents in
     storage
   | Nop ->
     storage
 in ([] : operation list), storage

