type storage = {
   contents: (int, int) big_map ;
}

type action =
  | Add of (int * int)
  | Lookup of int
  | Nop

let initial_storage: storage =
  { contents = (Big_map.empty: (int, int) big_map); }

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

