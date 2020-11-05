(*
 * BigMap
 *
 * We use a bigmap as our memory, and an int64 as addresses.
 *
 * There is no garbage collection, so operations are responsible for
 * not leaving any dangling pointers.
 *
 * We always increase the memory addresses, even after removals. But
 * int64 is big enough that it shouldn't be an issue.
 *
 * TODO: Maybe we should use something like [int8] as a variable
 * width address.
 *)

module BigMap = Map.Make(Int64)

type ptr = int64 [@@deriving show]

let mem_next_ptr (m: 'a BigMap.t): ptr =
  match BigMap.max_binding_opt m with
  | None -> Int64.one
  | Some (t, _) -> Int64.succ t

let null: ptr = Int64.zero
let is_null (p: ptr) = p == null

let mem_set (m: 'a BigMap.t) (k: ptr) (v: 'a) : 'a BigMap.t =
  BigMap.add k v m

let mem_new (m: 'a BigMap.t) (v: 'a) : 'a BigMap.t * ptr =
  let ptr = mem_next_ptr m in
  (mem_set m ptr v, ptr)

let mem_get (m: 'a BigMap.t) (k: ptr) : 'a =
  BigMap.find k m

let mem_update (m: 'a BigMap.t) (k: ptr) (f: 'a -> 'a) : 'a BigMap.t =
  mem_set m k @@ f (mem_get m k)

let mem_del (m: 'a BigMap.t) (k: ptr) : 'a BigMap.t =
  assert (BigMap.mem k m);
  BigMap.remove k m
