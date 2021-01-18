open Ptr

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
 *)

module M = Map.Make(Ptr)
type 'a t = 'a M.t

type ops = { reads: int; writes: int }
[@@deriving show]
let ops: ops ref = ref { reads=0; writes=0 }
let reset_ops () = ops := { reads=0; writes=0 }

type ptr = Ptr.t [@@deriving show]

let mem_next_ptr (m: 'a M.t): ptr =
  match M.max_binding_opt m with
  | None -> ptr_init
  | Some (t, _) -> ptr_next t

let is_null (p: ptr) = p = ptr_null

let empty = M.empty
let is_empty = M.is_empty
let bindings = M.bindings

let mem_set (m: 'a M.t) (k: ptr) (v: 'a) : 'a M.t =
  ops := { !ops with writes = !ops.writes + 1; };
  M.add k v m

let mem_new (m: 'a M.t) (v: 'a) : 'a M.t * ptr =
  let ptr = mem_next_ptr m in
  (mem_set m ptr v, ptr)

let mem_get (m: 'a M.t) (k: ptr) : 'a =
  ops := { !ops with reads = !ops.reads + 1; };
  M.find k m

let mem_update (m: 'a M.t) (k: ptr) (f: 'a -> 'a) : 'a M.t =
  mem_set m k @@ f (mem_get m k)

let mem_del (m: 'a M.t) (k: ptr) : 'a M.t =
  assert (M.mem k m);
  ops := { !ops with writes = !ops.writes + 1; };
  M.remove k m

module Foo = struct
  type ('k, 'v) map = ('k, 'v) Hashtbl.t
end
