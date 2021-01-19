open Ptr
open LiquidationAuctionTypes

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

type mem = {
  mem: (ptr, node) Ligo.big_map;
  last_ptr: ptr;
}

(* BEGIN_OCAML *)
type ops = { reads: int; writes: int }
[@@deriving show]
let ops: ops ref = ref { reads=0; writes=0 }
let reset_ops () = ops := { reads=0; writes=0 }
(* END_OCAML *)

let mem_empty = {last_ptr = ptr_null; mem = (Ligo.Big_map.empty: (ptr, node) Ligo.big_map); }

(* BEGIN_OCAML *)
let mem_bindings (mem: mem) = Ligo.Big_map.bindings mem.mem
(* END_OCAML *)

let mem_set (m: mem) (k: ptr) (v: node) : mem =
  (* BEGIN_OCAML *) ops := { !ops with writes = !ops.writes + 1; }; (* END_OCAML *)
  { m with mem = Ligo.Big_map.update k (Some v) m.mem }

let mem_new (m: mem) (v: node) : mem * ptr =
  let ptr = ptr_next m.last_ptr in
  let m =
    { mem = Ligo.Big_map.update ptr (Some v) m.mem;
      last_ptr = ptr; } in
  (m, ptr)

let mem_get (m: mem) (k: ptr) : node =
  (* BEGIN_OCAML *) ops := { !ops with reads = !ops.reads + 1; }; (* END_OCAML *)
  match Ligo.Big_map.find_opt k m.mem with
  | Some v -> v
  | None -> (failwith "mem_get: not found": node)

let mem_update (m: mem) (k: ptr) (f: node -> node) : mem =
  mem_set m k (f (mem_get m k))

let mem_del (mem: mem) (k: ptr) : mem =
  (* BEGIN_OCAML *) ops := { !ops with writes = !ops.writes + 1; }; (* END_OCAML *)
  let {mem=mem; last_ptr=last_ptr} = mem in
  assert (Ligo.Big_map.mem k mem);
  { mem=Ligo.Big_map.update k None mem; last_ptr=last_ptr }