open OUnit2
open Error
open Mem

let suite =
  "MemTests" >::: [
    "mem_get - fails for missing ptr" >::
    (fun _ ->
       assert_raises
         (Failure (Ligo.string_of_int internalError_MemGetElementNotFound))
         (fun _ -> mem_get mem_empty (Ptr.ptr_next Ptr.ptr_null))
    )
  ]

let () =
  run_test_tt_main
    suite
