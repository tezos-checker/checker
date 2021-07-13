open OUnit2

open Mem
let suite =
  "MemTests" >::: [
    "mem_get - fails for missing ptr" >::
    (fun _ ->
       assert_raises
         (Failure "mem_get: not found")
         (fun _ -> mem_get mem_empty (Ptr.ptr_init))
    )
  ]

let () =
  run_test_tt_main
    suite
