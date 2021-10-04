open OUnit2
open TestLib

let property_test_count = 100
let qcheck_to_ounit t = OUnit.ounit2_of_ounit1 @@ QCheck_ounit.to_ounit_test t

(* NOTE: Ideally we want the definition of ptr to be opaque. A sensible
 * property to expect would be the following:
 *
 *   forall (n: nat) (m: nat).
 *     n <> m =>
 *     ptr_next (..n_times (ptr_next ptr_null)..) <> ptr_next (..m_times (ptr_next ptr_null)..)
 *
 *   forall (n: nat) (m: nat).
 *     n = m =>
 *     ptr_next (..n_times (ptr_next ptr_null)..) = ptr_next (..m_times (ptr_next ptr_null)..)
 *
 * But that can be satisfied in many ways (hashing, adding a non-zero value,
 * etc.), not just by adding 1 every time we call ptr_next. The simplest way to
 * address the mutation is to use the show instance for pointers for writing a
 * test tied to our current implementation.
*)

(* Perhaps move this to a shared location? Only when needed. *)
let apply_times f n e = List.fold_left (fun x _ -> f x) e (List.init n (fun x -> x + 1))

let coerce_ptr_to_int (p : Ptr.ptr) : int = Stdlib.int_of_string (Ptr.show_ptr p)

let test_ptr_next_property =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_ptr_next_property"
    ~count:property_test_count
    (QCheck.pair QCheck.small_nat QCheck.small_nat) (* must be small_nat or it will run for a very long time *)
  @@ fun (n, m) ->
  if n <> m then (* n <> m *)
    assert_bool
      "test_ptr_next_property (unequal)"
      (apply_times Ptr.ptr_next n Ptr.ptr_null <> apply_times Ptr.ptr_next m Ptr.ptr_null)
  else (* n = m *)
    assert_bool
      "test_ptr_next_property (equal)"
      (apply_times Ptr.ptr_next n Ptr.ptr_null =  apply_times Ptr.ptr_next m Ptr.ptr_null);
  true

(* too tight (to catch mutations): null = 0 *)
let test_ptr_null_is_zero =
  "test_ptr_null_is_zero" >:: fun _ ->
    assert_string_equal
      ~expected:"0"
      ~real:(Ptr.show_ptr Ptr.ptr_null)

(* too tight (to catch mutations): next ptr - ptr = 1 *)
let test_ptr_next_difference =
  qcheck_to_ounit
  @@ QCheck.Test.make
    ~name:"test_ptr_next_difference"
    ~count:property_test_count
    QCheck.small_nat (* must be small_nat or it will run for a very long time *)
  @@ fun n ->
  let ptr = apply_times Ptr.ptr_next n Ptr.ptr_null in
  let difference = coerce_ptr_to_int (Ptr.ptr_next ptr) - coerce_ptr_to_int ptr in
  assert_stdlib_int_equal
    ~expected:1
    ~real:difference;
  true

let suite =
  "Ptr tests" >::: [
    (* property-based random tests *)
    test_ptr_next_property;
    test_ptr_next_difference;

    (* unit tests *)
    test_ptr_null_is_zero;
  ]

let () =
  run_test_tt_main
    suite
