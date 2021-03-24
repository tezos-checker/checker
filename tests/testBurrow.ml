open OUnit2
open TestCommon
open CheckerTypes

let suite =
  "Checker tests" >::: [
    (* ("initial touch (noop)" >::
       fun _ ->
       Ligo.Tezos.reset ();
       let checker1 = initial_checker in
       let ops, checker2 = Checker.touch_with_index checker1 (Ligo.tez_from_literal "0mutez") in

       assert_equal [] ops ~printer:show_operation_list;
       assert_equal checker1 checker2; (* NOTE: we really want them to be identical here, hence the '='. *)
       ()
       ); *)


  ]
