open OUnit2

(* FIXME: Just for getting the file under the radar of the test coverage
 * checker, without triggering unused-open errors. Should be replaced with
 * actual tests. *)
module X = CheckerEntrypoints

let suite =
  "CheckerEntrypointsTests" >::: [
    (* Add tests here *)
  ]
