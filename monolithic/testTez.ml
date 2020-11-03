open FixedPoint
open OUnit2
open Tez

type tz = Tez.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "TezTests" >::: [
    "tez arithmetic" >::
    (fun _ ->
       let tz1 = Tez.of_float 5.0 in
       let tz2 = Tez.of_float 3.0 in
       let tz3 = Tez.of_float 5.1234 in
       let tz4 = Tez.of_float 5001.0 in
       let tz5 = Tez.of_float 40.0 in
       let fp1 = FixedPoint.of_string "3.0" in
       assert_equal ~printer:show_tz (Tez.of_float 8.0) Tez.(tz1 + tz2);
       assert_equal ~printer:show_tz (Tez.of_float 2.0) Tez.(tz1 - tz2);
       (* TODO: negative numbers? *)
       assert_equal ~printer:show_fp (FixedPoint.of_string "8.0")     Tez.(tz5 / tz1);
       assert_equal ~printer:show_fp (FixedPoint.of_string "1.7078")  Tez.(tz3 / tz2);
       assert_equal ~printer:show_fp (FixedPoint.of_string "125.025") Tez.(tz4 / tz5);
       assert_equal ~printer:show_tz (Tez.of_float 15.3702) (Tez.scale tz3 fp1)
    )
  ]
