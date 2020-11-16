open OUnit2

type kt = Kit.t [@@deriving show]
type fp = FixedPoint.t [@@deriving show]

let suite =
  "KitTests" >::: [
    "kit arithmetic" >::
    (fun _ ->
       let kt1 = Kit.of_mukit 5_000_000 in
       let kt2 = Kit.of_mukit 3_000_000 in
       let kt3 = Kit.of_mukit 5_123_400 in
       let kt4 = Kit.of_mukit 5_001_000_000 in
       let kt5 = Kit.of_mukit 40_000_000 in
       let kt6 = Kit.of_mukit (-50_309_951) in
       let kt7 = Kit.of_mukit 50_309_951 in
       let fp1 = FixedPoint.of_string "3.0" in
       assert_equal ~printer:show_kt (Kit.of_mukit 8_000_000) Kit.(kt1 + kt2);
       assert_equal ~printer:show_kt (Kit.of_mukit 2_000_000) Kit.(kt1 - kt2);
       (* TODO: negative numbers? *)
       assert_equal ~printer:show_fp (FixedPoint.of_string "8.0")     Kit.(kt5 / kt1);
       assert_equal ~printer:show_fp (FixedPoint.of_string "1.7078")  Kit.(kt3 / kt2);
       assert_equal ~printer:show_fp (FixedPoint.of_string "125.025") Kit.(kt4 / kt5);
       assert_equal ~printer:show_kt (Kit.of_mukit 15_370_200) (Kit.scale kt3 fp1);
       assert_equal ~printer:show_kt kt1 (max kt1 kt2);
       assert_equal ~printer:(fun x -> x) "-50.309951" (show_kt kt6);
       assert_equal ~printer:(fun x -> x) "50.309951" (show_kt kt7);
    )
  ]
