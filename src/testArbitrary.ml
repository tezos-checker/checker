open Kit

let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(0 -- max_int)

let arb_positive_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(1 -- max_int)

(* somewhere between 0 and 3 tez *)
let arb_small_tez =
  QCheck.map
    (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez"))
    QCheck.(1 -- ((max_int / 479_988_656_967) / 4))

let arb_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(0 -- max_int)
let arb_positive_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(1 -- max_int)

let arb_liquidation_slice =
  QCheck.map
    (fun tz ->
       LiquidationAuctionTypes.
         ({ tez = Ligo.tez_from_literal ((string_of_int tz) ^ "mutez")
          ; older = None
          ; younger = None
          ; burrow = Ptr.ptr_null
          ; min_kit_for_unwarranted = kit_zero
          })
    )
    QCheck.(0 -- 1000)
