open Ctez
open Kit
open Lqt

let arb_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(0 -- max_int)

let arb_positive_tez = QCheck.map (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez")) QCheck.(1 -- max_int)

let arb_positive_ctez = QCheck.map (fun x -> ctez_of_muctez (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(1 -- max_int)

let arb_address = QCheck.map Ligo.address_of_string QCheck.(string_of_size (Gen.return 36))

let arb_small_tez =
  QCheck.map
    (fun x -> Ligo.tez_from_literal ((string_of_int x) ^ "mutez"))
    QCheck.(1 -- 2_401_976)

let arb_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(0 -- max_int)
let arb_positive_kit = QCheck.map (fun x -> kit_of_mukit (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(1 -- max_int)

let arb_lqt = QCheck.map (fun x -> lqt_of_denomination (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(0 -- max_int)
let arb_positive_lqt = QCheck.map (fun x -> lqt_of_denomination (Ligo.nat_from_literal (string_of_int x ^ "n"))) QCheck.(1 -- max_int)

let arb_nat = QCheck.map (fun x -> Ligo.nat_from_literal ((string_of_int x) ^ "n")) QCheck.(0 -- max_int)

let arb_small_nat =
  print_string ("\n" ^ string_of_int max_int ^ "\n");
  QCheck.map
    (fun x -> Ligo.nat_from_literal ((string_of_int x) ^ "n"))
    QCheck.(1 -- 2_401_976)

let arb_liquidation_slice_contents =
  QCheck.map
    (fun tz ->
       LiquidationAuctionPrimitiveTypes.
         ({ tez = Ligo.tez_from_literal ((string_of_int tz) ^ "mutez")
          ; burrow = (Ligo.address_of_string "", Ligo.nat_from_literal "0n")
          ; min_kit_for_unwarranted = Some kit_zero
          })
    )
    QCheck.(0 -- 1000)

let arb_liquidation_slice =
  QCheck.map
    (fun sl ->
       LiquidationAuctionPrimitiveTypes.
         ({ contents = sl
          ; older = None
          ; younger = None
          })
    )
    arb_liquidation_slice_contents
