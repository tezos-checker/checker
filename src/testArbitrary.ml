
let arb_tez = QCheck.map (fun x -> Ligo.tez_from_mutez_literal x) QCheck.(0 -- max_int)

let arb_positive_tez = QCheck.map (fun x -> Ligo.tez_from_mutez_literal x) QCheck.(1 -- max_int)

(* somewhere between 0 and 3 tez *)
let arb_small_tez =
  QCheck.map
    (fun x -> Ligo.tez_from_mutez_literal x)
    QCheck.(1 -- ((max_int / 479_988_656_967) / 4))

let arb_kit = QCheck.map (fun x -> Kit.of_mukit (Ligo.int_from_literal x)) QCheck.(0 -- max_int)

let arb_kit = QCheck.map (fun x -> Kit.of_mukit (Ligo.int_from_literal x)) QCheck.(0 -- max_int)
let arb_positive_kit = QCheck.map (fun x -> Kit.of_mukit (Ligo.int_from_literal x)) QCheck.(1 -- max_int)

let arb_level = QCheck.map ~rev:Level.to_int Level.of_int QCheck.(0 -- max_int)

let arb_tezos =
  QCheck.map
    (fun level ->
       Tezos.{
         now = Ligo.timestamp_from_seconds_literal (level * 60);
         level = Level.of_int level;
         self = Ligo.address_from_literal "checker";
         amount = Ligo.tez_from_mutez_literal 0;
         sender = Ligo.address_from_literal "somebody";
       }
    )
    QCheck.(0 -- (31556952000 (* a thousand years in seconds *) / 60))

let arb_liquidation_slice =
  QCheck.map
    (fun tz ->
       LiquidationAuctionTypes.
         ({ tez = Ligo.tez_from_mutez_literal tz
          ; older = None
          ; younger = None
          ; burrow = Ptr.ptr_null
          ; min_kit_for_unwarranted = Kit.zero
          })
    )
    QCheck.(0 -- 1000)
