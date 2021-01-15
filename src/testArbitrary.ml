
let arb_tez = QCheck.map (fun x -> Tez.of_mutez (Ligo.int_from_literal x)) QCheck.(0 -- max_int)

let arb_positive_tez = QCheck.map (fun x -> Tez.of_mutez (Ligo.int_from_literal x)) QCheck.(1 -- max_int)

(* somewhere between 0 and 3 tez *)
let arb_small_tez =
  QCheck.map
    (fun x -> Tez.of_mutez (Ligo.int_from_literal x))
    QCheck.(1 -- ((max_int / 479_988_656_967) / 4))

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
       }
    )
    QCheck.(0 -- (31556952000 (* a thousand years in seconds *) / 60))
