
let arb_tez = QCheck.map ~rev:Tez.to_mutez Tez.of_mutez QCheck.(0 -- max_int)

let arb_positive_tez = QCheck.map ~rev:Tez.to_mutez Tez.of_mutez QCheck.(1 -- max_int)

(* somewhere between 0 and 3 tez *)
let arb_small_tez =
  QCheck.map
    ~rev:Tez.to_mutez
    Tez.of_mutez
    QCheck.(1 -- ((max_int / 479_988_656_967) / 4))

let arb_kit = QCheck.map (fun x -> Kit.of_mukit (Z.of_int x)) QCheck.(0 -- max_int)
let arb_positive_kit = QCheck.map (fun x -> Kit.of_mukit (Z.of_int x)) QCheck.(1 -- max_int)

let arb_level = QCheck.map ~rev:Level.to_int Level.of_int QCheck.(0 -- max_int)

let arb_tezos =
  QCheck.map
    (fun level ->
       Tezos.{
         now = Timestamp.of_seconds (level * 60);
         level = Level.of_int level;
         self = Address.of_string "checker";
       }
    )
    QCheck.(0 -- (31556952000 (* a thousand years in seconds *) / 60))
