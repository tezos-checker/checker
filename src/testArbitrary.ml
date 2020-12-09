
let arb_level = QCheck.map ~rev:Level.to_int Level.of_int QCheck.(0 -- max_int)

let arb_kit = QCheck.map (fun x -> Kit.of_mukit (Z.of_int x)) QCheck.(0 -- max_int)
let arb_tez = QCheck.map ~rev:Tez.to_mutez Tez.of_mutez QCheck.(0 -- max_int)

let arb_positive_kit = QCheck.map (fun x -> Kit.of_mukit (Z.of_int x)) QCheck.(1 -- max_int)
let arb_positive_tez = QCheck.map ~rev:Tez.to_mutez Tez.of_mutez QCheck.(1 -- max_int)
