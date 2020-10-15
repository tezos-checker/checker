type t = unit
type kit_utxo = unit
let touch x = x
let invalidate_all_permissions = failwith "not_implemented"
let make_permission = failwith "not_implemented"
let set_allow_all_kit_burns = failwith "not_implemented"
let set_allow_all_tez_deposits = failwith "not_implemented"
let set_delegate = failwith "not_implemented"
let withdraw = failwith "not_implemented"
let deposit = failwith "not_implemented"
let default = failwith "not_implemented"
let allow_all_tez = failwith "foo"
type rights = Admin
            | User of
                { deposit_tez : bool
                ; withdraw_tez : bool
                ; mint_kit : bool
                ; burn_kit : bool
                ; set_delegate : bool
                }
type permission = rights Tezos.ticket
