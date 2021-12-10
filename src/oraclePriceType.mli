open CheckerTypes
open FixedPoint

type oracle_price_type

val get_oracle_entrypoint : external_contracts -> (oracle_price_type Ligo.contract) Ligo.contract
val call_the_oracle : external_contracts -> LigoOp.operation
val oracle_price_to_fixedpoint : oracle_price_type -> fixedpoint
