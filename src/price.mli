open CfmmTypes
open Common
open CheckerTypes

val calculate_kit_in_tok : cfmm -> ratio option -> external_contracts -> ratio * LigoOp.operation list
