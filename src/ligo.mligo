[@inline] let add_int_int (i: int) (j: int) : int = i + j
[@inline] let add_tez_tez (i: tez) (j: tez) : tez = i + j

[@inline] let sub_int_int (i: int) (j: int) : int = i - j
[@inline] let sub_tez_tez (i: tez) (j: tez) : tez = i - j

[@inline] let lt_int_int (i: int) (j: int) : bool = i < j
[@inline] let gt_int_int (i: int) (j: int) : bool = i > j
[@inline] let leq_int_int (i: int) (j: int) : bool = i <= j
[@inline] let geq_int_int (i: int) (j: int) : bool = i >= j
