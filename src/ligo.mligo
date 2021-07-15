[@inline] let add_int_int (i: int) (j: int) : int = i + j
[@inline] let add_nat_nat (i: nat) (j: nat) : nat = i + j
[@inline] let add_tez_tez (i: tez) (j: tez) : tez = i + j

[@inline] let sub_int_int (i: int) (j: int) : int = i - j
[@inline] let sub_tez_tez (i: tez) (j: tez) : tez = i - j
[@inline] let sub_nat_nat (i: nat) (j: nat) : int = i - j
[@inline] let sub_timestamp_timestamp (i: timestamp) (j: timestamp) : int = i - j

[@inline] let mul_int_int (i: int) (j: int) : int = i * j
[@inline] let mul_nat_tez (i: nat) (j: tez) : tez = i * j
[@inline] let mul_int_nat (i: int) (j: nat) : int = i * j
[@inline] let mul_nat_int (i: nat) (j: int) : int = i * j
[@inline] let mul_nat_nat (i: nat) (j: nat) : nat = i * j

[@inline] let div_int_int (i: int) (j: int) : int = i / j
[@inline] let div_nat_nat (i: nat) (j: nat) : nat = i / j
[@inline] let div_tez_tez (i: tez) (j: tez) : nat = i / j

[@inline] let mod_int_int (i: int) (j: int) : nat = i mod j

[@inline] let ediv_tez_nat (i: tez) (j: nat) : (tez * tez) option = ediv i j
[@inline] let ediv_int_int (i: int) (j: int) : (int * nat) option = ediv i j
[@inline] let ediv_nat_nat (i: nat) (j: nat) : (nat * nat) option = ediv i j

[@inline] let ne_nat_nat (i: nat) (j: nat) : bool = i <> j

[@inline] let eq_int_int (i: int) (j: int) : bool = i = j
[@inline] let eq_nat_nat (i: nat) (j: nat) : bool = i = j
[@inline] let eq_tez_tez (i: tez) (j: tez) : bool = i = j

[@inline] let lt_int_int (i: int) (j: int) : bool = i < j
[@inline] let lt_nat_nat (i: nat) (j: nat) : bool = i < j
[@inline] let lt_tez_tez (i: tez) (j: tez) : bool = i < j

[@inline] let gt_int_int (i: int) (j: int) : bool = i > j
[@inline] let gt_nat_nat (i: nat) (j: nat) : bool = i > j
[@inline] let gt_tez_tez (i: tez) (j: tez) : bool = i > j

[@inline] let leq_int_int (i: int) (j: int) : bool = i <= j
[@inline] let leq_nat_nat (i: nat) (j: nat) : bool = i <= j
[@inline] let leq_tez_tez (i: tez) (j: tez) : bool = i <= j

[@inline] let geq_int_int (i: int) (j: int) : bool = i >= j
[@inline] let geq_nat_nat (i: nat) (j: nat) : bool = i >= j
[@inline] let geq_tez_tez (i: tez) (j: tez) : bool = i >= j
[@inline] let geq_timestamp_timestamp (i: timestamp) (j: timestamp) : bool = i >= j
