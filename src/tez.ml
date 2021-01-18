(* Tez payments *)
type payment = {destination: Ligo.address; amount: Ligo.tez;}
[@@deriving show]
