type state =
  { price: (nat * nat);
    owner: address;
  }

type params =
  | Update of (nat * nat)
  | GetPrice of (nat * nat) contract

let main (op, state: params * state): operation list * state =
  match op with
  | GetPrice cb ->
    let op = Tezos.transaction state.price 0mutez cb in
    ([op], state)
  | Update new_price ->
    if Tezos.sender = state.owner
    then (([]: operation list), {state with price = new_price })
    else failwith "unauthorized"
