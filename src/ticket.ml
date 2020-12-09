type 'a t =
  { issuer : Address.t;
    amount : Z.t [@printer Z.pp_print]; (* INVARIANT: NON-NEGATIVE *)
    content : 'a;
  }
[@@deriving show]

let create ~issuer ~amount ~content =
  assert (amount >= Z.zero);
  { issuer = issuer;
    amount = amount;
    content = content;
  }

let read ticket = (ticket.issuer, ticket.amount, ticket.content, ticket)

let split ticket left right =
  if Z.(left + right <> ticket.amount)
  then None
  else
    (* NOTE: I hope the content has no tickets in it to duplicate! *)
    let l = create ~issuer:ticket.issuer ~amount:left  ~content:ticket.content in
    let r = create ~issuer:ticket.issuer ~amount:right ~content:ticket.content in
    Some (l, r)

let join t1 t2 =
  if (t1.content <> t2.content) || (t1.issuer <> t2.issuer)
  then None
  else Some (create
               ~issuer:t1.issuer
               ~amount:Z.(t1.amount + t2.amount)
               ~content:t1.content)

