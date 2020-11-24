type 'a t =
  { issuer : Address.t;
    amount : int; (* INVARIANT: NON-NEGATIVE *)
    content : 'a;
  }

let create ~issuer ~amount ~content =
  assert (amount >= 0);
  { issuer = issuer;
    amount = amount;
    content = content;
  }

let read ticket = (ticket.issuer, ticket.amount, ticket.content)

let split ticket left right =
  if left + right <> ticket.amount
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
               ~amount:(t1.amount + t2.amount)
               ~content:t1.content)

