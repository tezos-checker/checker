type 'a t =
  { issuer : Ligo.address;
    content : 'a;
    amount : Ligo.nat;
  }
[@@deriving show]

let create (tezos:Tezos.t) content amount =
  { issuer = tezos.self;
    content = content;
    amount = amount;
  }

let read ticket = ((ticket.issuer, ticket.content, ticket.amount), ticket)

let split ticket (left, right) =
  if (Ligo.add_nat_nat left right) <> ticket.amount
  then None
  else
    (* NOTE: I hope the content has no tickets in it to duplicate! *)
    let l = {issuer = ticket.issuer; content = ticket.content; amount = left;} in
    let r = {issuer = ticket.issuer; content = ticket.content; amount = right;} in
    Some (l, r)

let join t1 t2 =
  if (t1.content <> t2.content) || (t1.issuer <> t2.issuer)
  then None
  else Some {issuer = t1.issuer; content = t1.content; amount = Ligo.add_nat_nat t1.amount t2.amount;}
