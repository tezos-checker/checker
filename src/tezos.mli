type t =
  { now: Ligo.timestamp;
    level: Level.t;
    self: Ligo.address; (* NOTE: is of type contract, really, not address *)
    sender: Ligo.address;
    amount: Ligo.tez;
  }

(*
Ticket-based entitities in checker and their expected value/mechanics:

| Ticket           | Multiplicity | (Usably) Splittable              |
|------------------|--------------|----------------------------------|
| liquidity        | non-negative | Yes (finitely, zero is useless)  |
| kit              | non-negative | Yes (finitely, zero is useless)  |
| permission       | always zero  | Yes (infinitely, always zero)    |
| del. auction bid | always one   | No  (zero is useless)            |
| col. auction bid | always one   | No  (zero is useless)            |
*)

(* Tickets are a way for smart-contracts to authenticate data with respect to a
 * Tezos address. This authentication can then be used to build composable
 * permission systems.

   Operations on tickets
   ~~~~~~~~~~~~~~~~~~~~~

   The following operations deal with tickets.

   A contract can create a ticket from a value and an amount. The ticket, when
   inspected reveals the value, the amount, and the address of the ticketer (the
   contract that created the ticket). It is impossible for a contract to "forge" a
   ticket that appears to have been created by another ticketer. The amount is a
   meta-data that can be used to implement UTXOs. Tickets cannot be duplicated
   using the ``DUP`` instruction.

   For example, a ticket could represent a Non Fungible Token (NFT) or a Unspent
   Transaction Output (UTXO) which can then be passed around and behave like a
   value.  This process can happen without the need to interact with a centralized
   NFT contract, simplifying the code.

   - ``TICKET`` :: 'a : nat : 'S -> ticket 'a : 'S

   Create a ticket with the given content and amount. The ticketer is the
   address of `SELF`. Type ``'a`` must be comparable (the ``COMPARE`` primitive
   must be defined over it).

   - ``READ_TICKET`` :: ticket 'a : 'S -> pair address 'a nat : ticket 'a : 'S

   Retrieve the information stored in a ticket. Also return the ticket.

   - ``SPLIT_TICKET`` :: ticket 'a : (pair nat nat) : 'S -> option (pair (ticket 'a) (ticket 'a)) : 'S

   Delete the given ticket and create two tickets with the same content and
   ticketer as the original, but with the new provided amounts.  (This can be
   used to easily implement UTXOs.) Return None iff the ticket's original amount
   is not equal to the sum of the provided amounts.

   - ``JOIN_TICKETS`` :: (pair (ticket 'a) (ticket 'a)) : 'S -> option (ticket 'a) : 'S

   The inverse of ``SPLIT_TICKET``. Delete the given tickets and create a ticket
   with an amount equal to the sum of the amounts of the input tickets.  (This
   can be used to consolidate UTXOs.) Return None iff the input tickets have a
   different ticketer or content.
*)

type 'a ticket

val create_ticket : t -> 'a -> Ligo.nat -> 'a ticket
val read_ticket : 'a ticket -> (Ligo.address * 'a * Ligo.nat) * 'a ticket
val split_ticket : 'a ticket -> (Ligo.nat * Ligo.nat) -> ('a ticket * 'a ticket) option
val join_tickets : 'a ticket -> 'a ticket -> ('a ticket) option

(* BEGIN_OCAML *)
val show_ticket : (Format.formatter -> 'a -> unit) -> 'a ticket -> string
val pp_ticket : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a ticket -> unit
