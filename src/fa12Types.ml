(** Type of data used when invoking the %transfer entrypoint. *)
type transfer =
  { address_from : Ligo.address;
    address_to : Ligo.address;
    value : Ligo.nat;
  }
[@@deriving show]
