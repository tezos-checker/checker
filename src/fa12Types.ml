(** Type of data used when invoking the %transfer entrypoint. *)
type fa12_transfer =
  (* BEGIN_LIGO [@layout:comb] END_LIGO *)
  { (* BEGIN_LIGO [@annot:from] END_LIGO *) address_from : Ligo.address;
    (* BEGIN_LIGO [@annot:to] END_LIGO *) address_to : Ligo.address;
    value : Ligo.nat }
[@@deriving show]
