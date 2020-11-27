type t =
  { now: Timestamp.t;
    level: Level.t;
    self: Address.t; (* NOTE: is of type contract, really, not address *)
    (* ADD MORE ENVIRONMENT VARIABLES HERE AS YOU NEED THEM *)
  }
