type t =
  { now: Ligo.timestamp;
    level: Level.t;
    self: Ligo.address; (* NOTE: is of type contract, really, not address *)
    (* ADD MORE ENVIRONMENT VARIABLES HERE AS YOU NEED THEM *)
  }
