[@inline] let tezos_level: nat =
  [%Michelson ({| { DROP ; LEVEL } |} : unit -> nat) ] ()
