type t = int [@@deriving show]
let of_int l = assert (l >= 0); l
let to_int l = assert (l >= 0); l
let blocks_elapsed ~(start:t) ~(finish:t) = finish - start
let cycle t = t / 4096
