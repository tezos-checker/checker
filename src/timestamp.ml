type t = int [@@deriving show]
let of_seconds s = assert (s >= 0); s
let seconds_elapsed ~(start:t) ~(finish:t) = finish - start
let add_seconds t s = t + s
