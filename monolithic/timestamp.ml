type t = int [@@deriving show]
let of_seconds s = s
let to_seconds d = d
let seconds_elapsed ~(start:t) ~(finish:t) = finish - start
let add_seconds t s = t + s
