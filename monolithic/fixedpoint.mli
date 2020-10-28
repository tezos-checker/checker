module type BASE = sig val d : Z.t end

module FixedPoint (Base : BASE) : sig
  type t
  val ( + ) : t -> t -> t
  val ( ~- ) : t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end = struct
  type t = Z.t
  let ( + ) a b = Z.(a + b)
  let ( ~- ) a = Z.(- a)
  let ( - ) a b = Z.(a - b)
  (* We round towards 0, for fixedpoint calculation, measuring things which are
     inherently noisy, this is ok. Greater care must be excerced when doing 
     accounting (e.g. uniswap)... for measuring things like drift, targets,
     imbalances etc which are naturally imprecise this is fine. *)
  let ( * ) a b = Z.((a * b) / B.d)
  let ( / ) a b = Z.((a * B.d) / b)                     
end
