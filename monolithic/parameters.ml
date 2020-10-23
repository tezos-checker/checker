open FixedPoint
open Kit
open Tez

(* ************************************************************************* *)
(*                               Parameters                                  *)
(* ************************************************************************* *)
module Parameters : sig
  type parameters =
    { (* TODO: Perhaps maintain 1/q instead of q? TBD *)
      q : FixedPoint.t [@printer FixedPoint.pp]; (* 1/kit, really *)
      index: Tez.t [@printer Tez.pp];
      protected_index: Tez.t [@printer Tez.pp];
      target: FixedPoint.t [@printer FixedPoint.pp];
      drift': FixedPoint.t [@printer FixedPoint.pp];
      drift: FixedPoint.t [@printer FixedPoint.pp];
      (* TODO: Add the imbalance adjustment index also *)
    }

  val show_parameters : parameters -> string
  val pp_parameters : Format.formatter -> parameters -> unit

  (** Current minting price. *)
  val minting_price : parameters -> FixedPoint.t

  (** Current liquidation price. *)
  val liquidation_price : parameters -> FixedPoint.t

  (** Given the amount of kit necessary to close all existing burrows
    * (burrowed) and the amount of kit that are currently in circulation,
    * compute the current imbalance adjustment (can be either a fee or a
    * bonus). *)
  val compute_imbalance : Kit.t -> Kit.t -> FixedPoint.t

  (** Given the current target p, calculate the rate of change of the drift d'.
    * TODO: Use FixedPoint.t instead of float. *)
  val compute_drift_derivative : float -> float
  val compute_drift_derivative_2 : FixedPoint.t -> FixedPoint.t
end =
struct
  type parameters =
    { q : FixedPoint.t [@printer FixedPoint.pp]; (* 1/kit, really *)
      index: Tez.t [@printer Tez.pp];
      protected_index: Tez.t [@printer Tez.pp];
      target: FixedPoint.t [@printer FixedPoint.pp];
      drift': FixedPoint.t [@printer FixedPoint.pp];
      drift: FixedPoint.t [@printer FixedPoint.pp];
    }
  [@@deriving show]

  (* tez. To get tez/kit must multiply with q. *)
  let tz_minting (p: parameters) : Tez.t =
    max p.index p.protected_index

  (* tez. To get tez/kit must multiply with q. *)
  let tz_liquidation (p: parameters) : Tez.t =
    min p.index p.protected_index

  let minting_price (p: parameters) : FixedPoint.t =
    FixedPoint.(p.q * Tez.to_fp (tz_minting p))

  let liquidation_price (p: parameters) : FixedPoint.t =
    FixedPoint.(p.q * Tez.to_fp (tz_liquidation p))

  let cnp (i: FixedPoint.t) : FixedPoint.t = FixedPoint.(i / of_float 100.0)

  (* TODO: Eventually get rid of? *)
  let sign (i: float) : float =
    if i > 0. then 1.
    else if i = 0. then 0.
    else -1.

  (** If we call burrowed the total amount of kit necessary to close all existing
    * burrows, and minted the total amount of kit in circulation, then the
    * imbalance fee/bonus is calculated as follows (per year):
    *
    *   min(   5 * burrowed, (burrowed - minted) ) * 1.0 cNp / burrowed , if burrowed >= minted
    *   max( - 5 * burrowed, (burrowed - minted) ) * 1.0 cNp / burrowed , otherwise
  *)
  let compute_imbalance (burrowed: Kit.t) (minted: Kit.t) : FixedPoint.t =
    assert (burrowed >= Kit.zero); (* Invariant *)
    assert (minted >= Kit.zero); (* Invariant *)
    let centinepers = cnp (FixedPoint.of_float 0.1) in (* TODO: per year! *)
    let burrowed_fivefold = Kit.scale burrowed (FixedPoint.of_float 5.0) in
    (* No kit in burrows or in circulation means no imbalance adjustment *)
    if burrowed = Kit.zero then
      (assert (minted = Kit.zero); FixedPoint.zero) (* George: Is it possible to have minted kit in circulation when nothing is burrowed? *)
    else if burrowed = minted then
      FixedPoint.zero (* George: I add this special case, to avoid rounding issues *)
    else if burrowed >= minted then
      Kit.div (Kit.scale (min burrowed_fivefold (Kit.sub burrowed minted)) centinepers) burrowed
    else
      FixedPoint.neg (Kit.div (Kit.scale (min burrowed_fivefold (Kit.sub minted burrowed)) centinepers) burrowed)

  (* Utku: Thresholds here are cnp / day^2, we should convert them to cnp /
   * second^2, assuming we're measuring time in seconds. My calculations might be
   * incorrect. *)
  let compute_drift_derivative (target : float) : float =
    assert (target > 0.);
    let cnp_001 = FixedPoint.to_float (cnp (FixedPoint.of_float 0.01)) in
    let cnp_005 = FixedPoint.to_float (cnp (FixedPoint.of_float 0.05)) in

    let log_target = log target in
    let abs_log_target = Float.abs log_target in
    if abs_log_target < FixedPoint.to_float (cnp (FixedPoint.of_float 0.5)) then
      0.
    else if abs_log_target < FixedPoint.to_float (cnp (FixedPoint.of_float 5.0)) then
      sign log_target *. (cnp_001 /. (24. *. 3600.) ** 2.)
    else
      sign log_target *. (cnp_005 /. (24. *. 3600.) ** 2.)

  (* George: Note that we don't really need to calculate the logs here (which can
   * be lossy); we can instead exponentiate the whole equation (exp is monotonic)
   * and win some precision, like this:
  *)
  let compute_drift_derivative_2 (target : FixedPoint.t) : FixedPoint.t =
    assert (target > FixedPoint.zero);
    FixedPoint.(
      let cnp_001 = cnp (of_float 0.01) in
      let cnp_005 = cnp (of_float 0.05) in
      let secs_in_a_day = of_float (24. *. 3600.) in
      match () with
      (* No acceleration (0) *)
      | () when exp (of_float (-. 0.5 /. 100.)) < target && target < exp (of_float (0.5 /. 100.)) -> zero
      (* Low acceleration (-/+) *)
      | () when exp (of_float (-. 5.0 /. 100.)) < target && target <= exp (of_float (-. 0.5 /. 100.)) -> neg (cnp_001 / sqr secs_in_a_day)
      | () when exp (of_float    (5.0 /. 100.)) > target && target >= exp (of_float    (0.5 /. 100.)) ->     (cnp_001 / sqr secs_in_a_day)
      (* High acceleration (-/+) *)
      | () when target <= exp (of_float (-. 5.0 /. 100.)) -> neg (cnp_005 / sqr secs_in_a_day)
      | () when target >= exp (of_float    (5.0 /. 100.)) ->     (cnp_005 / sqr secs_in_a_day)
      | _ -> failwith "impossible"
    )
end

