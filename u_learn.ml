(*
   Reinforcement step.
*)

open U_log
open U_system

(*
   The original weights with which we correct contributions are based
   on the recent standard deviation of those contributions.
   It turns out that if we give more weight to the largest standard
   deviations, it helps contributions to non-noisy effects to converge faster
   when they're mixed with effects associated with noise.

   Strictly increasing, continuous function f from [0,1] to [0,1],
   with the following properties:
     f(0) = 0
     f(1) = 1
     f(x) = x' such that x' < x
     f(x) behaves linearly near 0

   For this, we use a function of the form ax^b + (1-a)x,
   determined empirically.
*)
let reduce_low_relative_weight x =
  0.5 *. x ** 1.3 +. 0.5 *. x

let adjust_relative_weight x =
  assert (x >= 0. && x <= 1.);
  let weight = reduce_low_relative_weight x in
  (* Hack intended to correct for any initial underestimation
     of the standard deviation and for sudden changes of a contribution
     previously estimated with high certainty. *)
  max 0.001 weight

(*
   Express all values as a fraction of the maximum value.
*)
let normalize_max l =
  let m = U_float.maxf l snd in
  List.rev_map (fun (a, b) -> (a, b /. m)) l

(*
   Express all values as a fraction of the total.
*)
let normalize_total l =
  let sum = U_float.sumf l snd in
  List.rev_map (fun (a, b) -> (a, b /. sum)) l

(*
   Adjust the value of each contribution, in the case where
   the weight of each contribution is known with enough confidence.

   The weights are redistributed so as to increase higher weights.
   This is done in two passes:

   1. Normalize the weights with respect to the maximum weight,
      modify them to favor higher weights
   2. Normalize the new weights with respect to the total.
*)
let adjust_partial_contributions ~delta contributions_and_weights =
  let l = normalize_max contributions_and_weights in
  let l = List.rev_map (fun (a, b) -> (a, adjust_relative_weight b)) l in
  let l = normalize_total l in
  List.iter (fun (x, share) ->
    let old_contrib = U_control.get_contrib_value x in
    let new_contrib = old_contrib +. share *. delta in
    if debug then
      logf "contrib: %g -> %g"
        old_contrib new_contrib;
    U_control.update_contrib x new_contrib
  ) l

let adjust_contributions_evenly ~delta contributions =
  let n = List.length contributions in
  if n > 0 then (
    let contrib_delta = delta /. float n in
    List.iter
      (fun x ->
         let old_contrib = U_control.get_contrib_value x in
         let new_contrib = old_contrib +. contrib_delta in
         if debug then
           logf "contrib: %g -> %g"
             old_contrib new_contrib;
         U_control.update_contrib x new_contrib)
      contributions
  )

let adjust_contributions_with_infinite_weight ~delta contributions =
  let infinite_contributions =
    List.filter (fun x -> U_control.get_weight x = infinity)
      contributions
  in
  adjust_contributions_evenly ~delta infinite_contributions

let adjust_contributions contributions feedback =
  let prediction =
    List.fold_left (fun acc x ->
      let contrib = U_control.get_contrib_value x in
      if debug then
        logf "contribution to prediction: %g" contrib;
      acc +. contrib
    )
      0. contributions
  in
  let contributions_and_weights =
    List.rev_map (fun x -> (x, U_control.get_weight x)) contributions
  in
  let total_weight = U_float.sumf contributions_and_weights snd in
  let delta = feedback -. prediction in
  logf "feedback: %g, prediction: %g, delta: %g, total_weight: %g"
    feedback prediction delta total_weight;
  if total_weight > 0. && total_weight < infinity then
    adjust_partial_contributions ~delta contributions_and_weights
  else if total_weight = 0. then
    adjust_contributions_evenly ~delta contributions
  else if total_weight = infinity then
    adjust_contributions_with_infinite_weight ~delta contributions
  else
    assert false

let extract_info feedback contributions =
  let open U_info in
  let info = {
    goal = feedback;
    pos_contrib = 0.;
    neg_contrib = 0.;
    pos_contrib_count = 0;
    neg_contrib_count = 0;
  } in
  let info =
    List.fold_left (fun info contrib ->
      let x = U_control.get_average contrib in
      if x > 0. then
        { info with
          pos_contrib = info.pos_contrib +. x;
          pos_contrib_count = info.pos_contrib_count + 1 }
      else if x < 0. then
        { info with
          neg_contrib = info.neg_contrib +. x;
          neg_contrib_count = info.neg_contrib_count + 1 }
      else
        info
    ) info contributions
  in
  info

let learn (x : U_system.t) (feedback : float) : U_info.t =
  let contributions =
    U_recent_acts.fold x.recent_acts [] (fun age controlid acc ->
      let control = x.get_control controlid in
      let contribution = U_control.(control.contributions).(age) in
      contribution :: acc
    )
  in
  adjust_contributions contributions feedback;
  extract_info feedback contributions
