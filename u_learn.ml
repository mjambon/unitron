(*
   Reinforcement step.
*)

open U_log
open U_system

let adjust_partial_contribution ~delta ~total_weight x =
  let w = U_control.get_weight x in
  let share = w /. total_weight in
  assert (share >= 0.);
  let share =
    (* Hack intended to correct for any initial underestimation
       of the standard deviation and for sudden changes of a contribution
       previously estimated with high certainty. *)
    max 0.001 share
  in
  let new_contrib =
    U_control.(x.last) +. share *. delta
  in
  U_control.update_contrib x new_contrib

(*
   Adjust the value of each contribution, in the case where
   the weight of each contribution is known with enough confidence.
*)
let adjust_partial_contributions ~delta ~total_weight contributions =
  List.iter (fun x ->
    adjust_partial_contribution
      ~delta
      ~total_weight
      x
  ) contributions

let adjust_infinite_contributions ~delta contributions =
  let infinite_contributions =
    List.filter (fun x -> U_control.get_weight x = infinity)
      contributions
  in
  let n = List.length infinite_contributions in
  assert (n > 0);
  let new_contrib = max 0. (delta /. float n) in
  List.iter
    (fun x -> U_control.update_contrib x new_contrib)
    infinite_contributions

let adjust_contributions contributions feedback =
  let prediction =
    List.fold_left (fun acc x -> acc +. U_control.(x.last))
      0. contributions
  in
  let total_weight =
    List.fold_left (fun acc x -> acc +. U_control.get_weight x)
      0. contributions
  in
  let delta = feedback -. prediction in
  logf "feedback: %g, delta: %g, total_weight: %g"
    feedback delta total_weight;
  if total_weight = 0. then
    ()
  else if total_weight > 0. && total_weight < infinity then
    adjust_partial_contributions ~delta ~total_weight contributions
  else if total_weight = infinity then
    adjust_infinite_contributions ~delta contributions
  else
    assert false

let learn (x : U_system.t) (feedback : float) =
  let contributions =
    U_recent_acts.fold x.recent_acts [] (fun age controlid acc ->
      let control = x.get_control controlid in
      let contribution = U_control.(control.contributions).(age) in
      contribution :: acc
    )
  in
  adjust_contributions contributions feedback
