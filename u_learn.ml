(*
   Reinforcement step.
*)

open U_log
open U_system

let adjust_partial_contribution ~delta ~total_weight x =
  assert (total_weight > 0.);
  let w = U_control.get_weight x in
  let new_contrib = U_control.(x.last) +. (w /. total_weight) *. delta in
  U_control.update_contrib x (max 0. new_contrib)

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
  if total_weight = 0. || delta = 0. then
    ()
  else if total_weight > 0. && total_weight < infinity then
    List.iter
      (fun x -> adjust_partial_contribution ~delta ~total_weight x)
      contributions
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
