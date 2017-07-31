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
  let old_contrib = U_control.get_contrib_value x in
  let new_contrib = old_contrib +. share *. delta in
  logf "contrib: %g -> %g"
    old_contrib new_contrib;
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

let adjust_contributions_evenly ~delta contributions =
  let n = List.length contributions in
  if n > 0 then (
    let contrib_delta = delta /. float n in
    List.iter
      (fun x ->
         let old_contrib = U_control.get_contrib_value x in
         let new_contrib = old_contrib +. contrib_delta in
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
      logf "contribution to prediction: %g" contrib;
      acc +. contrib
    )
      0. contributions
  in
  let total_weight =
    List.fold_left (fun acc x ->
      let w = U_control.get_weight x in
      logf "weight: %g" w;
      acc +. w
    )
      0. contributions
  in
  let delta = feedback -. prediction in
  logf "feedback: %g, prediction: %g, delta: %g, total_weight: %g"
    feedback prediction delta total_weight;
  if total_weight > 0. && total_weight < infinity then
    adjust_partial_contributions ~delta ~total_weight contributions
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
