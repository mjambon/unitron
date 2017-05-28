(*
   Reinforcement step.
*)

open U_system

let learn (x : U_system.t) (feedback : float) =
  let contributions =
    U_recent_acts.fold x.recent_acts [] (fun age controlid acc ->
      let control = x.get_control controlid in
      let contribution = U_control.(control.contributions).(age) in
      contribution :: acc
    )
  in
  (* TODO: adjust each contribution based on feedback *)
  ()

