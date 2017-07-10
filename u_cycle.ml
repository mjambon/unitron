(*
   Definition of one cycle (step) corresponding to one time increment.
*)

open U_log
open U_system

let register_active_controls x t =
  let recent_acts = x.recent_acts in
  U_recent_acts.step recent_acts;
  let add controlid = U_recent_acts.add recent_acts controlid in
  x.read_active_controls t add

(*
   Run each action at most once.
   An action can be triggered by multiple controls but runs at most once
   per cycle.
*)
let run_actions x t =
  let actionids =
    let controlids = U_recent_acts.get_latest x.recent_acts in
    U_set.fold controlids (U_set.create_set ()) (fun controlid acc ->
      let control = x.get_control controlid in
      U_set.add acc U_control.(control.actionid);
      acc
    )
  in
  U_set.iter actionids (fun actionid ->
    let action = x.get_action actionid in
    U_action.(action.func) ()
  )

(*
   Run one cycle at time t.

   1. Activate controls (get their list).
   2. Record active controls (acts) and keep them until they're older
      than some max.
   3. Perform the actions triggered by the controls.
   4. Collect feedback from the goal function.
   5. Decompose feedback as a sum of contributions from all recent acts.
*)
let step (x : U_system.t) t =
  register_active_controls x t;
  run_actions x t;
  let goal = x.goal_function t in
  let info = U_learn.learn x goal in
  U_obs.update x.observables t info

let loop
    ?inner_log_mode
    ?max_iter
    ?stop_condition
    ?(before_step = fun t -> ())
    ?(after_step = fun t -> ())
    system =
  let add_duration, get_mean_duration = U_stat.create_mean_acc () in
  U_loop.run ?inner_log_mode ?max_iter ?stop_condition (fun t ->
    before_step t;
    let (), step_duration =
      U_perf.time (fun () ->
        step system t
      )
    in
    add_duration step_duration;
    after_step t
  );
  let step_duration = get_mean_duration () in
  logf "effective step duration: %.2g ms, %.2g KHz"
    (1e3 *. step_duration) (1. /. (1e3 *. step_duration))
