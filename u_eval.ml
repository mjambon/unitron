(*
   A very simple setup to make sure we don't have a major bug
   and to evaluate the behavior of the system in different conditions.

   Not tested at this point:
   - any number of actions other than 2
   - multiple controls for the same action
   - delayed contributions
*)

open Printf
open U_log

let default_global_iter = 100
let default_max_iter = 100
let default_window_length = 10

let default_base_contrib_a0 = 1.
let default_base_contrib_a1 = -0.5
let default_base_contrib_a2 = 0.25

let default_base_contrib_b0 = 0.1
let default_base_contrib_b1 = 0.2
let default_base_contrib_b2 = 0.05

let default_tolerance_a = 0.05
let default_tolerance_b = 0.05
let default_max_stdev_a = 0.05
let default_max_stdev_b = 0.05
let default_determine_actions_ab t = (U_random.pick 0.5, U_random.pick 0.5)

let print_controls controls =
  U_set.iter_ordered controls (fun x ->
    logf "%s" (U_control.to_info x)
  )

let get_average_contributions window_length acc =
  let stat =
    Array.init window_length (fun age ->
      U_stat.create_stdev_acc (), U_stat.create_stdev_acc ()
    )
  in
  List.iter (fun control ->
    U_control.iter_contributions control (fun ~age ~average ~stdev ->
      let (add1, _, _), (add2, _, _) = stat.(age) in
      add1 average;
      add2 stdev
    )
  ) acc;

  Array.map (fun ((_, _, get_avg_stat), (_, _, get_stdev_stat)) ->
    get_avg_stat (), get_stdev_stat ()
  ) stat

let print_contrib_stats controlid contrib_stat_array =
  Array.iteri (fun age ((avg_mean, avg_stdev), (stdev_mean, stdev_stdev)) ->
    logf "contribution %s[%i]: avg:(%.2g, %.2g) stdev:(%.2g, %.2g)"
      (U_controlid.to_string controlid) age
      avg_mean avg_stdev stdev_mean stdev_stdev
  ) contrib_stat_array

let print_observables system t =
  let x = U_obs.get U_system.(system.observables) t in
  logf "observables: %s" (U_obs.to_string x)

let create_delayed_effect_manager () =
  let scheduled_contributions = ref [] in
  let add_action future_contributions =
    scheduled_contributions :=
      future_contributions :: !scheduled_contributions
  in
  let pop_effects t =
    let current =
      List.fold_left (fun acc l ->
        match l with
        | current :: _ -> acc +. current
        | [] -> acc
      ) 0. !scheduled_contributions
    in
    let future =
      List.filter ((<>) [])
        (List.map (function [] -> [] | _ :: l -> l) !scheduled_contributions)
    in
    scheduled_contributions := future;
    current
  in
  add_action, U_lazy.get pop_effects

let test_system_once
  ?inner_log_mode
  ~create_experiment
  ~window_length
  ~controlid_a
  ~controlid_b
  ~base_contrib_a0
  ~base_contrib_a1
  ~base_contrib_a2
  ~base_contrib_b0
  ~base_contrib_b1
  ~base_contrib_b2
  ?(noise_a = fun t -> 0.)
  ?(noise_b = fun t -> 0.)
  ?(noise = fun t -> 0.)
  ?(determine_actions_ab = default_determine_actions_ab)
  () =

  let moving_avg_cst = 0.5 (* seems high but works well *) in

  let controls = U_control.create_set () in
  let actions = U_action.create_set () in
  let add_control id actionid =
    U_control.add ~moving_avg_cst ~window_length ~id ~actionid controls
  in

  (* A has its own frequency and constant contribution. *)
  let a_was_active = ref false in
  let actionid_a = U_actionid.of_string "A" in
  U_action.add actionid_a (fun () -> a_was_active := true) actions;

  add_control controlid_a actionid_a;

  (* B has its own frequency and constant contribution, independent from A. *)
  let b_was_active = ref false in
  let actionid_b = U_actionid.of_string "B" in
  U_action.add actionid_b (fun () -> b_was_active := true) actions;

  add_control controlid_b actionid_b;

  let before_step t =
    logf "--------------------------------------------------------------";
    a_was_active := false;
    b_was_active := false
  in

  let add_action, pop_effects = create_delayed_effect_manager () in

  let read_active_controls t add =
    let a, b = determine_actions_ab t in
    if a then (
      logf "A*";
      add controlid_a;
      let extra = noise_a t in
      add_action [base_contrib_a0 +. extra;
                  base_contrib_a1 +. extra;
                  base_contrib_a2 +. extra];
    );
    if b then (
      logf "B*";
      add controlid_b;
      let extra = noise_b t in
      add_action [base_contrib_b0 +. extra;
                  base_contrib_b1 +. extra;
                  base_contrib_b2 +. extra];
    )
  in

  let goal_function t =
    pop_effects t +. noise t
  in

  let get_control id =
    U_control.get controls id
  in
  let get_action id =
    U_action.get actions id
  in

  let get_controls () =
    get_control controlid_a, get_control controlid_b
  in

  let system =
    U_system.create
      ~window_length
      ~goal_function
      ~read_active_controls
      ~get_control
      ~get_action
  in
  let experiment = create_experiment get_controls in
  let after_step t =
    print_controls controls;
    print_observables system t;
    let stop = U_exp.stop_condition experiment system t in
    not stop
  in
  U_cycle.loop
    ?inner_log_mode
    ~before_step
    ~after_step
    system;

  let control_a, control_b = get_controls () in
  (experiment, control_a, control_b)

(*
   We run the system from t=0 several times in order to get a sense
   of the variability of the results.
*)
let test_system
    ~name
    ~create_experiment
    ~base_contrib_a0
    ~base_contrib_a1
    ~base_contrib_a2
    ~base_contrib_b0
    ~base_contrib_b1
    ~base_contrib_b2
    ?(global_iter = default_global_iter)
    ?(window_length = default_window_length)
    ?noise_a
    ?noise_b
    ?noise
    ?determine_actions_ab
    () =

  let controlid_a = U_controlid.of_string "A" in
  let controlid_b = U_controlid.of_string "B" in
  let acc_exp = ref [] in
  let acc_a = ref [] in
  let acc_b = ref [] in
  for i = 1 to global_iter do
    let inner_log_mode =
      if i = 1 then `Skip
      else `Off
    in
    logf "--- Run %i/%i ---" i global_iter;
    let experiment, control_a, control_b =
      test_system_once
        ~inner_log_mode
        ~create_experiment
        ~window_length
        ~controlid_a
        ~controlid_b
        ~base_contrib_a0
        ~base_contrib_a1
        ~base_contrib_a2
        ~base_contrib_b0
        ~base_contrib_b1
        ~base_contrib_b2
        ?noise_a
        ?noise_b
        ?noise
        ?determine_actions_ab
        ()
    in
    acc_exp := experiment :: !acc_exp;
    acc_a := control_a :: !acc_a;
    acc_b := control_b :: !acc_b;
  done;
  let contrib_stat_a = get_average_contributions window_length !acc_a in
  let contrib_stat_b = get_average_contributions window_length !acc_b in
  print_contrib_stats controlid_a contrib_stat_a;
  print_contrib_stats controlid_b contrib_stat_b;
  let exp_report = U_exp.make_report !acc_exp in
  U_exp.print_report exp_report;
  true

let create_default_goals
    ?(tolerance_a = default_tolerance_a)
    ?(tolerance_b = default_tolerance_b)
    ?(max_stdev_a = default_max_stdev_a)
    ?(max_stdev_b = default_max_stdev_b)
    ~base_contrib_a0
    ~base_contrib_b0
    get_controls =
  let cond_a0 system t =
    let control_a, control_b = get_controls () in
    let contrib_a0 = U_control.get_contribution control_a 0 in
    let avg = U_control.get_average contrib_a0 in
    let stdev = U_control.get_stdev contrib_a0 in
    stdev <= max_stdev_a
    && abs_float (avg -. base_contrib_a0) <= tolerance_a
  in
  let cond_b0 system t =
    let control_a, control_b = get_controls () in
    let contrib_b0 = U_control.get_contribution control_b 0 in
    let avg = U_control.get_average contrib_b0 in
    let stdev = U_control.get_stdev contrib_b0 in
    stdev <= max_stdev_b
    && abs_float (avg -. base_contrib_b0) <= tolerance_b
  in
  let goal_a = U_exp.create_goal "a0" cond_a0 in
  let goal_b = U_exp.create_goal "b0" cond_b0 in
  [goal_a; goal_b]

let make_create_experiment
    ~base_contrib_a0
    ~base_contrib_b0
    ?tolerance_a
    ?tolerance_b
    ?max_stdev_a
    ?max_stdev_b
    ?(create_extra_goals = fun get_controls -> [])
    name =
  fun get_controls ->
    let base_goals =
      create_default_goals
        ~base_contrib_a0
        ~base_contrib_b0
        ?max_stdev_a
        ?max_stdev_b
        get_controls
    in
    let extra_goals = create_extra_goals get_controls in
    U_exp.create_experiment name (base_goals @ extra_goals)

let make_test
    ?window_length
    ?(base_contrib_a0 = default_base_contrib_a0)
    ?(base_contrib_a1 = default_base_contrib_a1)
    ?(base_contrib_a2 = default_base_contrib_a2)
    ?(base_contrib_b0 = default_base_contrib_b0)
    ?(base_contrib_b1 = default_base_contrib_b1)
    ?(base_contrib_b2 = default_base_contrib_b2)
    ?tolerance_a
    ?tolerance_b
    ?max_stdev_a
    ?max_stdev_b
    ?noise_a
    ?noise_b
    ?noise
    ?determine_actions_ab
    ~name
    () =
  let create_experiment =
    make_create_experiment
      ~base_contrib_a0
      ~base_contrib_b0
      ?tolerance_a
      ?tolerance_b
      ?max_stdev_a
      ?max_stdev_b
      name
  in
  test_system
    ~name
    ~create_experiment
    ~base_contrib_a0
    ~base_contrib_a1
    ~base_contrib_a2
    ~base_contrib_b0
    ~base_contrib_b1
    ~base_contrib_b2
    ?window_length
    ?noise_a
    ?noise_b
    ?noise
    ?determine_actions_ab
    ()

let test_default () =
  make_test
    ~name: "default"
    ()

let test_shortest_window () =
  make_test
    ~name: "shortest_window"
    ~window_length: 1
    ~base_contrib_a1: 0.
    ~base_contrib_a2: 0.
    ~base_contrib_b1: 0.
    ~base_contrib_b2: 0.
    ()

let test_negative () =
  make_test
    ~base_contrib_b0: (-0.1)
    ~name: "negative"
    ()

let test_large_difference () =
  make_test
    ~name: "large_difference"
    ~base_contrib_a0: 100.
    ()

let test_large_difference_corrected () =
  (* hardcoded ratio of max gap between expected contributions,
     from default setup to this setup *)
  let r = (100. -. (-0.5)) /. (1. -. (-0.5)) in
  let tolerance_a = r *. default_tolerance_a in
  let tolerance_b = r *. default_tolerance_b in
  let max_stdev_a = r *. default_max_stdev_a in
  let max_stdev_b = r *. default_max_stdev_b in
  make_test
    ~name: "large_difference_corrected"
    ~base_contrib_a0: 100.
    ~tolerance_a
    ~tolerance_b
    ~max_stdev_a
    ~max_stdev_b
    ()

(* B active => A active *)
let test_subaction () =
  let determine_actions_ab t =
    let a = U_random.pick 0.5 in
    let b = a && U_random.pick 0.5 in
    a, b
  in
  make_test
    ~name: "subaction"
    ~determine_actions_ab
    ()

let test_global_noise () =
  assert (default_base_contrib_a0 = 1.);
  assert (default_base_contrib_b0 = 0.1);
  let noise t =
    U_random.normal ~stdev:0.08 ()
  in
  make_test
    ~name: "global_noise"
    ~noise
    ()

(* same parameters as noisy_contribution below, without the noise. *)
let test_nonnoisy_contribution () =
  assert (default_base_contrib_a0 = 1.);
  make_test
    ~name: "nonnoisy_contribution"
    ~max_stdev_a: 0.5
    ()

let test_noisy_contribution () =
  assert (default_base_contrib_a0 = 1.);
  make_test
    ~name: "noisy_contribution"
    ~noise_a:(fun _ ->
      U_random.normal ~stdev: 0.5 ()
    )
    ~max_stdev_a: 0.5
    ()

let test_noisy_contributions () =
  assert (default_base_contrib_a0 = 1.);
  assert (default_base_contrib_b0 = 0.1);
  let noise_a t =
    U_random.normal ~stdev:0.4 ()
  in
  let noise_b t =
    U_random.normal ~stdev:0.04 ()
  in
  make_test
    ~name: "noisy_contributions"
    ~tolerance_a: 0.1
    ~tolerance_b: 0.08
    ~max_stdev_a: 0.5
    ~max_stdev_b: 0.05
    ~noise_a
    ~noise_b
    ()

(*
   Change the contributions of A and B suddenly, and see if we can adjust
   the predictions.
*)
let test_adaptation () =
  assert (default_base_contrib_a0 = 1.);
  assert (default_base_contrib_b0 = 0.1);
  let noise_a t =
    if t < 100 then 1.
    else 0.
  in
  let noise_b t =
    if t < 50 then (-0.1)
    else 0.
  in
  make_test
    ~name: "adaptation"
    ~noise_a
    ~noise_b
    ~determine_actions_ab: (fun t -> U_random.pick 0.5, U_random.pick 0.5)
    ()

let tests = [
  "default", test_default;
  "shortest_window", test_shortest_window;
  "negative", test_negative;
  "large difference", test_large_difference;
  "large difference_corrected", test_large_difference_corrected;
  "subaction", test_subaction;
  "global noise", test_global_noise;
  "non-noisy contribution", test_nonnoisy_contribution;
  "noisy contribution", test_noisy_contribution;
  "noisy contributions", test_noisy_contributions;
  "adaptation", test_adaptation;
]
