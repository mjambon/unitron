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
let default_window_length = 5

(* We give up with an error after this many steps *)
let max_iter = 100_000

let default_base_contrib_a0 = 1.
let default_base_contrib_a1 = -0.5
let default_base_contrib_a2 = 0.25

let default_base_contrib_b0 = 0.1
let default_base_contrib_b1 = 0.2
let default_base_contrib_b2 = 0.05

let default_epsilon_a0 = 0.05
let default_epsilon_b0 = 0.005

let default_determine_actions_ab t = (U_random.pick 0.5, U_random.pick 0.5)
let determine_actions_always_b t = (U_random.pick 0.5, true)

let print_control oc x =
  logf "%s" (U_control.to_info x);
  U_control.print_csv oc x

let get_average_contributions window_length acc =
  let stat =
    Array.init window_length (fun age ->
      U_stat.create_stdev_acc (), U_stat.create_stdev_acc ()
    )
  in
  List.iter (fun control ->
    U_control.iter_contributions control (fun ~age ~contrib ~average ~stdev ->
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

let csv_dir = "out"

let get_csv_dir () =
  if not (Sys.file_exists csv_dir) then
    Unix.mkdir csv_dir 0o777;
  csv_dir

let get_csv_filename name =
  sprintf "%s/%s.csv"
    (get_csv_dir ()) name

let test_system_once
  ?inner_log_mode
  ~name
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

  let controls = U_control.create_set () in
  let actions = U_action.create_set () in
  let add_control id actionid =
    U_control.add ~window_length ~id ~actionid controls
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
  let oc_a =
    U_control.open_csv window_length (get_csv_filename (name ^ "-a")) in
  let oc_b =
    U_control.open_csv window_length (get_csv_filename (name ^ "-b")) in

  let after_step t =
    let control_a, control_b = get_controls () in
    print_control oc_a control_a;
    print_control oc_b control_b;
    print_observables system t;
    let stop = U_exp.stop_condition experiment system t in
    let continue = not stop in
    if continue && t >= max_iter then (
      eprintf "> %s ERROR: too many iterations (%i)\n%!" name max_iter;
      failwith "Too many iterations"
    );
    continue
  in
  U_cycle.loop
    ?inner_log_mode
    ~before_step
    ~after_step
    system;

  close_out oc_a;
  close_out oc_b;
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
        ~name: (sprintf "%s-%i" name i)
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
    ?(epsilon_a0 = default_epsilon_a0)
    ?(epsilon_b0 = default_epsilon_b0)
    ~base_contrib_a0
    ~base_contrib_b0
    get_controls =
  assert (epsilon_a0 > 0.);
  assert (epsilon_b0 > 0.);
  let cond_a0 system t =
    let control_a, control_b = get_controls () in
    let contrib_a0 = U_control.get_contribution control_a 0 in
    let avg = U_control.get_average contrib_a0 in
    abs_float (avg -. base_contrib_a0) <= epsilon_a0
  in
  let cond_b0 system t =
    let control_a, control_b = get_controls () in
    let contrib_b0 = U_control.get_contribution control_b 0 in
    let avg = U_control.get_average contrib_b0 in
    abs_float (avg -. base_contrib_b0) <= epsilon_b0
  in
  let goal_a = U_exp.create_goal "a0" cond_a0 in
  let goal_b = U_exp.create_goal "b0" cond_b0 in
  [goal_a; goal_b]

let make_create_experiment
    ~base_contrib_a0
    ~base_contrib_b0
    ?epsilon_a0
    ?epsilon_b0
    ?(create_extra_goals = fun get_controls -> [])
    name =
  fun get_controls ->
    let base_goals =
      create_default_goals
        ?epsilon_a0
        ?epsilon_b0
        ~base_contrib_a0
        ~base_contrib_b0
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
    ?epsilon_a0
    ?epsilon_b0
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
      ?epsilon_a0
      ?epsilon_b0
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

let test_window1 () =
  make_test
    ~name: "window1"
    ~window_length: 1
    ~base_contrib_a1: 0.
    ~base_contrib_a2: 0.
    ~base_contrib_b1: 0.
    ~base_contrib_b2: 0.
    ()

let test_window3 () =
  make_test
    ~name: "window3"
    ~window_length: 3
    ()

let test_window10 () =
  make_test
    ~name: "window10"
    ~window_length: 10
    ()

let test_scaled_contributions () =
  let scale x = -1000. *. x in
  let abs_scale x = abs_float (scale x) in
  make_test
    ~base_contrib_a0: (scale default_base_contrib_a0)
    ~base_contrib_a1: (scale default_base_contrib_a1)
    ~base_contrib_a2: (scale default_base_contrib_a2)
    ~base_contrib_b0: (scale default_base_contrib_b0)
    ~base_contrib_b1: (scale default_base_contrib_b1)
    ~base_contrib_b2: (scale default_base_contrib_b2)
    ~epsilon_a0: (abs_scale default_epsilon_a0)
    ~epsilon_b0: (abs_scale default_epsilon_b0)
    ~name: "scaled_contributions"
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
  let epsilon_a0 = r *. default_epsilon_a0 in
  let epsilon_b0 = r *. default_epsilon_b0 in
  make_test
    ~name: "large_difference_corrected"
    ~base_contrib_a0: 100.
    ~epsilon_a0
    ~epsilon_b0
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

(* same parameters as noisy_contribution below, without the noise. *)
let test_nonnoisy_contribution () =
  assert (default_base_contrib_a0 = 1.);
  make_test
    ~name: "nonnoisy_contribution"
    ~determine_actions_ab:determine_actions_always_b
    ~epsilon_b0: 1000.
    ()

let test_noisy_other_contribution () =
  assert (default_base_contrib_a0 = 1.);
  make_test
    ~name: "noisy_other_contribution"
    ~determine_actions_ab:determine_actions_always_b
    ~noise_b:(fun _ ->
      U_random.normal ~stdev: 0.5 ()
    )
    ~epsilon_b0: 1000.
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
    ~epsilon_a0: 0.4
    ~epsilon_b0: 0.04
    ~noise_a
    ~noise_b
    ()

let test_global_noise suffix noise_stdev =
  assert (default_base_contrib_a0 = 1.);
  assert (default_base_contrib_b0 = 0.1);
  let noise t =
    U_random.normal ~stdev:noise_stdev ()
  in
  make_test
    ~name: ("global_noise" ^ suffix)
    ~noise
    ()

let test_global_noise1 () = test_global_noise "1" 0.1
let test_global_noise2 () = test_global_noise "2" 0.2

let tests = [
  "default", test_default;
  "window1", test_window1;
  "window3", test_window3;
  "window10", test_window10;
  "scaled_contributions", test_scaled_contributions;
  "large difference", test_large_difference;
  "large difference_corrected", test_large_difference_corrected;
  "subaction", test_subaction;
  "adaptation", test_adaptation;
  "non-noisy contribution", test_nonnoisy_contribution;
  "noisy other contribution", test_noisy_other_contribution;
  "noisy contributions", test_noisy_contributions;
  "global noise1", test_global_noise1;
  "global noise2", test_global_noise2;
]
