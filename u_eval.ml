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

let default_global_iter = 30
let default_max_iter = 100
let default_window_length = 10
let default_contrib_a = 1.
let default_contrib_b = 0.1
let default_tolerance_a = 0.05
let default_tolerance_b = 0.05
let default_determine_actions_ab t = (U_random.pick 0.5, U_random.pick 0.5)

let print_controls controls =
  U_set.iter_ordered controls (fun x ->
    logf "%s" (U_control.to_info x)
  )

let get_average_contributions window_length acc =
  let stat =
    Array.init window_length (fun age ->
      U_stat.create_stdev_acc ()
    )
  in
  List.iter (fun control ->
    U_control.iter_contributions control (fun ~age ~average ~stdev ->
      let add, _, _ = stat.(age) in
      add average
    )
  ) acc;

  Array.map (fun (_, _, get_mean_and_stdev) -> get_mean_and_stdev ()) stat

let print_contrib_stats controlid contrib_stat_array =
  Array.iteri (fun age (mean, stdev) ->
    logf "contribution %s[%i]: mean %.2g, stdev %.2g"
      (U_controlid.to_string controlid) age
      mean stdev
  ) contrib_stat_array

let check_expectation
    ~system_name
    ~controlid
    ~age
    ~expected
    ~tolerance
    ~obtained =
  let ok =
    obtained >= expected -. tolerance
    && obtained <= expected +. tolerance
  in
  if not ok then
    logf "ERROR %s %s[%i] expected:%g tolerance:%g obtained:%g"
      system_name
      (U_controlid.to_string controlid) age
      expected tolerance obtained;
  ok

let check_learned_contributions
    ~system_name ~controlid
    ~contrib0 ~tolerance contrib_stat =
  Array.iteri (fun age (contrib_mean, contrib_stdev) ->
    match age with
    | 0 ->
        assert (
          check_expectation
            ~system_name
            ~controlid
            ~age
            ~expected:contrib0
            ~tolerance
            ~obtained:contrib_mean
        )
    | _ ->
        assert (
          check_expectation
            ~system_name
            ~controlid
            ~age
            ~expected:0.
            ~tolerance
            ~obtained:contrib_mean
        )
  ) contrib_stat

let print_observables system t =
  let x = U_obs.get U_system.(system.observables) t in
  logf "observables: %s" (U_obs.to_string x)

(*
   TODO: change tolerance criteria to allow legitimate outliers.
   Possible solution:
   Run the same test multiple times and for each contribution
   in each control, compute mean and standard deviation,
   and compare to expectations.
*)
let test_system_once
  ?inner_log_mode
  ?(max_iter = default_max_iter)
  ~window_length
  ~controlid_a
  ~controlid_b
  ~contrib_a
  ~contrib_b
  ?(noise_a = fun t -> 0.)
  ?(noise_b = fun t -> 0.)
  ?(noise = fun t -> 0.)
  ?(determine_actions_ab = default_determine_actions_ab)
  () =
  let moving_avg_cst = 0.1 in

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

  let read_active_controls t add =
    let a, b = determine_actions_ab t in
    if a then (
      logf "A*";
      add controlid_a
    );
    if b then (
      logf "B*";
      add controlid_b
    )
  in

  let goal_function t =
    let contrib =
      (if !a_was_active then contrib_a +. noise_a t
       else 0.)
      +.
      (if !b_was_active then contrib_b +. noise_b t
       else 0.)
    in
    contrib +. noise t
  in

  let get_control id =
    U_control.get controls id
  in
  let get_action id =
    U_action.get actions id
  in

  let system =
    U_system.create
      ~window_length
      ~goal_function
      ~read_active_controls
      ~get_control
      ~get_action
  in
  let after_step t =
    print_controls controls;
    print_observables system t;
  in
  U_cycle.loop
    ?inner_log_mode
    ~max_iter
    ~before_step
    ~after_step
    system;

  let control_a = get_control controlid_a in
  let control_b = get_control controlid_b in
  (control_a, control_b)

(*
   We run the system from t=0 several times in order to get a sense
   of the variability of the results.
*)
let test_system
    ~name
    ?(global_iter = default_global_iter)
    ?max_iter
    ?(window_length = default_window_length)
    ?(contrib_a = default_contrib_a)
    ?(contrib_b = default_contrib_b)
    ?(tolerance_a = default_tolerance_a)
    ?(tolerance_b = default_tolerance_b)
    ?noise_a
    ?noise_b
    ?noise
    ?determine_actions_ab
    () =

  let controlid_a = U_controlid.of_string "A" in
  let controlid_b = U_controlid.of_string "B" in
  let acc_a = ref [] in
  let acc_b = ref [] in
  for i = 1 to global_iter do
    let inner_log_mode =
      if i = 1 then `Skip
      else `Off
    in
    logf "--- Run %i/%i ---" i global_iter;
    let control_a, control_b =
      test_system_once
        ~inner_log_mode
        ?max_iter
        ~window_length
        ~controlid_a
        ~controlid_b
        ~contrib_a
        ~contrib_b
        ?noise_a
        ?noise_b
        ?noise
        ?determine_actions_ab
        ()
    in
    acc_a := control_a :: !acc_a;
    acc_b := control_b :: !acc_b;
  done;
  let contrib_stat_a = get_average_contributions window_length !acc_a in
  let contrib_stat_b = get_average_contributions window_length !acc_b in
  print_contrib_stats controlid_a contrib_stat_a;
  print_contrib_stats controlid_b contrib_stat_b;
  check_learned_contributions
    ~system_name:name
    ~controlid:controlid_a
    ~contrib0:contrib_a
    ~tolerance:tolerance_a
    contrib_stat_a;
  check_learned_contributions
    ~system_name:name
    ~controlid:controlid_b
    ~contrib0:contrib_b
    ~tolerance:tolerance_b
    contrib_stat_b;
  true

let test_default () =
  test_system
    ~name:"default"
    ()

let test_negative () =
  test_system
    ~name:"negative"
    ~contrib_b:(-0.1)
    ()

let test_large_difference () =
  test_system
    ~name:"large_difference"
    ~contrib_a:(10.)
    ~contrib_b:(0.1)
    ~tolerance_a:0.5
    ~tolerance_b:0.5
    ~determine_actions_ab: (fun t -> U_random.pick 0.5, U_random.pick 0.5)
    ()

let test_noisy_contribution () =
  assert (default_contrib_a = 1.);
  test_system
    ~name:"noisy_contribution"
    ~noise_a:(fun _ ->
      Random.float 0.2 -. Random.float 0.2
    )
    ()

(* B active => A active *)
let test_subaction () =
  let determine_actions_ab t =
    let a = U_random.pick 0.5 in
    let b = a && U_random.pick 0.5 in
    a, b
  in
  test_system
    ~name:"subaction"
    ~max_iter:200
    ~determine_actions_ab
    ()

let test_global_noise () =
  assert (default_contrib_a = 1.);
  assert (default_contrib_b = 0.1);
  let noise t =
    Random.float 0.2 -. 0.1
  in
  test_system
    ~name:"global_noise"
    ~noise
    ()

let test_noisy_contributions () =
  assert (default_contrib_a = 1.);
  assert (default_contrib_b = 0.1);
  let noise_a t =
    Random.float 1. -. 0.5
  in
  let noise_b t =
    Random.float 0.1 -. 0.05
  in
  test_system
    ~name:"noisy_contributions"
    ~tolerance_a: 0.1
    ~tolerance_b: 0.08
    ~noise_a
    ~noise_b
    ()

(*
   Change the contributions of A and B suddenly, and see if we can adjust
   the predictions.
*)
let test_adaptation () =
  assert (default_contrib_a = 1.);
  assert (default_contrib_b = 0.1);
  let max_iter = 200 in
  let noise_a t =
    if t < 100 then 1.
    else 0.
  in
  let noise_b t =
    if t < 50 then (-0.1)
    else 0.
  in
  test_system
    ~name:"adaptation"
    ~max_iter
    ~noise_a
    ~noise_b
    ~determine_actions_ab: (fun t -> U_random.pick 0.5, U_random.pick 0.5)
    ()

let tests = [
  "default", test_default;
  "negative", test_negative;
  "large difference", test_large_difference;
  "noisy contribution", test_noisy_contribution;
  "subaction", test_subaction;
  "global noise", test_global_noise;
  "noisy contributions", test_noisy_contributions;
  "adaptation", test_adaptation;
]
