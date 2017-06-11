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

let print_controls controls =
  U_set.iter_ordered controls (fun x ->
    logf "%s" (U_control.to_info x)
  )

(*
   Return true with probability `proba`.
*)
let pick proba =
  Random.float 1. < proba

let check_expectation ~expected ~tolerance ~obtained =
  let ok =
    obtained >= expected -. tolerance
    && obtained <= expected +. tolerance
  in
  if not ok then
    logf "failed expected:%g tolerance:%g obtained:%g"
      expected tolerance obtained;
  ok

let check_learned_contributions ~control ~contrib0 ~tolerance =
  U_control.iter_contributions control (fun ~age ~average ~variance ->
    match age with
    | 0 ->
        assert (
          check_expectation ~expected:contrib0 ~tolerance ~obtained:average
        )
    | _ ->
        assert (
          check_expectation ~expected:0. ~tolerance ~obtained:average
        )
  )

let default_contrib_a = 1.
let default_contrib_b = 0.1

let test_system
  ?(max_iter = 1000)
  ?(contrib_a = default_contrib_a)
  ?(contrib_b = default_contrib_b)
  ?(tolerance_a = 0.05)
  ?(tolerance_b = 0.05)
  ?(noise_a = fun t -> 0.)
  ?(noise_b = fun t -> 0.)
  ?(noise = fun t -> 0.)
  ?(determine_actions_ab = fun t -> pick 0.6, pick 0.9)
  () =
  let window_length = 10 in
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

  let controlid_a = U_controlid.of_string "A" in
  add_control controlid_a actionid_a;

  (* B has its own frequency and constant contribution, independent from A. *)
  let b_was_active = ref false in
  let actionid_b = U_actionid.of_string "B" in
  U_action.add actionid_b (fun () -> b_was_active := true) actions;

  let controlid_b = U_controlid.of_string "B" in
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
    print_controls controls
  in
  U_cycle.loop
    ~max_iter
    ~before_step
    ~after_step
    system;

  let control_a = get_control controlid_a in
  let control_b = get_control controlid_b in
  check_learned_contributions
    ~control:control_a
    ~contrib0:contrib_a
    ~tolerance:tolerance_a;
  check_learned_contributions
    ~control:control_b
    ~contrib0:contrib_b
    ~tolerance:tolerance_b;
  true

let test_default () =
  test_system ()

let test_negative () =
  test_system
    ~contrib_b:(-0.1)
    ()

let test_large_difference () =
  test_system
    ~contrib_a:(10.)
    ~contrib_b:(0.1)
    ~tolerance_a:0.1
    ~tolerance_b:0.001
    ~determine_actions_ab: (fun t -> pick 0.5, pick 0.5)
    ()

let test_noisy_contribution () =
  assert (default_contrib_a = 1.);
  test_system
    ~noise_a:(fun _ ->
      Random.float 0.2 -. Random.float 0.2
    )
    ()

(* B active => A active *)
let test_subaction () =
  let determine_actions_ab t =
    let a = pick 0.5 in
    let b = a && pick 0.5 in
    a, b
  in
  test_system
    ~determine_actions_ab
    ()

let test_global_noise () =
  assert (default_contrib_a = 1.);
  assert (default_contrib_b = 0.1);
  let noise t =
    Random.float 0.2 -. 0.1
  in
  test_system
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
    ~max_iter
    ~noise_a
    ~noise_b
    ~determine_actions_ab: (fun t -> pick 0.5, pick 0.5)
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
