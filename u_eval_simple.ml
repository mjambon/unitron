(*
   A very simple setup to make sure we don't have a major bug.
*)

open Printf
open U_log

let print_controls controls =
  U_set.iter_ordered controls (fun x ->
    logf "%s" (U_control.to_info x)
  )

(*
   Types of tests we want here:
   - two independent actions with positive contributions
   - one positive, one negative
   - one very large, one very small
   - one constant, one noisy
   - two noisy
   - one action implying the other one
   - constant global noise
   - fluctuating global noise independent from the actions

   Not covered here:
   - any number of actions other than 2
   - multiple controls for the same action
   - longer window
*)

let create_system () =
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
  let read_a add =
    if Random.float 1. < 0.6 then (
      logf "A*";
      add controlid_a
    )
  in

  (* B has its own frequency and constant contribution, independent from A. *)
  let b_was_active = ref false in
  let actionid_b = U_actionid.of_string "B" in
  U_action.add actionid_b (fun () -> b_was_active := true) actions;

  let controlid_b = U_controlid.of_string "B" in
  add_control controlid_b actionid_b;
  let read_b add =
    if Random.float 1. < 0.9 then (
      logf "B*";
      add controlid_b
    )
  in

  let before_step t =
    logf "--------------------------------------------------------------";
    a_was_active := false;
    b_was_active := false
  in
  let read_active_controls t add =
    read_a add;
    read_b add
  in

  let goal_function t =
    let contrib =
      (if !a_was_active then 1.
       else 0.)
      +.
      (if !b_was_active then (-0.1)
       else 0.)
    in
    let noise = 0. in
    contrib +. noise
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
  system, before_step, after_step

let test () =
  let system, before_step, after_step = create_system () in
  U_cycle.loop
    ~max_iter:100
    ~before_step
    ~after_step
    system;
  true

let tests = [
  "main", test;
]
