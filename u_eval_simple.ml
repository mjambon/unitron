(*
   A very simple setup to make sure we don't have a major bug.
*)

let create_system () =
  let window_length = 1 in
  let moving_avg_cst = 0.05 in

  let controls = U_control.create_set () in
  let actions = U_action.create_set () in
  let add_control id actionid =
    U_control.add ~moving_avg_cst ~window_length ~id ~actionid controls
  in

  (* A always results in the same positive contribution. *)
  let a_was_active = ref false in
  let actionid_a = U_actionid.of_string "A" in
  U_action.add actionid_a (fun () -> a_was_active := true) actions;

  let controlid_a = U_controlid.of_string "A" in
  add_control controlid_a actionid_a;
  let read_a add =
    if Random.int 2 = 0 then
      add controlid_a
  in

  (* B results in nothing. *)
  let actionid_b = U_actionid.of_string "B" in
  U_action.add actionid_b (fun () -> ()) actions;

  let controlid_b = U_controlid.of_string "B" in
  add_control controlid_b actionid_b;
  let read_b add =
    if Random.int 2 = 0 then
      add controlid_b
  in

  let init_step () =
    a_was_active := false
  in
  let read_active_controls t add =
    init_step ();
    read_a add;
    read_b add
  in

  (* Additionally, noise is added to the feedback. *)
  let goal_function t =
    let noise = Random.float 1. in
    let contrib =
      if !a_was_active then 1.
      else 0.
    in
    noise +. contrib
  in

  let get_control id =
    U_control.get controls id
  in
  let get_action id =
    U_action.get actions id
  in

  U_system.create
    ~window_length
    ~goal_function
    ~read_active_controls
    ~get_control
    ~get_action

let test () =
  let system = create_system () in
  U_cycle.loop ~max_iter:100 system;
  true

let tests = [
  "main", test;
]
