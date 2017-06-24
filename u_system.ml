(*
   Initialization of a complete system.

   At each cycle t, the system is fed some input consisting in a set of
   control IDs. Each action activated by at least one control is executed.
*)

type time = int

type t = {
  (* Parameters *)
  window_length: int;
    (* Reinforcement time window *)

  goal_function : time -> float;

  read_active_controls : time -> (U_controlid.t -> unit) -> unit;
    (* `read_active_controls t add` is in charge of registering
       active controls using the provided `add` function. *)

  get_control : U_controlid.t -> U_control.t;
  get_action : U_actionid.t -> U_action.t;
  recent_acts : U_recent_acts.t;

  observables : U_obs.t;
}

let create
    ~window_length
    ~goal_function
    ~read_active_controls
    ~get_control
    ~get_action =
  let recent_acts = U_recent_acts.create window_length in
  {
    window_length;
    goal_function;
    read_active_controls;
    get_control;
    get_action;
    recent_acts;
    observables;
  }
