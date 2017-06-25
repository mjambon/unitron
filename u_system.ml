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

  get_observables : time -> U_obs.t;
}

let create
    ~window_length
    ~goal_function
    ~read_active_controls
    ~get_control
    ~get_action =
  let recent_acts = U_recent_acts.create window_length in
  let get_observables =
    (* probably the wrong place for creating this;
       whatever handles these values internally should surface this
       as its API *)
    U_obs.create
      ~get_goal
      ~get_pos_contrib
      ~get_neg_contrib
      ~get_pos_contrib_count
      ~get_neg_contrib_count
  in
  {
    window_length;
    goal_function;
    read_active_controls;
    get_control;
    get_action;
    recent_acts;
    get_observables;
  }
