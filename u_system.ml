(*
   A complete system.

   At each cycle t, the system is fed some input consisting in a set of
   control IDs. Each action activated by at least one control is executed.
*)

type time = int

type t = {
  goal_function : time -> float;
  read_active_controls : time -> U_controlid.t U_set.set;
  get_control : U_controlid.t -> U_control.t;
  get_action : U_actionid.t -> U_action.t;
  recent_acts : U_recent_acts.t;
}

(*
   Run one cycle at time t.
*)
let tick x t =
  ()
