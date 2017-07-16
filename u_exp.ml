(*
   Define an experiment, i.e. a set of parameters and
   and conditions that make the system stop once it reaches
   a satisfying state.
*)

type time = int

type goal = {
  goal_name: string;
  goal_condition: time -> U_system.t -> bool;
  mutable goal_reached_at: time option;
}

type experiment = {
  exp_name: string;
  exp_goals: goal list;
}
