(*
   Define an experiment, i.e. a set of parameters and
   and conditions that make the system stop once it reaches
   a satisfying state.
*)

open U_log

type goal = {
  goal_name: string;
  goal_condition: U_system.t -> U_system.time -> bool;
  mutable goal_reached_at: U_system.time option;
}

type experiment = {
  exp_name: string;
  exp_goals: goal list;
}

let update_goal_state x system t =
  match x.goal_reached_at with
  | Some _ -> ()
  | None ->
      if x.goal_condition system t then (
        x.goal_reached_at <- Some t
      )

let goal_was_reached x =
  x.goal_reached_at <> None

let stop_condition (x : experiment) system t =
  List.iter (fun goal -> update_goal_state goal system t) x.exp_goals;
  List.for_all goal_was_reached x.exp_goals

let create_goal name cond = {
  goal_name = name;
  goal_condition = cond;
  goal_reached_at = None;
}

let create_experiment name goals = {
  exp_name = name;
  exp_goals = goals;
}

(*
   Inspect the results of the same experiment with the same goals,
   repeated several times, and associate each goal name with
   the number of steps it took to reach it.
*)
let make_report l =
  let first_exp =
    match l with
    | [] -> assert false
    | x :: _ -> x
  in

  let tbl = Hashtbl.create 10 in
  List.iter
    (fun x -> Hashtbl.add tbl x.goal_name (ref []))
    first_exp.exp_goals;

  List.iter (fun x ->
    List.iter (fun goal ->
      match goal.goal_reached_at with
      | None -> assert false
      | Some n ->
          let r =
            try Hashtbl.find tbl goal.goal_name
            with Not_found -> assert false
          in
          r := n :: !r
    ) x.exp_goals
  ) l;

  let goals_reached =
    List.map (fun x ->
      let name = x.goal_name in
      let r =
        try Hashtbl.find tbl name
        with Not_found -> assert false
      in
      (name, !r)
    ) first_exp.exp_goals
  in
  (first_exp.exp_name, goals_reached)

let print_goal_report exp_name (goal_name, int_list) =
  let data = List.map float int_list in
  let mean, stdev = U_stat.get_mean_and_stdev data in
  let p = U_stat.get_percentile data in
  logf "experiment %s, number of steps used to reach goal %s:"
    exp_name goal_name;
  logf "  n = %i" (List.length data);
  logf "  mean, stdev = %.2f, %.2f" mean stdev;
  logf "  min    = %0f" (p 0.);
  logf "  p10    = %0f" (p 0.1);
  logf "  p25    = %0f" (p 0.25);
  logf "  median = %0f" (p 0.5);
  logf "  p75    = %0f" (p 0.1);
  logf "  p25    = %0f" (p 0.25);
  logf "  max    = %0f" (p 1.)

let print_report (exp_name, goals_reached) =
  List.iter (print_goal_report exp_name) goals_reached
