(*
   Various values, updated at each cycle, meant to be used
   by as input by an IO module.
   See mli.
*)

open Printf

type t = {
  normalized_goal : float;
    (* normalized goal *)
  recent_normalized_goal : float;
    (* recent normalized goal *)
  recent_delta : float;
    (* recent |delta| / average |delta| *)
  activity : float;
    (* activity =
         recent number of contributions / average number of contributions *)
  recent_pos_contrib : float;
    (* recent positive contribution / average positive contribution *)
  recent_neg_contrib : float;
    (* recent negative contribution / average negative contribution *)
}

type time = int

type state = {
  mutable value : t option;
  mutable last_updated : time;
  update_value : time -> U_info.t -> t;
}

let get state t =
  if t < 0 then
    invalid_arg "U_obs.get: negative time";
  if t <> state.last_updated then
    invalid_arg "U_obs.get: state is not up to date";
  match state.value with
  | None -> assert false
  | Some x -> x

let update state t info =
  if t <> state.last_updated + 1 then
    invalid_arg (
      sprintf "U_obs.update: last updated at %i, current time is %i"
        state.last_updated t
    );
  assert (U_float.is_finite info.U_info.goal);
  let new_value = state.update_value t info in
  state.value <- Some new_value;
  state.last_updated <- t

let create_stat window get =
  let alpha = 1. /. float window in
  let state = Mv_var.init ~alpha_avg:alpha ~alpha_var:alpha () in
  let force_update t =
    let x = get t in
    if U_float.is_finite x then
      Mv_var.update state x
  in
  let update = U_lazy.get force_update in
  let get_average t =
    update t;
    Mv_var.get_average state
  in
  let get_stdev t =
    update t;
    Mv_var.get_stdev state
  in
  let get_normalized t =
    update t;
    Mv_var.get_normalized state
  in
  get_average, get_stdev, get_normalized

let create () =
  let open U_info in
  let short_window = 20 in
  let long_window = 1000 in

  let create_long_stat get =
    create_stat long_window get
  in

  let create_short_stat get =
    create_stat short_window get
  in

  let input = ref U_info.dummy in

  let get_goal t = !input.goal in
  let get_pos_contrib t = !input.pos_contrib in
  let get_neg_contrib t = !input.neg_contrib in
  let get_pos_contrib_count t = !input.pos_contrib_count in
  let get_neg_contrib_count t = !input.neg_contrib_count in

  let _, _, get_norm_goal = create_long_stat get_goal in
  let get_recent_norm_goal, _, _ = create_short_stat get_norm_goal in

  let get_prediction t =
    get_pos_contrib t +. get_neg_contrib t
  in

  let get_delta t =
    abs_float (get_prediction t -. get_goal t)
  in

  let get_avg_delta, _, _ = create_long_stat get_delta in
  let get_recent_delta, _, _ = create_short_stat get_delta in
  let get_delta_ratio t =
    get_recent_delta t /. get_avg_delta t
  in

  let get_avg_pos_contrib, _, _ = create_long_stat get_pos_contrib in
  let get_recent_pos_contrib, _, _ = create_short_stat get_pos_contrib in
  let get_pos_contrib_ratio t =
    get_recent_pos_contrib t /. get_avg_pos_contrib t
  in

  let get_avg_neg_contrib, _, _ = create_long_stat get_neg_contrib in
  let get_recent_neg_contrib, _, _ = create_short_stat get_neg_contrib in
  let get_neg_contrib_ratio t =
    get_recent_neg_contrib t /. get_avg_neg_contrib t
  in

  let get_contrib_count t =
    float (get_pos_contrib_count t + get_neg_contrib_count t)
  in

  let get_avg_contrib_count, _, _ = create_long_stat get_contrib_count in
  let get_recent_contrib_count, _, _ = create_short_stat get_contrib_count in
  let get_activity t =
    get_recent_contrib_count t /. get_avg_contrib_count t
  in

  let update_value t new_input =
    input := new_input;
    let normalized_goal = get_norm_goal t in
    let recent_normalized_goal = get_recent_norm_goal t in
    let recent_delta = get_delta_ratio t in
    let activity = get_activity t in
    let recent_pos_contrib = get_pos_contrib_ratio t in
    let recent_neg_contrib = get_neg_contrib_ratio t in
    {
      normalized_goal;
      recent_normalized_goal;
      recent_delta;
      activity;
      recent_pos_contrib;
      recent_neg_contrib;
    }
  in
  {
    value = None;
    last_updated = -1;
    update_value;
  }

let to_string x =
  sprintf "normalized_goal=%.3f \
           recent_normalized_goal=%.3f \
           recent_delta=%.3f \
           activity=%.3f \
           recent_pos_contrib=%.3f \
           recent_neg_contrib=%.3f"
    x.normalized_goal
    x.recent_normalized_goal
    x.recent_delta
    x.activity
    x.recent_pos_contrib
    x.recent_neg_contrib
