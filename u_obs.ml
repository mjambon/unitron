(*
   Various values, updated at each cycle, meant to be used
   by as input by an IO module.
*)

type signal =
  | Goal
      (* goal function *)
  | Pos_contrib
      (* sum of the positive terms of the prediction of the goal function *)
  | Neg_contrib
      (* sum of the negative terms of the prediction of the goal function *)
  | Pos_contrib_count
      (* number of positive terms in the prediction of the goal function *)
  | Neg_contrib_count
      (* number of negative terms in the prediction of the goal function *)

  | Average of stat
      (* moving average *)

  | Stdev of stat
      (* moving standard deviation *)

  | Normalized of stat
      (* signal translated and scaled such that mean = 0 and stdev = 1,
         using moving average and moving standard deviation. *)

  | Add of signal * signal
  | Sub of signal * signal
  | Mul of signal * signal
  | Div of signal * signal

and stat =
  | Short of signal
      (* moving average and standard deviation over a short window *)
  | Long of signal
      (* moving average and standard deviation over a long window *)

(*
   instant = latest value (identity: instant signal = signal)
   recent = moving average over a short window
   average = moving average over a long window
   normalized = signal translated and scaled such that mean = 0 and stdev = 1,
                using moving average and moving variance estimated over
                a long window.
   goal = feedback obtained at each cycle, that we try to predict
   prediction = prediction of the goal function
   delta = prediction - goal
   contribution = one term in the prediction produced at a given time
                  (the number of contributions changes over time)
*)
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

(*
   Produce a lazy update function for a given time step.
   It allows calling it multiple times without worrying
   about unnecessary recomputations.
*)
let lazy_update (update : int -> unit) : int -> unit =
  let last_updated = ref max_int in
  fun t ->
    if t <> !last_updated then (
      update t;
      last_updated := t
    )

let create
    ~get_goal
    ~get_pos_contrib
    ~get_neg_contrib
    ~get_pos_contrib_count
    ~get_neg_contrib_count =

  let short_window = 20 in
  let long_window = 1000 in

  let create_stat window get =
    let r = 1. /. float window in
    let state = Moving_variance.init ~r_avg:r ~r_var:r () in
    let force_update t =
      Moving_variance.update state (get t)
    in
    let update = lazy_update force_update in
    let get_average t =
      update t;
      Moving_variance.get_average state
    in
    let get_stdev t =
      update t;
      Moving_variance.get_stdev state
    in
    let get_normalized t =
      update t;
      Moving_variance.get_normalized state
    in
    get_average, get_stdev, get_normalized
  in

  let create_long_stat get =
    create_stat long_window get
  in

  let create_short_stat get =
    create_stat short_window get
  in

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

  let latest = ref {
    normalized_goal = nan;
    recent_normalized_goal = nan;
    recent_delta = nan;
    activity = nan;
    recent_pos_contrib = nan;
    recent_neg_contrib = nan;
  } in

  let force_update t =
    let normalized_goal = get_norm_goal t in
    let recent_normalized_goal = get_recent_norm_goal t in
    let recent_delta = get_delta_ratio t in
    let activity = get_activity t in
    let recent_pos_contrib = get_pos_contrib_ratio t in
    let recent_neg_contrib = get_neg_contrib_ratio t in
    latest := {
      normalized_goal;
      recent_normalized_goal;
      recent_delta;
      activity;
      recent_pos_contrib;
      recent_neg_contrib;
    }
  in
  let update = lazy_update force_update in
  let get t =
    update t;
    !latest
  in
  update, get
