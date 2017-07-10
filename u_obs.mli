(*
   Various values, updated at each cycle, meant to be used
   by as input by an IO module.
*)

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
type t = private {
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

type state
type time = int

val create : unit -> state
  (* Create an initial state at time = -1 with meaningless values.
     `get` may not be called before an `update` takes place. *)

val update : state -> time -> U_info.t -> unit
  (* Add data for a new timestep. Timesteps must be consecutive.
     Skipping or repeating is not allowed. *)

val get : state -> time -> t
  (* Get the observable data for the current timestep.
     Fails if the specified timestep differs.
     May be called multiple times for the same timestep. *)

val to_string : t -> string
  (* Produce a string representation for logging and debugging. *)
