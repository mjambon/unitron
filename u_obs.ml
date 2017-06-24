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
type t = {
  (* normalized goal *)
  (* recent normalized goal *)
  (* recent delta / average delta *)
  (* activity = recent normalized number of contributions *)
  (* recent normalized average positive contribution *)
  (* recent normalized average negative contribution *)
}
