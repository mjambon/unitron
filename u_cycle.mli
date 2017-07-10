(*
   Run a system forever or for an number of steps,
   with some useful logging.

   max_iter: number of iterations; the default is to loop forever.
   before_step, after_step: functions to call at the beginning and at the
                            end of each iteration.
*)
val loop :
  ?inner_log_mode: U_log.mode ->
  ?max_iter: U_system.time ->
  ?stop_condition: (U_system.time -> bool) ->
  ?before_step: (U_system.time -> unit) ->
  ?after_step: (U_system.time -> unit) ->
  U_system.t -> unit
