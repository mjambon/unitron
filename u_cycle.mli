(*
   Run a system forever or for an number of steps,
   with some useful logging.

   max_iter: number of iterations; the default is to loop forever.
   before_step, after_step: functions to call at the beginning and at the
                            end of each iteration.
                            if `after_step` returns `false`, iterations
                            will stop.
*)
val loop :
  ?inner_log_mode: U_log.mode ->
  ?max_iter: U_system.time ->
  ?before_step: (U_system.time -> unit) ->
  ?after_step: (U_system.time -> bool) ->
  U_system.t -> unit
