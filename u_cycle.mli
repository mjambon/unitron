(*
   Run a system forever or for an number of steps,
   with some useful logging.
*)
val loop :
  ?max_iter: U_system.time ->
  ?before_step: (U_system.time -> unit) ->
  ?after_step: (U_system.time -> unit) ->
  U_system.t -> unit
