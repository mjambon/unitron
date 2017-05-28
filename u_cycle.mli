(*
   Run a system forever or for an number of steps,
   with some useful logging.
*)
val loop : ?max_iter:U_system.time -> U_system.t -> unit
