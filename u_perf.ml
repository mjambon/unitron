(*
   Real time measurements.
*)

let time f =
  let t1 = Unix.gettimeofday () in
  let result = f () in
  let t2 = Unix.gettimeofday () in
  let dt = t2 -. t1 in
  result, dt

let print_time name f =
  let result, dt = time f in
  U_log.logf "time %s: %.6f" name dt;
  result
