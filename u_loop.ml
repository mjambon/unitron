(*
   Generic main loop.
*)

let should_start opt_max_iter t =
  match opt_max_iter with
  | None -> true
  | Some n -> t < n

let run ?(inner_log_mode = `Skip) ?max_iter f =
  let iter_count = ref 0 in
  let rec loop continue t =
    U_log.set_time t;
    let ok = continue && should_start max_iter t in
    if ok then (
      let continue = f t in
      U_log.flush ();
      incr iter_count;
      loop continue (t + 1)
    )
  in
  let (), dt =
    U_perf.time (fun () ->
      U_log.set_mode inner_log_mode;
      loop true 0;
      U_log.clear_time ();
      U_log.set_mode `Full
    )
  in
  let step_duration = dt /. float !iter_count in
  U_log.logf "total time: %.6f s" dt;
  U_log.logf "step duration: %.2g ms, %.2g KHz"
    (1e3 *. step_duration) (1. /. (1e3 *. step_duration));
  U_log.flush ()

let test () =
  let acc = ref [] in
  let f t =
    acc := t :: !acc;
    true
  in
  run ~max_iter:3 f;
  assert (!acc = List.rev [0; 1; 2]);
  true

let tests = [
  "main", test;
]
