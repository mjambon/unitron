(*
   Generic main loop.
*)

let continue opt_max_iter opt_stop_condition t =
  (match opt_max_iter with
   | None -> true
   | Some n -> t < n)
  &&
  (match opt_stop_condition with
   | None -> true
   | Some stop_condition -> stop_condition t)

let run ?(inner_log_mode = `Skip) ?max_iter ?stop_condition f =
  let rec loop t =
    U_log.set_time t;
    let ok = continue max_iter stop_condition t in
    if ok then (
      f t;
      U_log.flush ();
      loop (t + 1)
    )
  in
  let iter_count =
    match max_iter with
    | None -> assert false
    | Some x -> x
  in
  let (), dt =
    U_perf.time (fun () ->
      U_log.set_mode inner_log_mode;
      loop 0;
      U_log.clear_time ();
      U_log.set_mode `Full
    )
  in
  let step_duration = dt /. float iter_count in
  U_log.logf "total time: %.6f s" dt;
  U_log.logf "step duration: %.2g ms, %.2g KHz"
    (1e3 *. step_duration) (1. /. (1e3 *. step_duration));
  U_log.flush ()

let test () =
  let acc = ref [] in
  let f t =
    acc := t :: !acc
  in
  run ~max_iter:3 f;
  assert (!acc = List.rev [0; 1; 2]);
  true

let tests = [
  "main", test;
]
