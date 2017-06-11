(*
   Generic main loop.
*)

let run ?max_iter f =
  let rec loop t =
    U_log.set_time t;
    let ok =
      match max_iter with
      | None -> true
      | Some n -> t < n
    in
    if ok then (
      f t;
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
      loop 0
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
