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
  let iterations_per_second = float iter_count /. dt in
  U_log.logf "total time: %.6f s" dt;
  U_log.logf "iterations per second: %.2g" iterations_per_second;
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
