(*
   Logging
*)

open Printf

let string_of_exn e =
  sprintf "%s\n%s"
    (Printexc.to_string e)
    (Printexc.get_backtrace ())

let () = Printexc.record_backtrace true

let out_channel = stderr

let flush () =
  Pervasives.flush out_channel

let time = ref None

let set_time t =
  time := Some t

let clear_time () =
  time := None

let log s =
  match !time with
  | Some t ->
      fprintf out_channel "[%i] %s\n"
        t s
  | None ->
      fprintf out_channel "[] %s\n"
        s

let rec has_only_one_nonzero_digit n =
  if n < 0 then
    invalid_arg "has_only_one_nonzero_digit: negative value"
  else if n < 10 then
    true
  else if n mod 10 = 0 then
    has_only_one_nonzero_digit (n / 10)
  else
    false

type mode = [ `Full | `Skip | `Off ]

let mode = ref (`Full : mode)

let set_mode x =
  mode := x

let should_skip () =
  match !time with
  | None ->
      true
  | Some t ->
      not (has_only_one_nonzero_digit t)

let logf msgf =
  let print =
    match !mode with
    | `Skip when should_skip () ->
        (fun s -> ())
    | `Full | `Skip ->
        log
    | `Off ->
        (fun s -> ())
  in
  Printf.kprintf print msgf
