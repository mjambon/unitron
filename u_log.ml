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

let time = ref 0

let set_time t =
  time := t;
  flush ()

let log s =
  fprintf out_channel "[%i] %s\n"
    !time
    s

let logf msgf =
  Printf.kprintf log msgf
