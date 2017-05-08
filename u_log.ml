(*
   Logging
*)

open Printf

let string_of_exn e =
  sprintf "%s\n%s"
    (Printexc.to_string e)
    (Printexc.get_backtrace ())

let () = Printexc.record_backtrace true
