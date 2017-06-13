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

type level = [ `Opt | `Info ]

let time = ref 0

let set_time t =
  time := t

let log s =
  fprintf out_channel "[%i] %s\n"
    !time
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

type mode = [ `Full | `Skip ]

let mode = ref (`Full : mode)

let set_mode x =
  mode := x

let logf msgf =
  let print =
    match !mode with
    | `Skip when not (has_only_one_nonzero_digit !time) ->
        (fun s -> ())
    | _ ->
        log
  in
  Printf.kprintf print msgf

