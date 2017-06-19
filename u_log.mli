(*
   Logging.
*)

val string_of_exn : exn -> string
  (* convert an exception into a readable multiline string including
     a backtrace. *)

val log : string -> unit
  (* logging function, writes to stderr.
     Warning: it does not automatically flush the buffered output. *)

val logf : ('a, unit, string, unit) format4 -> 'a
  (* printf-like logging function, writes to stderr.
     Warning: it does not automatically flush the buffered output. *)

val clear_time : unit -> unit
val set_time : int -> unit
  (* set the time to be displayed by each call to `log` or `logf`,
     meant to be an iteration number rather than real time. *)

type mode = [ `Full | `Skip | `Off ]

val set_mode : mode -> unit
  (* set the logging mode or "level".
     `Skip` will result in `logf` printing only if the current time
     has a single leading nonzero digit followed by zeroes.
     The goal of the skip mode is to produce less and less output
     as the number of steps increases. *)

val flush : unit -> unit
  (* flush buffered log output. *)
