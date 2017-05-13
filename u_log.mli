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

val set_time : int -> unit
  (* set the time to be displayed by each call to `log` or `logf`,
     meant to be an iteration number rather than real time.
     This flushes any buffered log output. *)

val flush : unit -> unit
  (* flush buffered log output. *)
