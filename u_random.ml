(*
   Utilities for random number generation.
*)

(*
   Return true with probability `proba`.
*)
let pick proba =
  Random.float 1. < proba

(*
   Pick a value following a normal distribution.
*)
let normal ?(mean = 0.) ?(stdev = 1.) () =
  let x = U_normal.pick () in
  mean +. stdev *. x
