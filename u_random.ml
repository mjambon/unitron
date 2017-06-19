
(*
   Return true with probability `proba`.
*)
let pick proba =
  Random.float 1. < proba
