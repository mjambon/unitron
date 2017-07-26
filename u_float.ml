(*
   Utilities operating on floats
*)

let is_finite x =
  match classify_float x with
  | FP_infinite
  | FP_nan -> false
  | FP_normal
  | FP_subnormal
  | FP_zero -> true

let min l = List.fold_left min infinity l
let max l = List.fold_left max neg_infinity l
let sum l = List.fold_left (+.) 0. l
