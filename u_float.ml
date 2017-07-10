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
