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

let minf l f = List.fold_left (fun acc x -> min acc (f x)) 0. l
let min l = List.fold_left min infinity l

let maxf l f = List.fold_left (fun acc x -> max acc (f x)) 0. l
let max l = List.fold_left max neg_infinity l

let sumf l f = List.fold_left (fun acc x -> acc +. f x) 0. l
let sum l = List.fold_left (+.) 0. l

let default ~if_nan x =
  if (x <> x) then
    if_nan
  else
    x
