(*
   Simple stats (mean, stdev, etc.) used primarily for testing.
*)

let sum l =
  List.fold_left (fun acc x -> acc +. x) 0. l

(*
   Create an accumulator for computing an arithmetic mean, using O(1) memory.
*)
let create_mean_acc () =
  let n = ref 0 in
  let sum = ref 0. in
  let add_sample x =
    incr n;
    sum := !sum +. x
  in
  let get_mean () =
    !sum /. float !n
  in
  add_sample, get_mean

(*
   Create an accumulator for computing a variance, using O(N) memory.
*)
let create_stdev_acc () =
  let values = ref [] in
  let add_sample x =
    values := x :: !values
  in
  let get_mean () =
    let l = !values in
    sum l /. float (List.length l)
  in
  let get_mean_and_stdev () =
    let mean = get_mean () in
    let variance =
      let l = !values in
      List.fold_left (fun acc x -> acc +. (x -. mean) ** 2.) 0. l
      /. float (List.length l - 1)
    in
    mean, sqrt variance
  in
  add_sample, get_mean, get_mean_and_stdev

let test_mean () =
  let add, get = create_mean_acc () in
  add 1.;
  add 3.;
  assert (get () = 2.);
  true

let test_stdev () =
  let add, get_mean, get_mean_and_stdev = create_stdev_acc () in
  add 1.;
  add 5.;
  let expected_mean = 3. in
  let expected_stdev = sqrt 8. in
  assert (get_mean () = expected_mean);
  let mean, stdev = get_mean_and_stdev () in
  assert (mean = expected_mean);
  assert (stdev = expected_stdev);
  true

let tests = [
  "mean", test_mean;
  "stdev", test_stdev;
]
