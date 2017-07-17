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

let get_mean_and_stdev l =
  let add, get_mean, get_mean_and_stdev = create_stdev_acc () in
  List.iter add l;
  get_mean_and_stdev ()

(*
   Compute a percentile from a list of floats.
   The percentile is specified as parameter p in the range [0,1].
*)
let get_percentile l =
  let a = Array.of_list l in
  let n = Array.length a in
  if n = 0 then
    invalid_arg "get_percentile: no data";
  Array.sort compare a;
  fun p ->
    if not (p >= 0. && p <= 1.) then
      invalid_arg "get_percentile: p must be within [0,1]";
    let index = float (n - 1) *. p in
    let low_index = truncate index in
    let high_index = min (n - 1) (low_index + 1) in
    assert (low_index >= 0);
    let high_weight, _ = modf index in
    let low_weight = 1. -. high_weight in
    low_weight *. a.(low_index) +. high_weight *. a.(high_index)

let test_percentiles () =
  assert (get_percentile [0.] 0. = 0.);
  assert (get_percentile [1.23] 0. = 1.23);
  assert (get_percentile [1.23] 0.5 = 1.23);
  assert (get_percentile [1.23] 1. = 1.23);

  assert (get_percentile [0.; 1.] 0.6 = 0.6);
  assert (get_percentile [0.; 1.] 0. = 0.);
  assert (get_percentile [0.; 1.] 1. = 1.);

  assert (get_percentile [0.; 1.; 10.] 0.5 = 1.);
  assert (get_percentile [0.; 1.; 10.] 0.25 = 0.25);
  assert (get_percentile [0.; 1.; 10.] 0.75 = 5.5);

  assert (get_percentile [-1.; -0.5; 0.; 10.] 0. = -1.);
  assert (get_percentile [-1.; -0.5; 0.; 10.] 0.5 = -0.25);

  true


let tests = [
  "mean", test_mean;
  "stdev", test_stdev;
  "percentiles", test_percentiles;
]
