(*
   Run tests for this library.
*)

open Printf

let tests = List.flatten [
  U_permanent_id.tests;
]

let run_test (name, f) =
  eprintf "Test %s\n%!" name;
  let success =
    try f ()
    with e ->
      eprintf "Exception %s\n%!" (U_log.string_of_exn e);
      false
  in
  name, success

let print_result (name, success) =
  eprintf "%-20s %s\n" name (if success then "OK" else "ERROR")

let print_summary passed total =
  eprintf "Tests passed: %i/%i\n%!"
    passed total

let run_tests () =
  let results = List.map run_test tests in
  List.iter print_result results;
  let passed = List.length (List.filter snd results) in
  let total = List.length results in
  print_summary passed total
