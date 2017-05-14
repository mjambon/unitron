(*
   Test suite.
*)

let tests = U_test.flatten [
  "U_permanent_id", U_permanent_id.tests;
  "U_loop", U_loop.tests;
  "U_recent", U_recent.tests;
]

let run () =
  U_test.run_tests tests
