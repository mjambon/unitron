(*
   Test suite.
*)

let tests = U_test.flatten [
  (* Unit tests of individual modules and subsystems *)
  "U_permanent_id", U_permanent_id.tests;
  "U_loop", U_loop.tests;
  "U_recent", U_recent.tests;

  (* Evaluation of the behavior of system for several scenarios *)
  "U_eval_simple", U_eval_simple.tests;
]

let run () =
  U_test.run_tests tests
