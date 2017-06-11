(*
   Tests + demo program
*)

let main () =
  if not (U_tests.run ()) then
    exit 1

let () = main ()
