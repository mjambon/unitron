(*
   Identifiers of controls, specified as strings and translated into ints.
   See u_permanent_id.ml
*)

include U_permanent_id.Make (struct
  let name = "control ID"
end)
