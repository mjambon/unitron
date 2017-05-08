(*
   Generic ID generator suitable for permanent resource identifiers.

   It internally uses ints for efficiency, but maintains a table
   for translating those ints back to their original strings.
*)

let tbl = Hashtbl.create 1000

