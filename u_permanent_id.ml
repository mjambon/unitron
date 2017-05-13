(*
   Generic ID generator suitable for permanent resource identifiers.

   It internally uses ints for efficiency, but maintains a table
   for translating those ints back to their original strings.
*)

module type Param = sig
  val name : string
end

module type Id = sig
  type t = private int

  val of_string : string -> t
    (* Any string is valid. It is guaranteed that
         a = b => of_string a = of_string b
    *)

  val to_string : t -> string
    (* It is guaranteed that
         a = b => to_string a = to_string b
    *)
end

module Make (Param : Param) : Id = struct
  type t = int

  let tbl_str_to_int = Hashtbl.create 1000
  let tbl_int_to_str = Hashtbl.create 1000

  let counter = ref 0

  let of_string s =
    try Hashtbl.find tbl_str_to_int s
    with Not_found ->
      let last = !counter in
      let i = last + 1 in
      if i = 0 then
        failwith ("Cannot create new " ^ Param.name)
      else (
        counter := i;
        Hashtbl.add tbl_int_to_str i s;
        Hashtbl.add tbl_str_to_int s i;
        i
      )

  let to_string i =
    try Hashtbl.find tbl_int_to_str i
    with Not_found -> assert false
end

module Test = struct
  module Id = Make (struct let name = "test ID" end)

  let test () =
    let a = Id.of_string "a" in
    let b = Id.of_string "b" in
    let a2 = Id.of_string "a" in
    assert (a = a2);
    assert (a <> b);
    assert (Id.to_string a = Id.to_string a2);
    assert (Id.to_string a == Id.to_string a2);
    assert (Id.to_string a = "a");
    assert (Id.to_string b = "b");
    true
end

let tests = [
  "main", Test.test;
]
