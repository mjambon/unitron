(*
   A mutable collection of unique elements identified by a key.
*)

type ('k, 'v) t = {
  get_key: 'v -> 'k;
    (* Obtain an object's key. The key must be comparable and hashable,
       see the documentation for Hashtbl. *)

  tbl: ('k, 'v) Hashtbl.t;
}

type 'a set = ('a, 'a) t

let create get_key =
  let tbl = Hashtbl.create 100 in
  { get_key; tbl }

let create_set () =
  create (fun x -> x)

let get x k =
  try Some (Hashtbl.find x.tbl)
  with Not_found -> None

let add x v =
  let k = x.get_key v in
  Hashtbl.replace x.tbl k v

let remove x v =
  Hashtbl.remove x.tbl (x.get_key v)

let remove_key x k =
  Hashtbl.remove x.tbl k

(*
   Get and remove.
*)
let pop x k =
  let opt = get x k in
  if opt <> None then
    remove_key x k;
  opt

let to_list x =
  Hashtbl.fold (fun k v acc -> v :: acc) x.tbl []

let of_list l =
  let x = create (fun v -> v) in
  List.iter (add x) l;
  x

let of_list_full l get_key =
  let x = create get_key in
  List.iter (add x) l;
  x

let iter x f =
  Hashtbl.iter (fun k v -> f v) x.tbl

let fold x acc f =
  Hashtbl.fold (fun k v acc -> f v acc) x.tbl acc

let clear x =
  Hashtbl.clear x.tbl
