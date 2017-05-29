(*
   A mutable collection of unique elements identified by a key.
*)

(*
   A map from keys to values.
   Each key is associated with a unique value.
*)
type ('k, 'v) t = {
  get_key: 'v -> 'k;
    (* Obtain an object's key. The key must be comparable and hashable,
       see the documentation for Hashtbl. *)

  tbl: ('k, 'v) Hashtbl.t;
}

(*
   A map in which key and value are equal.
*)
type 'a set = ('a, 'a) t

val create : ('v -> 'k) -> ('k, 'v) t
  (* Create an empty map. *)

val create_set : unit -> 'a set
  (* Create an empty set. *)

val get : ('k, 'v) t -> 'k -> 'v option
  (* Get an element from its key. *)

val add : ('k, 'v) t -> 'v -> unit
  (* Add an element. *)

val remove : ('k, 'v) t -> 'v -> unit
  (* Remove an element. *)

val remove_key : ('k, 'v) t -> 'k -> unit
  (* Remove an element from its key. *)

val pop : ('k, 'v) t -> 'k -> 'v option
  (* Get an element, then remove it. *)

val to_list : ('k, 'v) t -> 'v list
  (* List the elements. *)

val sort : ('k, 'v) t -> 'v list
  (* List and sort the elements based on their keys. *)

val of_list : 'a list -> 'a set
  (* Create a set from a list of elements. Elements must be suitable keys. *)

val of_list_full : 'v list -> ('v -> 'k) -> ('k, 'v) t
  (* Create a map from a list of elements. *)

val iter : ('k, 'v) t -> ('v -> unit) -> unit
  (* Iterate over the elements in no particular order. *)

val iter_ordered : ('k, 'v) t -> ('v -> unit) -> unit
  (* Iterate over the elements in the order defined by the keys,
     which involves a sorting step. *)

val fold : ('k, 'v) t -> 'acc -> ('v -> 'acc -> 'acc) -> 'acc
  (* Fold over the elements in no particular order. *)

val clear : ('k, 'v) t -> unit
  (* Remove all the elements. *)
