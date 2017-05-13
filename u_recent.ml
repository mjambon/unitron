(*
   Data structure for keeping a window of the last w states in a sequence.
*)

(*
   For a window of length 5, we use an array structured as follows:

     [0 1 2 3 4]
        ^ ^
        | current index
        previous index
*)
type 'a t = {
  array: 'a array;
  mutable index: int;
    (* position of the latest element in the array *)
}

(*
   Initialize the circular array, proceeding from oldest to latest.
   The argument of the user function is the age of the element,
   like for `iter`.
*)
let init w f =
  let a =
    Array.init w (fun pos ->
      let age = w - pos - 1 in
      f age
    )
  in
  {
    array = a;
    index = w - 1;
  }

(*
   Return the latest element, whose age is 0.
*)
let latest x =
  x.array.(x.index)

(*
   Iterate from oldest to latest.
   The user function takes the age of the element and the element.
*)
let iter x f =
  let a = x.array in
  let w = Array.length a in
  for i = 0 to w - 1 do
    (* position in the array *)
    let pos = (i + 1) mod w in

    (* age of the element, using age(latest) = 0 *)
    let age = w - i - 1 in

    let elt = a.(pos) in
    f age elt
  done

(*
   Move in time, adding 1 to the age of all past elements.
   The oldest element is passed to the user function to be recycled
   into the new latest element.
*)
let next x f =
  let a = x.array in
  let w = Array.length a in
  let new_index = (x.index + 1) mod w in
  let recycled_elt = a.(new_index) in
  a.(new_index) <- f recycled_elt;
  x.index <- new_index

(* TODO: tests *)
