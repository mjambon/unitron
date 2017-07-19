(*
   Produce a lazy update function for a given time step.
   It allows calling it multiple times without worrying
   about unnecessary recomputations.
*)
let get (refresh : U_time.t -> 'a) : U_time.t -> 'a =
  let last_updated = ref max_int in
  let cached_result = ref None in
  fun t ->
    if t <> !last_updated then (
      let result = refresh t in
      cached_result := Some result;
      last_updated := t;
      result
    )
    else
      match !cached_result with
      | None -> assert false
      | Some x -> x
