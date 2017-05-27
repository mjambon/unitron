(*
   An act is an instance of an action, i.e. a pair (time, action).
   We keep track of recent acts for reinforcement purposes.
*)

type t = {
  recent: U_controlid.t U_set.set U_recent.t;
}

let create window_length =
  let recent = U_recent.init window_length (fun age -> U_set.create_set ()) in
  { recent }

let step x =
  U_recent.step x.recent (fun old -> U_set.clear old; old)

let get_latest x =
  U_recent.get_latest x.recent

let add x controlid =
  let latest = get_latest x in
  U_set.add latest controlid

(*
   Fold over all the pairs (age, control ID)
*)
let fold x acc0 f =
  U_recent.fold x.recent acc0 (fun age set acc ->
    U_set.fold set acc (fun controlid acc ->
      f age controlid acc
    )
  )

let iter x f =
  fold x () (fun age controlid () -> f age controlid)

let to_list x =
  fold x [] (fun age controlid acc -> (age, controlid) :: acc)
