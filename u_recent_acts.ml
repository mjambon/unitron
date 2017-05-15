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

let add x controlid =
  let latest = U_recent.latest x.recent in
  U_set.add latest controlid
