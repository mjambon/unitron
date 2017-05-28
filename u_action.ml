(*
   Library for managing actions.
   An action is a named function that has arbitrary effects on the world.
   Actions are triggered by controls. Several controls may trigger the
   same action at the same time. Each action runs at most once at a given
   time step.
*)

type t = {
  id: U_actionid.t;
  func: unit -> unit;
}

let create id func =
  { id; func }

let create_set () =
  U_set.create (fun x -> x.id)

let add actionid func set =
  U_set.add set (create actionid func)

let get set id =
  match U_set.get set id with
  | None -> failwith ("Invalid action ID " ^ U_actionid.to_string id)
  | Some x -> x
