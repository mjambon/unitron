(*
   Library for managing actions.
   An action is a named function that has arbitrary effects on the world.
   Actions are triggered by controls. Several controls may trigger the
   same action at the same time. Each action runs at most once at a given
   time step.
*)

type t = {
  actionid: U_actionid.t;
  func: unit -> unit;
}
