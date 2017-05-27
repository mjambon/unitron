(*
   Library for managing controls.
   A control is a named virtual button, permanently linked to one
   action. Several controls may be linked to the same action.
   When a control is activated, the linked action will run during the
   same time step. If several controls trigger the same action during the
   same time step, the action will run just once.
*)

type contribution = {
  mutable last: float;
  variance: Moving_variance.state;
}

type t = {
  controlid: U_controlid.t;
  action: U_actionid.t;
  contributions: contribution array;
    (* one contribution per age, starting from age 0 *)
}
