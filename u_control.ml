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
    (* object that includes exponential moving average
       and exponential moving variance *)
}

type t = {
  controlid: U_controlid.t;
  action: U_actionid.t;
  contributions: contribution array;
    (* one contribution per age, starting from age 0 *)
}

(*
   Get the weight used to correct the contribution based on the difference
   between observed and predicted value of the goal function.

   This weight is the recent standard deviation, as estimated by
   an exponential moving average. The wider a contribution fluctuates,
   the greater correction it will receive,
   relative to the other contributions.
*)
let get_weight (x : contribution) =
  sqrt (Moving_variance.get (x.variance))

let update_contrib (x : contribution) v =
  Moving_variance.update x.variance v;
  x.last <- v
