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
  id: U_controlid.t;
  actionid: U_actionid.t;
  contributions: contribution array;
    (* one contribution per age, starting from age 0 *)
}

let create_contribution moving_avg_cst =
  {
    last = 0.;
    variance =
      Moving_variance.init
        ~r_avg:moving_avg_cst
        ~r_var:moving_avg_cst
        ()
  }

let create ~moving_avg_cst ~window_length ~id ~actionid =
  let contributions =
    Array.init window_length
      (fun i -> create_contribution moving_avg_cst)
  in
  {
    id;
    actionid;
    contributions;
  }

let create_set () =
  U_set.create (fun x -> x.id)

let add ~moving_avg_cst ~window_length ~id ~actionid set =
  let control = create ~moving_avg_cst ~window_length ~id ~actionid in
  U_set.add set control

let get set id =
  match U_set.get set id with
  | None -> failwith ("Invalid control ID " ^ U_controlid.to_string id)
  | Some x -> x

(*
   Get the weight used to correct the contribution based on the difference
   between observed and predicted value of the goal function.

   This weight is the recent standard deviation, as estimated by
   an exponential moving average. The wider a contribution fluctuates,
   the greater correction it will receive,
   relative to the other contributions.
*)
let get_weight (x : contribution) =
  let mv = x.variance in
  let n = Moving_variance.get_count mv in
  if n < 5 then
    (* too few samples for a reliable estimate *)
    infinity
  else
    let variance = Moving_variance.get (x.variance) in
    sqrt variance

let update_contrib (x : contribution) v =
  assert (v >= 0.);
  Moving_variance.update x.variance v;
  x.last <- v
