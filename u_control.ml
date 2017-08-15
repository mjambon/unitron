(*
   Library for managing controls.
   A control is a named virtual button, permanently linked to one
   action. Several controls may be linked to the same action.
   When a control is activated, the linked action will run during the
   same time step. If several controls trigger the same action during the
   same time step, the action will run just once.
*)

open Printf

type contribution = {
  mutable last: float;
  variance: Mv_var.state;
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
    last = nan;
    variance =
      Mv_var.init
        ~alpha_avg:moving_avg_cst
        ~alpha_var:moving_avg_cst
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
  let n = Mv_var.get_count mv in
  if n <= 1 then
    (* Assign an infinite weight, which is usable, unlike a NaN. *)
    infinity
  else
    (* Initially, the estimate of the standard deviation is very coarse.
       Not sure if or how it should be tweaked for better results. *)
    Mv_var.get_stdev (x.variance)

let update_contrib (x : contribution) v =
  assert (v = v);
  Mv_var.update x.variance v;
  x.last <- v

let get_contribution x age =
  x.contributions.(age)

let get_average contrib =
  Mv_var.get_average contrib.variance

let get_contrib_value contrib =
  U_float.default ~if_nan:0. (get_average contrib)

let get_stdev contrib =
  Mv_var.get_stdev contrib.variance

let get_contribution_average x age =
  get_average (get_contribution x age)

let iter_contributions x f =
  Array.iteri (fun age x ->
    let v = x.variance in
    let average = Mv_var.get_average v in
    let stdev = Mv_var.get_stdev v in
    f ~age ~average ~stdev
  ) x.contributions

let map_contributions x f =
  Array.mapi (fun age x ->
    let v = x.variance in
    let average = Mv_var.get_average v in
    let stdev = Mv_var.get_stdev v in
    f ~age ~average ~stdev
  ) x.contributions

let info_of_contributions a =
  let strings =
    Array.mapi (fun age x ->
      let v = x.variance in
      let mean = Mv_var.get_average v in
      let stdev = Mv_var.get_stdev v in
      sprintf "%i:(%.2g, %.2g)"
        age
        mean stdev
    ) a
  in
  String.concat " " (Array.to_list strings)

let to_info x =
  sprintf "control %s: [%s]"
    (U_controlid.to_string x.id)
    (info_of_contributions x.contributions)

let open_csv window_length fname =
  let header =
    let a =
      Array.init window_length
        (fun age ->
           sprintf "contrib[%i]" age
        )
    in
    String.concat "," (Array.to_list a)
  in
  let oc = open_out fname in
  fprintf oc "%s\n" header;
  oc

let print_csv oc x =
  let a =
    Array.mapi (fun age x ->
      sprintf "%g"
        (Mv_var.get_average x.variance)
    ) x.contributions
  in
  fprintf oc "%s\n"
    (String.concat "," (Array.to_list a))
