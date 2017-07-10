(*
   Informational metrics, used to produced stats.
*)
type t = {
  goal: float;
  pos_contrib: float;
  neg_contrib: float;
  pos_contrib_count: int;
  neg_contrib_count: int;
}

let dummy = {
  goal = nan;
  pos_contrib = 0.;
  neg_contrib = 0.;
  pos_contrib_count = 0;
  neg_contrib_count = 0;
}
