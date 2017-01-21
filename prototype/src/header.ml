
(* FIXME: misnomer *)

type t = {
  header_candidates : int;
  header_seats : int;
}

let create candidates seats =
  assert (candidates > 0);
  assert (seats > 0);
  assert (candidates > seats);
  {
    header_candidates = candidates;
    header_seats = seats
  }

let get_totals h =
  (h.header_candidates, h.header_seats)
