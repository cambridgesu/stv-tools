
exception Invalid_contest

type t = {
  header_candidates : int;
  header_seats : int;
}

let create candidates seats =
  if (candidates > 0) && (seats > 0) && (candidates > seats)
  then {
    header_candidates = candidates;
    header_seats = seats
  }
  else raise Invalid_contest

let get_totals h =
  (h.header_candidates, h.header_seats)
