(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

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
