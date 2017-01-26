(*
  STV Tools, a reference implementation of STV, by Martin Keegan

  Copyright (C) 2016-2017  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the GNU Affero General Public Licence as published by
  the Free Software Foundation, either version 3 of said Licence, or
  (at your option) any later version.
*)

open Ballot

type t = {
  total_candidates : int;
  total_seats : int;
  candidates : Candidate.t list;
  ballots : Ballot.t list;
}

exception Too_many_preferences of Ballot.t

let check_ballot_size max_size ballot =
  if max_size < (Ballot.total_preferences ballot)
  then raise (Too_many_preferences ballot)
  else ()

let check_consistency total_candidates seats ballots candidates =
  let check_size = check_ballot_size total_candidates in
    if total_candidates <= seats
    then failwith "Enough seats for all candidates; no need for election"
    else if List.length candidates <> total_candidates
    then failwith "Number of candidates doesn't match names"
    else ballots |> List.iter check_size

let create contest ballot_infos (candidates : Candidate.t list) =
  let total_candidates, seats = Contest.get_totals contest in
  let ballots =
    ballot_infos |> 
    List.map (fun (weight, prefs) -> Ballot.create contest weight prefs) in
    check_consistency total_candidates seats ballots candidates;
    {
      total_candidates = total_candidates;
      total_seats = seats;
      candidates = candidates;
      ballots = ballots;
    }

let dump tally =
  Printf.printf "Seats: %d; Candidates: %d\n\nCandidate names:\n"
    tally.total_seats tally.total_candidates;

  List.iteri (fun i c -> let name = Candidate.name c in
                           Printf.printf " %d. %s\n" (i + 1) name)
    tally.candidates;

  let names = List.map Candidate.name tally.candidates in
    List.iter (fun b -> Ballot.dump_named names b) tally.ballots
